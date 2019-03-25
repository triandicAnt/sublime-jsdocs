"""
DocBlockr v2.14.1
by Nick Fisher, and all the great people listed in CONTRIBUTORS.md
https://github.com/spadgos/sublime-jsdocs

*** Please read CONTIBUTING.md before sending pull requests. Thanks! ***

"""
import sublime
import sublime_plugin
import re
import datetime
import time
import imp
from functools import reduce
apex = 'Packages/MavensMate/sublime/lang/Apex.sublime-syntax'
java = 'Packages/Java/Java.sublime-syntax'

def get_symbols(view):
    ''' custom implementation to override `view.get_symbols()`
        If the current view is an Apex file, then temporarily convert the syntax to Java to obtain Java's symbols.
        Then switch the syntax back to Apex.
    '''
    all_methods = {}
    if view.settings().get('syntax') == apex:
        view.set_syntax_file(java) # workaround for Apex to use Java's tmpreferences
        c = view.substr(sublime.Region(0, view.size()))
        for l in view.find_by_selector('meta.method.identifier.java'):
            val = c[l.a: l.b].strip()
            all_methods[val] = l
    view.set_syntax_file(apex) # set syntax back to Apex
    return all_methods


def read_line(view, point):
    if (point >= view.size()):
        return

    next_line = view.line(point)
    return view.substr(next_line)


def write(view, str):
    view.run_command(
        'insert_snippet', {
            'contents': str
        }
    )

def counter():
    count = 0
    while True:
        count += 1
        yield(count)


def escape(str):
    return str.replace('$', '\$').replace('{', '\{').replace('}', '\}')


def is_numeric(val):
    try:
        float(val)
        return True
    except ValueError:
        return False


def getParser(view):
    scope = view.scope_name(view.sel()[0].end())
    res = re.search('\\bsource\\.([a-z+\-]+)', scope)
    sourceLang = res.group(1) if res else 'js'
    viewSettings = view.settings()

    if sourceLang == "php":
        return JsdocsPHP(viewSettings)
    elif sourceLang == "coffee":
        return JsdocsCoffee(viewSettings)
    elif sourceLang == "actionscript" or sourceLang == 'haxe':
        return JsdocsActionscript(viewSettings)
    elif sourceLang == "c++" or sourceLang == 'c' or sourceLang == 'cuda-c++':
        return JsdocsCPP(viewSettings)
    elif sourceLang == 'objc' or sourceLang == 'objc++':
        return JsdocsObjC(viewSettings)
    elif sourceLang == 'java' or sourceLang == 'groovy' or sourceLang == 'apex':
        return JsdocsJava(viewSettings)
    elif sourceLang == 'rust':
        return JsdocsRust(viewSettings)
    elif sourceLang == 'ts':
        return JsdocsTypescript(viewSettings)
    return JsdocsJavascript(viewSettings)


def splitByCommas(str):
    """
    Split a string by unenclosed commas: that is, commas which are not inside of quotes or brackets.
    splitByCommas('foo, bar(baz, quux), fwip = "hey, hi"')
     ==> ['foo', 'bar(baz, quux)', 'fwip = "hey, hi"']
    """
    out = []

    if not str:
        return out

    # the current token
    current = ''

    # characters which open a section inside which commas are not separators between different arguments
    openQuotes = '"\'<({'
    # characters which close the section. The position of the character here should match the opening
    # indicator in `openQuotes`
    closeQuotes = '"\'>)}'

    matchingQuote = ''
    insideQuotes = False
    nextIsLiteral = False

    for char in str:
        if nextIsLiteral:  # previous char was a \
            current += char
            nextIsLiteral = False
        elif insideQuotes:
            if char == '\\':
                nextIsLiteral = True
            else:
                current += char
                if char == matchingQuote:
                    insideQuotes = False
        else:
            if char == ',':
                out.append(current.strip())
                current = ''
            else:
                current += char
                quoteIndex = openQuotes.find(char)
                if quoteIndex > -1:
                    matchingQuote = closeQuotes[quoteIndex]
                    insideQuotes = True

    out.append(current.strip())
    return out


def flatten(theList):
    """
    Flatten a shallow list. Only works when all items are lists.
    [[(1,1)], [(2,2), (3, 3)]] --> [(1,1), (2,2), (3,3)]
    """
    return [item for sublist in theList for item in sublist]

def getDocBlockRegion(view, point):
    """
    Given a starting point inside a DocBlock, return a Region which encompasses the entire block.
    This is similar to `run_command('expand_selection', { to: 'scope' })`, however it is resilient to bugs which occur
    due to language files adding scopes inside the DocBlock (eg: to highlight tags)
    """
    start = end = point
    while start > 0 and view.scope_name(start - 1).find('comment.block') > -1:
        start = start - 1

    while end < view.size() and view.scope_name(end).find('comment.block') > -1:
        end = end + 1

    return sublime.Region(start, end)

class GenerateCommentsCommand(sublime_plugin.TextCommand):
    def run(self, edit, inline=False):
        size_dict = 0
        visited_methods = set()
        while size_dict != len(get_symbols(self.view.window().active_view())):
            syms = get_symbols(self.view.window().active_view())
            for k,v in syms.items():
                if k not in visited_methods:
                    visited_methods.add(k)
                    point = self.view.full_line(v.a).a - 1
                    if self.view.substr(self.view.full_line(point)).strip() != '*/':
                        JsdocsCommand(self.view).run(edit,self.view, point)
                    break
            size_dict = size_dict + 1


class JsdocsCommand(sublime_plugin.TextCommand):

    def run(self, edit, cmd_view, cmd_point=None, inline=False):
        self.initialize(self.view, cmd_point, inline)

        if self.parser.isExistingComment(self.line):
            write(self.view, "\n *" + self.indentSpaces)
            return

        # erase characters in the view (will be added to the output later)
        self.view.erase(edit, self.trailingRgn)

        # match against a function declaration.
        out = self.parser.parse(self.line)

        snippet = self.generateSnippet(out, inline)
        if not cmd_point:
            write(self.view, snippet)
        else:
            modified_snippet = re.sub(r"\b\d+\b", "", snippet)
            modified_snippet = modified_snippet.replace('${', '').replace(':', '').replace('}', '')
            if not snippet.strip():
                return
            padding = ' ' * (len(self.line) - len(self.line.lstrip()))
            final_comment = '\n' + padding + '/**\n'
            for l in modified_snippet.split('\n'):
                if l.rstrip():
                    final_comment += padding + l.rstrip() + '\n'
            final_comment = final_comment.rstrip('\n')
            # print(final_comment)
            self.write_to_point(cmd_view, edit, cmd_point, final_comment)
            # print(snippet)
            # print(self.line)
            #
    def write_to_point(self, view, edit, point, string):
        view.insert(edit, point, string)


    def initialize(self, v, cmd_point=None, inline=False):
        point = cmd_point if cmd_point else v.sel()[0].end()

        self.settings = v.settings()

        # trailing characters are put inside the body of the comment
        self.trailingRgn = sublime.Region(point, v.line(point).end())
        self.trailingString = v.substr(self.trailingRgn).strip()
        # drop trailing '*/'
        self.trailingString = escape(re.sub('\\s*\\*\\/\\s*$', '', self.trailingString))

        self.indentSpaces = " " * max(0, self.settings.get("jsdocs_indentation_spaces", 1))
        self.prefix = "*"

        settingsAlignTags = self.settings.get("jsdocs_align_tags", 'deep')
        self.deepAlignTags = settingsAlignTags == 'deep'
        self.shallowAlignTags = settingsAlignTags in ('shallow', True)

        self.parser = parser = getParser(v)
        parser.inline = inline

        # use trailing string as a description of the function
        if self.trailingString:
            parser.setNameOverride(self.trailingString)

        # read the next line
        self.line = parser.getDefinition(v, v.line(point).end() + 1)

    def generateSnippet(self, out, inline=False):
        # substitute any variables in the tags

        if out:
            out = self.substituteVariables(out)

        # align the tags
        if out and (self.shallowAlignTags or self.deepAlignTags) and not inline:
            out = self.alignTags(out)

        # fix all the tab stops so they're consecutive
        if out:
            out = self.fixTabStops(out)

        if inline:
            if out:
                return " " + out[0] + " */"
            else:
                return " $0 */"
        else:
            return self.createSnippet(out) + ('\n' if self.settings.get('jsdocs_newline_after_block') else '')

    def alignTags(self, out):
        def outputWidth(str):
            # get the length of a string, after it is output as a snippet,
            # "${1:foo}" --> 3
            return len(re.sub("[$][{]\\d+:([^}]+)[}]", "\\1", str).replace('\$', '$'))

        # count how many columns we have
        maxCols = 0
        # this is a 2d list of the widths per column per line
        widths = []

        # Grab the return tag if required.
        if self.settings.get('jsdocs_per_section_indent'):
            returnTag = self.settings.get('jsdocs_return_tag') or '@return'
        else:
            returnTag = False

        for line in out:
            if line.startswith('@'):
                # Ignore the return tag if we're doing per-section indenting.
                if returnTag and line.startswith(returnTag):
                    continue
                # ignore all the words after `@author`
                columns = line.split(" ") if not line.startswith('@author') else ['@author']
                widths.append(list(map(outputWidth, columns)))
                maxCols = max(maxCols, len(widths[-1]))

        #  initialise a list to 0
        maxWidths = [0] * maxCols

        if (self.shallowAlignTags):
            maxCols = 1

        for i in range(0, maxCols):
            for width in widths:
                if (i < len(width)):
                    maxWidths[i] = max(maxWidths[i], width[i])

        # Convert to a dict so we can use .get()
        maxWidths = dict(enumerate(maxWidths))

        # Minimum spaces between line columns
        minColSpaces = self.settings.get('jsdocs_min_spaces_between_columns', 1)

        for index, line in enumerate(out):
            # format the spacing of columns, but ignore the author tag. (See #197)
            if line.startswith('@') and not line.startswith('@author'):
                newOut = []
                for partIndex, part in enumerate(line.split(" ")):
                    newOut.append(part)
                    newOut.append(" " * minColSpaces + (" " * (maxWidths.get(partIndex, 0) - outputWidth(part))))
                out[index] = "".join(newOut).strip()

        return out

    def substituteVariables(self, out):
        def getVar(match):
            varName = match.group(1)
            if varName == 'datetime':
                date = datetime.datetime.now().replace(microsecond=0)
                offset = time.timezone / -3600.0
                return "%s%s%02d%02d" % (
                    date.isoformat(),
                    '+' if offset >= 0 else "-",
                    abs(offset),
                    (offset % 1) * 60
                )
            elif varName == 'date':
                return datetime.date.today().isoformat()
            else:
                return match.group(0)

        def subLine(line):
            return re.sub(r'\{\{([^}]+)\}\}', getVar, line)

        return list(map(subLine, out))

    def fixTabStops(self, out):
        tabIndex = counter()

        def swapTabs(m):
            return "%s%d%s" % (m.group(1), next(tabIndex), m.group(2))

        for index, outputLine in enumerate(out):
            out[index] = re.sub("(\\$\\{)\\d+(:[^}]+\\})", swapTabs, outputLine)

        return out

    def createSnippet(self, out):
        snippet = ""
        closer = self.parser.settings['commentCloser']
        if out:
            if self.settings.get('jsdocs_spacer_between_sections') == True:
                lastTag = None
                for idx, line in enumerate(out):
                    res = re.match("^\\s*@([a-zA-Z]+)", line)
                    if res and (lastTag != res.group(1)):
                        if self.settings.get('jsdocs_function_description') == False:
                            if lastTag != None:
                                out.insert(idx, "")
                        else:
                            out.insert(idx, "")
                        lastTag = res.group(1)
            elif self.settings.get('jsdocs_spacer_between_sections') == 'after_description' and self.settings.get('jsdocs_function_description'):
                lastLineIsTag = False
                for idx, line in enumerate(out):
                    res = re.match("^\\s*@([a-zA-Z]+)", line)
                    if res:
                        if not lastLineIsTag:
                            out.insert(idx, "")
                        lastLineIsTag = True
            for line in out:
                snippet += "\n " + self.prefix + (self.indentSpaces + line if line else "")
        else:
            snippet += "\n " + self.prefix + self.indentSpaces + "${0:" + self.trailingString + '}'

        snippet += "\n" + closer
        return snippet


class JsdocsParser(object):

    def __init__(self, viewSettings):
        self.viewSettings = viewSettings
        self.setupSettings()
        self.nameOverride = None

    def isExistingComment(self, line):
        return re.search('^\\s*\\*', line)

    def setNameOverride(self, name):
        """ overrides the description of the function - used instead of parsed description """
        self.nameOverride = name

    def getNameOverride(self):
        return self.nameOverride

    def parse(self, line):
        if self.viewSettings.get('jsdocs_simple_mode'):
            return None

        try:
            out = self.parseFunction(line)  # (name, args, retval, options)
            if (out):
                return self.formatFunction(*out)
            out = self.x(line)
            if out:
                return self.formatVar(*out)
        except:
            # TODO show exception if dev\debug mode
            return None

        return None

    def formatVar(self, name, val, valType=None):
        out = []
        if not valType:
            if not val or val == '':  # quick short circuit
                valType = "[type]"
            else:
                valType = self.guessTypeFromValue(val) or self.guessTypeFromName(name) or "[type]"
        if self.inline:
            out.append("@%s %s${1:%s}%s ${1:[description]}" % (
                self.settings['typeTag'],
                "{" if self.settings['curlyTypes'] else "",
                valType,
                "}" if self.settings['curlyTypes'] else ""
            ))
        else:
            out.append("${1:[%s description]}" % (escape(name)))
            out.append("@%s %s${1:%s}%s" % (
                self.settings['typeTag'],
                "{" if self.settings['curlyTypes'] else "",
                valType,
                "}" if self.settings['curlyTypes'] else ""
            ))

        return out

    def getTypeInfo(self, argType, argName):
        typeInfo = ''
        if self.settings['typeInfo']:
            typeInfo = '%s${1:%s}%s ' % (
                "{" if self.settings['curlyTypes'] else "",
                escape(argType or self.guessTypeFromName(argName) or "[type]"),
                "}" if self.settings['curlyTypes'] else "",
            )

        return typeInfo

    def formatFunction(self, name, args, retval, options={}):
        out = []
        if 'as_setter' in options:
            out.append('@private')
            return out

        extraTagAfter = self.viewSettings.get("jsdocs_extra_tags_go_after") or False

        description = self.getNameOverride() or ('@description [%s%sdescription]' % (escape(name), ' ' if name else ''))
        if self.viewSettings.get('jsdocs_function_description'):
            out.append("${1:%s}" % description)

        if (self.viewSettings.get("jsdocs_autoadd_method_tag") is True):
            out.append("@%s %s" % (
                "method",
                escape(name)
            ))

        if not extraTagAfter:
            self.addExtraTags(out)

        # if there are arguments, add a @param for each
        if (args):
            # remove comments inside the argument list.
            args = re.sub(r'/\*.*?\*/', '', args)
            for argType, argName in self.parseArgs(args):
                typeInfo = self.getTypeInfo(argType, argName)

                format_str = "@param %s%s"
                if (self.viewSettings.get('jsdocs_param_description')):
                    format_str += " ${1:[description]}"

                out.append(format_str % (
                    typeInfo,
                    escape(argName) if self.viewSettings.get('jsdocs_param_name') else ''
                ))

        # return value type might be already available in some languages but
        # even then ask language specific parser if it wants it listed
        retType = self.getFunctionReturnType(name, retval)
        if retType is not None:
            typeInfo = ''
            if self.settings['typeInfo']:
                typeInfo = ' %s${1:%s}%s' % (
                    "{" if self.settings['curlyTypes'] else "",
                    retType or "[type]",
                    "}" if self.settings['curlyTypes'] else ""
                )
            format_args = [
                self.viewSettings.get('jsdocs_return_tag') or '@return',
                typeInfo
            ]

            if (self.viewSettings.get('jsdocs_return_description')):
                format_str = "%s%s %s${1:[description]}"
                third_arg = ""

                # the extra space here is so that the description will align with the param description
                if args and self.viewSettings.get('jsdocs_align_tags') == 'deep':
                    if not self.viewSettings.get('jsdocs_per_section_indent'):
                        third_arg = " "

                format_args.append(third_arg)
            else:
                format_str = "%s%s"

            out.append(format_str % tuple(format_args))

        for notation in self.getMatchingNotations(name):
            if 'tags' in notation:
                out.extend(notation['tags'])

        if extraTagAfter:
            self.addExtraTags(out)

        return out

    def getFunctionReturnType(self, name, retval):
        """ returns None for no return type. False meaning unknown, or a string """

        if re.match("[A-Z]", name):
            # no return, but should add a class
            return None

        if re.match('[$_]?(?:set|add)($|[A-Z_])', name):
            # setter/mutator, no return
            return None

        if re.match('[$_]?(?:is|has)($|[A-Z_])', name):  # functions starting with 'is' or 'has'
            return self.settings['bool']

        return self.guessTypeFromName(name) or False

    def parseArgs(self, args):
        """
        a list of tuples, the first being the best guess at the type, the second being the name
        """
        blocks = splitByCommas(args)
        out = []
        for arg in blocks:
            out.append(self.getArgInfo(arg))

        return flatten(out)

    def getArgInfo(self, arg):
        """
        Return a list of tuples, one for each argument derived from the arg param.
        """
        return [(self.getArgType(arg), self.getArgName(arg))]

    def getArgType(self, arg):
        return None

    def getArgName(self, arg):
        return arg

    def addExtraTags(self, out):
        extraTags = self.viewSettings.get('jsdocs_extra_tags', [])
        if (len(extraTags) > 0):
            out.extend(extraTags)

    def guessTypeFromName(self, name):
        matches = self.getMatchingNotations(name)
        if len(matches):
            rule = matches[0]
            if ('type' in rule):
                return self.settings[rule['type']] if rule['type'] in self.settings else rule['type']

        if (re.match("(?:is|has)[A-Z_]", name)):
            return self.settings['bool']

        if (re.match("^(?:cb|callback|done|next|fn)$", name)):
            return self.settings['function']

        return False

    def getMatchingNotations(self, name):
        def checkMatch(rule):
            if 'prefix' in rule:
                regex = re.escape(rule['prefix'])
                if re.match('.*[a-z]', rule['prefix']):
                    regex += '(?:[A-Z_]|$)'
                return re.match(regex, name)
            elif 'regex' in rule:
                return re.search(rule['regex'], name)

        return list(filter(checkMatch, self.viewSettings.get('jsdocs_notation_map', [])))

    def getDefinition(self, view, pos):
        """
        get a relevant definition starting at the given point
        returns string
        """
        maxLines = 25  # don't go further than this
        openBrackets = 0

        definition = ''

        # count the number of open parentheses
        def countBrackets(total, bracket):
            return total + (1 if bracket == '(' else -1)

        for i in range(0, maxLines):
            line = read_line(view, pos)
            if line is None:
                break

            pos += len(line) + 1
            # strip comments
            line = re.sub(r"//.*",     "", line)
            line = re.sub(r"/\*.*\*/", "", line)

            searchForBrackets = line

            # on the first line, only start looking from *after* the actual function starts. This is
            # needed for cases like this:
            # (function (foo, bar) { ... })
            if definition == '':
                opener = re.search(self.settings['fnOpener'], line) if self.settings['fnOpener'] else False
                if opener:
                    # ignore everything before the function opener
                    searchForBrackets = line[opener.start():]

            openBrackets = reduce(countBrackets, re.findall('[()]', searchForBrackets), openBrackets)

            definition += line
            if openBrackets == 0:
                break
        return definition


class JsdocsJava(JsdocsParser):
    def setupSettings(self):
        identifier = '[a-zA-Z_$][a-zA-Z_$0-9]*'
        self.settings = {
            "curlyTypes": False,
            'typeInfo': False,
            "typeTag": "type",
            "varIdentifier": identifier,
            "fnIdentifier":  identifier,
            "fnOpener": identifier + '(?:\\s+' + identifier + ')?\\s*\\(',
            "commentCloser": " */",
            "bool": "Boolean",
            "function": "Function"
        }

    def parseFunction(self, line):
        line = line.strip()
        res = re.search(
            # Modifiers
            r'(?:(public|protected|private|static|abstract|final|transient|synchronized|native|strictfp)\s+)*'
            # Return value
            + r'(?P<retval>[a-zA-Z_$][<>., a-zA-Z_$0-9]+(\[\])*)\s+'
            # Method name
            + r'(?P<name>' + self.settings['fnIdentifier'] + r')\s*'
            # Params
            + r'\((?P<args>.*)\)\s*'
            # # Throws ,
            + r'(?:throws){0,1}\s*(?P<throws>[a-zA-Z_$0-9\.,\s]*)',
            line
        )

        if not res:
            return None
        group_dict = res.groupdict()
        name = group_dict["name"]
        retval = group_dict["retval"]
        full_args = group_dict["args"]
        throws = group_dict["throws"] or ""

        arg_list = []
        for arg in splitByCommas(full_args):
            arg_list.append(arg.strip().split(" ")[-1])
        args = ",".join(arg_list)

        throws_list = []
        for arg in splitByCommas(throws):
            throws_list.append(arg.strip().split(" ")[-1])
        throws = ",".join(throws_list)

        return (name, args, retval, throws)

    def parseVar(self, line):
        return None

    def guessTypeFromValue(self, val):
        return None

    def formatFunction(self, name, args, retval, throws_args, options={}):
        out = JsdocsParser.formatFunction(self, name, args, retval, options)

        if throws_args != "":
            for unused, exceptionName in self.parseArgs(throws_args):
                typeInfo = self.getTypeInfo(unused, exceptionName)
                out.append("@throws %s%s ${1:[description]}" % (
                    typeInfo,
                    escape(exceptionName)
                ))

        return out

    def getFunctionReturnType(self, name, retval):
        if retval == "void":
            return None
        return retval

    def getDefinition(self, view, pos):
        maxLines = 25  # don't go further than this

        definition = ''
        open_curly_annotation = False
        open_paren_annotation = False
        for i in range(0, maxLines):
            line = read_line(view, pos)
            if line is None:
                break

            pos += len(line) + 1
            # Move past empty lines
            if re.search("^\s*$", line):
                continue
            # strip comments
            line = re.sub("//.*", "", line)
            line = re.sub(r"/\*.*\*/", "", line)
            if definition == '':
                # Must check here for function opener on same line as annotation
                if self.settings['fnOpener'] and re.search(self.settings['fnOpener'], line):
                    pass
                # Handle Annotations
                elif re.search("^\s*@", line):
                    if re.search("{", line) and not re.search("}", line):
                        open_curly_annotation = True
                    if re.search("\(", line) and not re.search("\)", line):
                        open_paren_annotation = True
                    continue
                elif open_curly_annotation:
                    if re.search("}", line):
                        open_curly_annotation = False
                    continue
                elif open_paren_annotation:
                    if re.search("\)", line):
                        open_paren_annotation = False
                elif re.search("^\s*$", line):
                    continue
                # Check for function
                elif not self.settings['fnOpener'] or not re.search(self.settings['fnOpener'], line):
                    definition = line
                    break
            definition += line
            if line.find(';') > -1 or line.find('{') > -1:
                definition = re.sub(r'\s*[;{]\s*$', '', definition)
                break
        return definition

############################################################33


class JsdocsIndentCommand(sublime_plugin.TextCommand):

    def run(self, edit):
        v = self.view
        currPos = v.sel()[0].begin()
        currLineRegion = v.line(currPos)
        currCol = currPos - currLineRegion.begin()  # which column we're currently in
        prevLine = v.substr(v.line(v.line(currPos).begin() - 1))
        spaces = self.getIndentSpaces(prevLine)
        if spaces:
            toStar = len(re.search("^(\\s*\\*)", prevLine).group(1))
            toInsert = spaces - currCol + toStar
            if spaces is None or toInsert <= 0:
                v.run_command(
                    'insert_snippet', {
                        'contents': "\t"
                    }
                )
                return

            v.insert(edit, currPos, " " * toInsert)
        else:
            v.insert(edit, currPos, "\t")

    def getIndentSpaces(self, line):
        hasTypes = getParser(self.view).settings['typeInfo']
        extraIndent = '\\s+\\S+' if hasTypes else ''
        res = re.search("^\\s*\\*(?P<fromStar>\\s*@(?:param|property)%s\\s+\\S+\\s+)\\S" % extraIndent, line) \
           or re.search("^\\s*\\*(?P<fromStar>\\s*@(?:returns?|define)%s\\s+\\S+\\s+)\\S" % extraIndent, line) \
           or re.search("^\\s*\\*(?P<fromStar>\\s*@[a-z]+\\s+)\\S", line) \
           or re.search("^\\s*\\*(?P<fromStar>\\s*)", line)
        if res:
            return len(res.group('fromStar'))
        return None


class JsdocsJoinCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        v = self.view
        for sel in v.sel():
            for lineRegion in reversed(v.lines(sel)):
                v.replace(edit, v.find("[ \\t]*\\n[ \\t]*((?:\\*|//[!/]?|#)[ \\t]*)?", lineRegion.begin()), ' ')


class JsdocsDecorateCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        v = self.view
        re_whitespace = re.compile("^(\\s*)//")
        v.run_command('expand_selection', {'to': 'scope'})
        for sel in v.sel():
            maxLength = 0
            lines = v.lines(sel)
            for lineRegion in lines:
                lineText = v.substr(lineRegion)
                tabCount = lineText.count("\t")
                leadingWS = len(re_whitespace.match(lineText).group(1))
                leadingWS = leadingWS - tabCount
                maxLength = max(maxLength, lineRegion.size())

            lineLength = maxLength - (leadingWS + tabCount)
            leadingWS = tabCount * "\t" + " " * leadingWS
            v.insert(edit, sel.end(), leadingWS + "/" * (lineLength + 3) + "\n")

            for lineRegion in reversed(lines):
                line = v.substr(lineRegion)
                rPadding = 1 + (maxLength - lineRegion.size())
                v.replace(edit, lineRegion, leadingWS + line + (" " * rPadding) + "//")
                # break

            v.insert(edit, sel.begin(), "/" * (lineLength + 3) + "\n")


class JsdocsDeindent(sublime_plugin.TextCommand):
    """
    When pressing enter at the end of a docblock, this takes the cursor back one space.
    /**
     *
     */|   <-- from here
    |      <-- to here
    """
    def run(self, edit):
        v = self.view
        lineRegion = v.line(v.sel()[0])
        line = v.substr(lineRegion)
        v.insert(edit, v.sel()[0].begin(), re.sub("^(\\s*)\\s\\*/.*", "\n\\1", line))


class JsdocsReparse(sublime_plugin.TextCommand):
    """
    Reparse a docblock to make the fields 'active' again, so that pressing tab will jump to the next one
    """
    def run(self, edit):
        tabIndex = counter()

        def tabStop(m):
            return "${%d:%s}" % (next(tabIndex), m.group(1))

        v = self.view
        v.run_command('clear_fields')
        sel = getDocBlockRegion(v, v.sel()[0].begin())

        # escape string, so variables starting with $ won't be removed
        text = escape(v.substr(sel))

        # strip out leading spaces, since inserting a snippet keeps the indentation
        text = re.sub("\\n\\s+\\*", "\n *", text)

        # replace [bracketed] [text] with a tabstop
        text = re.sub("(\\[.+?\\])", tabStop, text)

        v.erase(edit, sel)
        write(v, text)


class JsdocsTrimAutoWhitespace(sublime_plugin.TextCommand):
    """
    Trim the automatic whitespace added when creating a new line in a docblock.
    """
    def run(self, edit):
        v = self.view
        lineRegion = v.line(v.sel()[0])
        line = v.substr(lineRegion)
        spaces = max(0, v.settings().get("jsdocs_indentation_spaces", 1))
        v.replace(edit, lineRegion, re.sub("^(\\s*\\*)\\s*$", "\\1\n\\1" + (" " * spaces), line))


class JsdocsWrapLines(sublime_plugin.TextCommand):
    """
    Reformat description text inside a comment block to wrap at the correct length.
    Wrap column is set by the first ruler (set in Default.sublime-settings), or 80 by default.
    Shortcut Key: alt+q
    """

    def run(self, edit):
        v = self.view
        settings = v.settings()
        rulers = settings.get('rulers')
        tabSize = settings.get('tab_size')

        wrapLength = rulers[0] if (len(rulers) > 0) else 80
        numIndentSpaces = max(0, settings.get("jsdocs_indentation_spaces", 1))
        indentSpaces = " " * numIndentSpaces
        indentSpacesSamePara = " " * max(0, settings.get("jsdocs_indentation_spaces_same_para", numIndentSpaces))
        spacerBetweenSections = settings.get("jsdocs_spacer_between_sections") == True
        spacerBetweenDescriptionAndTags = settings.get("jsdocs_spacer_between_sections") == "after_description"

        dbRegion = getDocBlockRegion(v, v.sel()[0].begin())

        # find the first word
        startPoint = v.find(r"\n\s*\* ", dbRegion.begin()).begin()
        # find the first tag, or the end of the comment
        endPoint = v.find(r"\s*\n\s*\*(/)", dbRegion.begin()).begin()

        # replace the selection with this ^ new selection
        v.sel().clear()
        v.sel().add(sublime.Region(startPoint, endPoint))
        # get the description text
        text = v.substr(v.sel()[0])

        # find the indentation level
        indentation = len(re.sub('\t', ' ' * tabSize, re.search("\n(\\s*\\*)", text).group(1)))
        wrapLength -= indentation - tabSize

        # join all the lines, collapsing "empty" lines
        text = re.sub("\n(\\s*\\*\\s*\n)+", "\n\n", text)

        def wrapPara(para):
            para = re.sub("(\n|^)\\s*\\*\\s*", " ", para)

            # split the paragraph into words
            words = para.strip().split(' ')
            text = '\n'
            line = ' *' + indentSpaces
            lineTagged = False  # indicates if the line contains a doc tag
            paraTagged = False  # indicates if this paragraph contains a doc tag
            lineIsNew = True
            tag = ''

            # join all words to create lines, no longer than wrapLength
            for i, word in enumerate(words):
                if not word and not lineTagged:
                    continue

                if lineIsNew and word[0] == '@':
                    lineTagged = True
                    paraTagged = True
                    tag = word

                if len(line) + len(word) >= wrapLength - 1:
                    # appending the word to the current line would exceed its
                    # length requirements
                    text += line.rstrip() + '\n'
                    line = ' *' + indentSpacesSamePara + word + ' '
                    lineTagged = False
                    lineIsNew = True
                else:
                    line += word + ' '

                lineIsNew = False

            text += line.rstrip()
            return {'text':       text,
                    'lineTagged': lineTagged,
                    'tagged':     paraTagged,
                    'tag':        tag}

        # split the text into paragraphs, where each paragraph is eighter
        # defined by an empty line or the start of a doc parameter
        paragraphs = re.split('\n{2,}|\n\\s*\\*\\s*(?=@)', text)
        wrappedParas = []
        text = ''
        for p, para in enumerate(paragraphs):
            # wrap the lines in the current paragraph
            wrappedParas.append(wrapPara(para))

        # combine all the paragraphs into a single piece of text
        for i in range(0, len(wrappedParas)):
            para = wrappedParas[i]
            last = i == len(wrappedParas) - 1

            nextIsTagged = not last and wrappedParas[i + 1]['tagged']
            nextIsSameTag = nextIsTagged and para['tag'] == wrappedParas[i + 1]['tag']

            if last or (para['lineTagged'] or nextIsTagged) and \
                    not (spacerBetweenSections and not nextIsSameTag) and \
                    not (not para['lineTagged'] and nextIsTagged and spacerBetweenDescriptionAndTags):
                text += para['text']
            else:
                text += para['text'] + '\n *'

        text = escape(text)
        write(v, text)
