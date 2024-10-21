# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# This module handles errors and warnings

__all__ = (
    'report',
    'ICE',

    'show_porting',
    'is_warning_tag',
    'ignore_warning',
    'warning_is_ignored',
    'enable_warning',
    'set_include_tag',
    'ErrorContext',

    'dollar',

    'FileInfo',
    'Site',
    'SimpleSite',
    'DumpableSite',
    'TemplateSite',

    'dbg',
    )

import sys
import os
import bisect
import abc
import contextlib

import dml.globals

show_porting = False

# Signal translation failure
failure = 0

# Stop compilation after this number of errors (0 means inf)
max_errors = 0

# Include warning and error ID's in reports
include_tag = False
def set_include_tag(val):
    global include_tag
    include_tag = val

def is_warning_tag(tag):
    from . import messages
    cls = getattr(messages, tag, None)
    return isinstance(cls, type) and issubclass(cls, DMLWarning)

# A set of ignored warnings
ignored_warnings = {}
def ignore_warning(tag):
    ignored_warnings[tag] = True

def enable_warning(tag):
    ignored_warnings[tag] = False

def warning_is_ignored(tag):
    return dml.globals.ignore_all_warnings or ignored_warnings.get(tag, False)

class ErrorContext(object):
    __slots__ = ('node', 'site')

    @classmethod
    def current(cls):
        return cls.stack[-1]

    stack = [None]
    last_entered = None

    def __init__(self, node, site=None):
        self.node = node
        self.site = site if site else node.site
    def __enter__(self):
        self.stack.append(self)
        ErrorContext.last_entered = self
    def __exit__(self, exc_type, exc_val, exc_tb):
        popped = self.stack.pop()
        assert popped is self
        if not exc_type:
            ErrorContext.last_entered = popped

last_used_error_context = None

def report_error():
    global failure
    failure += 1
    if failure == max_errors:
        exit(2)

def dollar(site):
    return '$' * (site.dml_version() == (1, 2))

# Error messages
#
# There are at least two kinds of error messages, errors and warnings.
# All messages are represented as instances of LogMessage, or one of
# its subclasses.
#
class LogMessage(object):
    # The kind is for example 'error' or 'warning'.
    kind = None

    # Controls which DML refmanual version it is documented in
    # Leave as None to appear in both
    version = None

    # Needed to build DML reference manual with an old 5 version of
    # doc-and-packaging. Otherwise unused; should be removed in 6.
    internal = True

    outfile = sys.stderr

    def __init__(self, site, *msgargs):
        # The site is the place in the source that this log message
        # refers to.
        if not site:
            self.site = None
            self.ctx = None
        elif isinstance(site, Site):
            self.site = site
            self.ctx = ErrorContext.current()
        else:
            # TODO: Figure out why it is sometimes a DMLObject; that's
            # probably wrong. Also, we should break the circular
            # module dependency on ctree by letting ctree classes
            # inherit an abstract class defined in this module
            from . import ctree, objects
            assert isinstance(site, (ctree.Expression, ctree.Statement,
                                     objects.DMLObject))
            self.site = site.site
            self.ctx = getattr(site, 'context', None)

        # The msg is the message to print.
        self.msg = self.fmt % msgargs

    # This is a utility method that prints a message prefixed with a
    # site indicator.  The msg should be a string without line breaks
    def print_site_message(self, site, msg):
        try:
            loc = site.loc() if site else "<unknown>"
            self.outfile.write("%s: %s\n" % (loc, msg))
        except AttributeError:
            #if os.getenv('DMLC_DEBUG'):
            #    import traceback
            #    traceback.print_exc()
            self.outfile.write("error when printing message: %s\n" % msg)
            if os.getenv('DMLC_DEBUG'):
                raise

    def tag(self):
        return self.__class__.__name__

    def preprocess(self):
        '''Call before log when reporting. Return True to actually log
        or False to abort'''
        return True
    
    # This method can be overridden
    def log(self):
        global last_used_error_context
        tag = self.tag()

        if not self.ctx or self.ctx == last_used_error_context:
            ctx = None
        else:
            last_used_error_context = self.ctx
            ctx = self.ctx

        tpl_site = ctx.node.site if ctx else self.site
        # The chain of is statements leading to this message. Pairs
        # (site, name) where name is the template instantiated by the
        # is statement on site. Innermost site first.
        if tpl_site:
            ises = []
            while tpl_site.is_site:
                ises.append((tpl_site.is_site, tpl_site.tpl_name))
                tpl_site = tpl_site.is_site
            for (site, name) in reversed(ises):
                self.print_site_message(site,
                                        "In template %s" % (name,))

        if ctx:
            self.print_site_message(ctx.site,
                                    "In %s" % self.ctx.node.identity())

        lines = self.msg.splitlines() or ['']
        if include_tag:
            tag = ' ' + tag
        else:
            tag = ''
        self.print_site_message(self.site,
                                '%s%s: %s' % (self.kind, tag, lines[0]))
        for l in lines[1:]:
            self.print_site_message(self.site, '  ' + l)

    def postprocess(self):
        pass

# This is a base class for internal compiler errors
#
class ICE(Exception, LogMessage):
    kind = "internal compiler error"
    fmt = "%s"
    def __init__(self, site, msg):
        LogMessage.__init__(self, site, msg)
        Exception.__init__(self, msg)

# This is a base class for warning messages
#
class DMLWarning(LogMessage):
    kind = "warning"
    next_warning_yields_error = False

    def preprocess(self):
        # Don't print anything if the user asked us not to
        if warning_is_ignored(self.tag()):
            return False
        if DMLWarning.next_warning_yields_error:
            self.print_site_message(self.site,
                                    'dmlc: warnings being treated as errors')
        return True

    @classmethod
    def enable_werror(cls):
        cls.next_warning_yields_error = True

    def postprocess(self):
        if DMLWarning.next_warning_yields_error:
            report_error()
            DMLWarning.next_warning_yields_error = False


# This is a base class for error messages
#
class DMLError(Exception, LogMessage):
    kind = "error"

    def __init__(self, site, *msgargs):
        LogMessage.__init__(self, site, *msgargs)
        loc = self.site.loc() if hasattr(site, 'loc') else "<unknown>"
        Exception.__init__(self, "%s: %s" % (loc, self.msg))

    def postprocess(self):
        report_error()

class PortingMessage(LogMessage):
    kind = 'porting'
    fmt = ''

    def __init__(self, site, *porting_args):
        LogMessage.__init__(self, site)
        self.args = porting_args

    def log(self):
        self.print_site_message(
            self.site,
            '%s %s: %r' % (self.kind, self.tag(), [
                # normcase helps port-dml to eliminate duplicate
                # messages on Windows
                os.path.normcase(arg.loc()) if isinstance(arg, Site) else arg
                for arg in self.args]))

class Site(metaclass=abc.ABCMeta):
    __slots__ = ()
    @abc.abstractmethod
    def loc(self): pass
    @abc.abstractmethod
    def filename(self): pass
    @abc.abstractproperty
    def lineno(self): pass
    @abc.abstractmethod
    def dml_version(self): pass
    @abc.abstractmethod
    def provisional_enabled(self, feature): pass
    @abc.abstractmethod
    def bitorder(self): pass
    is_site = None

class SimpleSite(Site):
    '''A variant of Site without line information. Useful for code
    that doesn't come from a real DML file'''
    __slots__ = ('_name', '_dml_version')
    def __init__(self, name, dml_version=(1, 4)):
        self._name = name
        self._dml_version = dml_version
    def __repr__(self):
        return '<site %s>' % (self._name,)
    def loc(self):
        return self._name
    def filename(self):
        return self._name
    def dml_version(self):
        return self._dml_version
    def bitorder(self):
        return 'le'
    def provisional_enabled(self, feature):
        return False
    lineno = 1

def accumulate(iterable):
    '''Like itertools.accumulate from Python 3.2'''
    acc = 0
    for x in iterable:
        acc += x
        yield acc

class FileInfo(object):
    '''Global information about a DML file. Each AST is associated
    with a unique FileInfo instance, which is accessible via all sites
    of the AST.'''
    __slots__ = ('name', 'version', 'bitorder', '_line_offsets',
                 'utf8_columns', 'provisional')
    def __init__(self, name, version, bitorder='le', content_lines=None,
                 provisional=None):
        name = str(name)
        self.name = name
        self.version = version
        self.bitorder = bitorder
        if not content_lines:
            content_lines = ctx = open(name)
        else:
            ctx = contextlib.nullcontext()
        with ctx:
            # line_offset[i] is the zero-based offset of line i+1
            content_lines = list(content_lines)
            self._line_offsets = [0] + list(
                accumulate(len(line) for line in content_lines))
            # Temporary py2 hack for expressing utf-8 based column numbers
            # if line N contains unicode characters, then utf8_column[N][c]
            # is the character index of the c:th byte
            utf8_columns = {}
            self.utf8_columns = utf8_columns
        self.set_name(name)
        self.provisional = provisional or {}

    alias = os.getenv('DMLC_PATHSUBST')

    def set_name(self, name):
        if self.alias:
            # Hack that allows source locations to point to standard
            # lib source instead of copied source; handy while hacking
            # the standard library. Typical usage: set
            # DMLC_PATHSUBST=linux64/bin/dml=src/tools/dmlc/lib.
            (subst_from, subst_to) = self.alias.split('=')
            if name and subst_from in name:
                mangled_name = name.replace(
                    subst_from, subst_to)
                if os.path.exists(mangled_name):
                    name = mangled_name
        self.name = name

    def __getstate__(self):
        return (self.name, self.version, self.bitorder, self._line_offsets,
                self.utf8_columns, {p.tag(): site
                                    for (p, site) in self.provisional.items()})
    def __setstate__(self, data):
        from . import provisional
        (self.name, self.version, self.bitorder, self._line_offsets,
         self.utf8_columns, provisionals) = data
        self.provisional = {provisional.features[tag]: site
                            for (tag, site) in provisionals.items()}
    def loc_from_offset(self, offset):
        '''Calculate a file location as a (line, col) pair'''
        line = bisect.bisect_right(self._line_offsets, offset) - 1
        col = offset - self._line_offsets[line]
        if line in self.utf8_columns:
            col = self.utf8_columns[line].get(col, col)
        return (line + 1, col + 1)
    def size(self):
        return self._line_offsets[-1]

class DumpableSite(Site):
    '''A source code location'''
    __slots__ = ('file_info', '_offs')
    def __init__(self, file_info, fileoffs):
        self.file_info = file_info
        self._offs = fileoffs
    def __repr__(self):
        return '<site %s>' % self.loc()
    def filename(self):
        return self.file_info.name
    @property
    def lineno(self):
        (line, col) = self.file_info.loc_from_offset(self._offs)
        return line
    def loc(self):
        (line, col) = self.file_info.loc_from_offset(self._offs)
        return "%s:%d:%d" % (self.filename(), line, col)
    def dml_version(self):
        return self.file_info.version
    def bitorder(self):
        return self.file_info.bitorder
    # A site is serialized when reading or writing .dmlast files. All
    # DumpableSite objects reachable from one AST must share the same FileInfo
    # object in the 'file' attribute; this is needed because it allows
    # FileInfo.name to be changed after the .dmlast file is loaded.
    def __getstate__(self):
        return (self.file_info, self._offs)
    def __setstate__(self, data):
        (self.file_info, self._offs) = data
    def provisional_enabled(self, feature):
        return self.file_info.provisional.get(feature, False)

class TemplateSite(Site):
    '''A source code location after template expansion'''
    __slots__ = ('site', 'is_site', 'tpl_name')
    def __init__(self, site, is_site, tpl_name):
        self.site = site
        # site of the is statement that yielded this site
        self.is_site = is_site
        # name of the template this statement was expanded from
        self.tpl_name = tpl_name
    def __repr__(self):
        return '<site %s via %s>' % (repr(self.site), repr(self.is_site))
    def dml_version(self): return self.site.dml_version()
    def loc(self): return self.site.loc()
    def filename(self): return self.site.filename()
    def bitorder(self): return self.site.bitorder()
    @property
    def lineno(self): return self.site.lineno
    def provisional_enabled(self, feature):
        return self.site.provisional_enabled(feature)

store_errors = None

def report(logmessage):
    if store_errors is not None and isinstance(logmessage,
                                               (DMLError, DMLWarning)):
        store_errors.append(logmessage)
        return

    if logmessage.preprocess():
        logmessage.log()
        logmessage.postprocess()

def dbg(*args):
    sys.stderr.write("%s\n" % (" ".join(map(str, args))))

@contextlib.contextmanager
def suppress_errors():
    global store_errors
    orig = store_errors
    store_errors = []
    try:
        yield store_errors
    except DMLError as e:
        store_errors.append(e)
    finally:
        store_errors = orig
