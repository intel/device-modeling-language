# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import os
from contextlib import contextmanager
from pathlib import Path

import dml.globals
from .logging import ICE, SimpleSite

__all__ = (
    'NoOutput',
    'FileOutput',
    'StrOutput',
    'out',
    'linemark',
    'indent_level',
    'quote_filename',
    'reset_line_directive',
    'allow_linemarks',
    'disallow_linemarks',
    'site_linemark',
    'coverity_marker',
    'coverity_markers',
)


class Output(object):
    outwrite_stack = []

    filename = None

    def __init__(self, indent=0):
        self.indent = indent
        self.lineno = 1
        self.bol = True
        self.redirected_filename = None
        self.redirected_lineno = None

    def write(self, s):
        assert False

    def __enter__(self):
        self.outwrite_stack.append(self)

    def __exit__(self, exc_type, exc_val, exc_tb):
        top = self.outwrite_stack.pop(-1)
        assert top is self

    def linemark(self, lineno, filename):
        assert lineno is not None and filename is not None
        if (self.redirected_filename != filename
            or self.redirected_lineno != lineno):
            self.redirected_lineno = None
            self.out('#line %d "%s"\n'
                     % (lineno, quote_filename(filename)))
            self.redirected_filename = filename
            self.redirected_lineno = lineno

    def reset_line_directive(self):
        assert self.filename is not None
        if self.redirected_filename is not None:
            self.redirected_filename = None
            self.redirected_lineno = None
            self.out('#line %d "%s"\n'
                     % (self.lineno + 1, quote_filename(self.filename)))

    class IndeterminateFilename: pass

    def indeterminate_line_directive(self):
        self.redirected_filename = Output.IndeterminateFilename
        self.redirected_lineno = None

    def out(self, output, preindent = 0, postindent = 0):
        self.indent += preindent * indent_level
        if output == '\n':
            # Don't indent empty lines...
            self.write('\n')
            self.bol = True
        elif output:
            if isinstance(output, bytes):
                output = output.decode('utf-8')
            if self.bol:
                self.write(' ' * self.indent)
            self.write(output)
            self.bol = (output.endswith('\n'))
        no_lines = output.count('\n')
        self.indent += postindent * indent_level
        self.lineno += no_lines
        if self.redirected_lineno is not None:
            self.redirected_lineno += no_lines

class NoOutput(Output):
    def write(self, s):
        pass
    def out(self, *args, **kwargs):
        pass

class FileOutput(Output):
    def __init__(self, filename):
        super(FileOutput, self).__init__()
        self.set_file(open(filename + ".tmp", "w"), filename)

    def set_file(self, f, filename):
        self.filename = str(Path(filename).resolve())
        self.__file = f
        self.write = self.__file.write
        self.lineno = 1
        self.redirected_filename = None
        self.redirected_lineno = None

    def tell(self):
        return self.__file.tell()

    def close(self):
        self.__file.close()

    def commit(self):
        if self.indent:
            raise ICE(SimpleSite(f"{self.filename}:0"), 'Unbalanced indent')
        try:
            os.remove(self.filename)
        except OSError:
            pass
        os.rename(self.filename+'.tmp', self.filename)

class StrOutput(Output):
    def __init__(self, indent=0, filename=None, lineno=1):
        super(StrOutput, self).__init__(indent)
        self.lineno = lineno
        self.filename = filename
        self.buf = ''

    def write(self, s):
        self.buf += s

indent_level = 4

def out(output = '', preindent = 0, postindent = 0):
    Output.outwrite_stack[-1].out(output, preindent, postindent)

def linemark(lineno, filename):
    Output.outwrite_stack[-1].linemark(lineno, filename)

def reset_line_directive():
    if dml.globals.linemarks_enabled:
        Output.outwrite_stack[-1].reset_line_directive()

def current():
    return Output.outwrite_stack[-1]

# This is a cache for the quote_filename function
quoted_filenames = {}

def quote_filename(filename):
    "Return filename as a properly quoted string"
    fn = quoted_filenames.get(filename, None)
    if not fn:
        p = filename.split('\\')
        fn = '\\\\'.join([s.replace('"', '\\"') for s in p])
        quoted_filenames[filename] = fn
    return fn


def site_linemark_nocoverity(site, adjust=0):
    if dml.globals.linemarks:
        if site is not None and not isinstance(site, SimpleSite):
            filename = site.filename()
            lineno = site.lineno
            if lineno + adjust < 0:
                raise ICE(
                    site,
                    "linemark can't be created for this line!! This should "
                    + "only happen if you disregard proper formatting and  "
                    + "omit a *ridiculous* number of natural linebreaks. "
                    + "If so you probably have no use for linemarks anyway, "
                    + "in which case you can pass '--noline' to DMLC.")
            linemark(lineno + adjust, quote_filename(filename))
        else:
            reset_line_directive()

def coverity_marker(event, classification=None, site=None):
    coverity_markers([(event, classification)], site)

def coverity_markers(markers, site=None):
    site_with_loc = site is not None and not isinstance(site, SimpleSite)
    if dml.globals.coverity and site_with_loc:
        custom_markers = []
        filename = site.filename()
        tgt_lineno = site.lineno

        while (filename, tgt_lineno) in dml.globals.coverity_pragmas:
            (start_lineno,
             inline_markers) = dml.globals.coverity_pragmas[(filename,
                                                             tgt_lineno)]
            custom_markers.extend(reversed(inline_markers))

            # A minor HACK to handle the case of:
            #   /*% COVERITY foo %*/ /*% COVERITY
            #   bar %*/
            #   some_statement
            #
            # Otherwise 'foo' won't be captured
            if (start_lineno + 1 < tgt_lineno
                and ((filename, start_lineno + 1)
                     in dml.globals.coverity_pragmas)):
                tgt_lineno = start_lineno + 1
            else:
                tgt_lineno = start_lineno

        custom_markers.reverse()
        markers = custom_markers + markers

    if dml.globals.coverity and markers:
        if dml.globals.linemarks:
            reset_line_directive()
            if site_with_loc:
                out('#ifndef __COVERITY__\n')
                site_linemark_nocoverity(site, adjust=-(len(markers) + 1))
                out('#endif\n')
                current().indeterminate_line_directive()
        for (event, classification) in markers:
            classification = f' : {classification}' if classification else ''
            out(f'/* coverity[{event}{classification}] */\n')
    else:
        site_linemark_nocoverity(site)

def site_linemark(site):
    coverity_markers([], site)

# Allow linemarks to be generated if dml.globals.linemarks_enabled is True
# and the current output has an associated file name
# This context manager generates a line directive reset when left unless
# linemarks were already allowed
@contextmanager
def allow_linemarks():
    prev_linemarks = dml.globals.linemarks
    curr_output = current()
    is_file_output = curr_output.filename is not None
    dml.globals.linemarks = is_file_output and dml.globals.linemarks_enabled
    assert not prev_linemarks or dml.globals.linemarks
    try:
        yield
    finally:
        assert current() is curr_output
        dml.globals.linemarks = prev_linemarks
        if not prev_linemarks and is_file_output:
            reset_line_directive()

# Locally set dml.globals.linemarks to be False, even if it were already True
# Will also disable the emission of coverity annotations
@contextmanager
def disallow_linemarks():
    prev_linemarks = dml.globals.linemarks
    prev_coverity = dml.globals.coverity
    dml.globals.linemarks = False
    dml.globals.coverity = False
    try:
        yield
    finally:
        dml.globals.linemarks = prev_linemarks
        dml.globals.coverity = prev_coverity
