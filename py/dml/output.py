# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import os
from .logging import ICE, SimpleSite

__all__ = (
    'NoOutput',
    'FileOutput',
    'StrOutput',
    'out',
    'indent_level',
    'quote_filename',
)


class Output(object):
    outwrite_stack = []

    def __init__(self, indent=0):
        self.indent = indent
        self.lineno = 1
        self.bol = True

    def write(self, s):
        assert False

    def __enter__(self):
        self.outwrite_stack.append(self)

    def __exit__(self, exc_type, exc_val, exc_tb):
        top = self.outwrite_stack.pop(-1)
        assert top is self

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
        self.indent += postindent * indent_level
        self.lineno += output.count('\n')

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
        self.filename = filename
        self.__file = f
        self.write = self.__file.write
        self.lineno = 1

    def tell(self):
        return self.__file.tell()

    def close(self):
        self.__file.close()

    def commit(self):
        if self.indent:
            raise ICE(SimpleSite("%s:0" % self.filename), 'Unbalanced indent')
        try:
            os.remove(self.filename)
        except OSError:
            pass
        os.rename(self.filename+'.tmp', self.filename)

class StrOutput(Output):
    def __init__(self, indent=0):
        super(StrOutput, self).__init__(indent)
        self.buf = ''

    def write(self, s):
        self.buf += s

indent_level = 4

def out(output = '', preindent = 0, postindent = 0):
    Output.outwrite_stack[-1].out(output, preindent, postindent)

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
