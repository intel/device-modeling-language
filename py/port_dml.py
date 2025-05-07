# © 2014 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import os
import sys
import argparse
import ast
import abc
import re
import hashlib
import unittest
import tempfile
import itertools
import traceback
from pathlib import Path
import ply.lex
from simicsutils.host import host_type

class PortingFailure(Exception):
    tag = None

def find_lexer(self_path):
    path = os.environ.get('DMLC_DIR')
    if path:
        path = Path(path) / 'dml' / 'python'
    else:
        path = self_path.parent
    if (path / 'dml').is_dir():
        sys.path.append(str(path))
        import dml.dmllex14 as _
        return
    raise Exception(
        f'cannot find dmlc in {path}.'
        + f' Please set DMLC_DIR=<path>/{host_type()}/bin')

def init_lexer(path, line):
    from dml import dmllex14, logging
    lexer = ply.lex.lex(dmllex14)
    lexer.file_info = logging.FileInfo(path, (1, 4), None)
    lexer.filename = path
    lexer.lineno = line
    return lexer

def rspace(s):
    '''return all trailing whitespace characters in s'''
    return s[len(s.rstrip()):]
def lspace(s):
    '''return all leading whitespace characters in s'''
    return s[:len(s)-len(s.lstrip())]

class test_space(unittest.TestCase):
    def test_space(self):
        a = ' \n '
        b = 'xyz'
        c = '\n     \n'
        self.assertEqual(rspace(a + b + c), c)
        self.assertEqual(lspace(a + b + c), a)
        self.assertEqual(lspace(' '), ' ')

def accumulate(iterable):
    '''Like itertools.accumulate from Python 3.2'''
    acc = 0
    for x in iterable:
        acc += x
        yield acc

# _line_offsets[path] is a list where index i contains the
# zero-based offset of line i+1 in file 'path'
_line_offsets = {}
def decode_loc(loc_str):
    '''Given a string "path:line:col", return a pair ("path", offs) where
    offs is a zero-based file offset'''
    (path, line, col) = loc_str.rsplit(':', 2)
    if path not in _line_offsets:
        with open(path, newline='') as f:
            _line_offsets[path] = [0] + list(accumulate(
                len(line) for line in f))
    return (path, _line_offsets[path][int(line) - 1] + int(col) - 1)

ident_re = re.compile('[A-Za-z_][A-Za-z_0-9]*')

class SourceFile(object):
    def __init__(self, path, compat=False):
        self.compat = compat
        self.path = path
        with open(path, 'r', newline='') as f:
            self.contents = f.read()

        # Each translation is a list of tuples (left, right, dest,
        # newlen) where (left,right) is the source interval (offsets,
        # half-open interval), dest is the destination offset where
        # this chunk was moved, and newlen is the length of the chunk
        # after edit.
        self.applied_translations = []

    def read_chunk(self, start, end):
        return self.contents[start:end]

    def read_line(self, offset):
        # This can get wrong if the file doesn't end with newline.
        # I'm not worried.
        return self.read_to_char(offset, '\n')[:-1]

    def read_to_char(self, offset, char):
        return self.contents[offset:self.contents.index(char, offset) + 1]

    def read_line_up_to(self, offset):
        lastindex = offset - self.contents[offset::-1].index('\n')
        return self.contents[lastindex + 1 : offset]

    def read_next_indent(self, offset):
        newline = self.contents.index('\n', offset)
        next_line = self.read_line(newline + 1)
        return '\n' + lspace(next_line)

    def read_regexp(self, offset, regexp):
        rest = self.contents[offset:]
        match = regexp.match(rest)
        if not match:
            return None
        return rest[:match.end()]

    def read_tokens(self, offset, end_offset=None):
        '''Generates pairs (pad, string, kind) of lexer tokens'''
        lexer = init_lexer(self.path, self.contents[:offset].count('\n') + 1)
        lexer.input(self.contents)
        lexer.lexpos = offset
        while end_offset is None or lexer.lexpos <= end_offset:
            prev_lexpos = lexer.lexpos
            t = lexer.token()
            if t is None:
                return
            yield (t.lexpos - prev_lexpos,
                   self.contents[t.lexpos : lexer.lexpos],
                   t.type)

    def skip_tokens(self, offs, n):
        '''return number of characters until the end of the next N tokens'''
        tokens = self.read_tokens(offs)
        return sum(pad + len(text)
                   for (pad, text, _) in itertools.islice(tokens, 0, n))

    def skip_token(self, offs):
        return self.skip_tokens(offs, 1)

    def translate_offs(self, offset):
        for (froml, fromr, to, newlen) in self.applied_translations:
            if froml < to:
                if froml <= offset < fromr:
                    delta = to - fromr
                elif fromr <= offset < to:
                    delta = froml - fromr
                elif offset >= to:
                    delta = newlen + froml - fromr
                else:
                    continue
            else:
                if to <= offset < froml:
                    delta = newlen
                elif froml <= offset < fromr:
                    delta = to - froml
                elif offset >= fromr:
                    delta = newlen + froml - fromr
                else:
                    continue
            offset += delta
        return offset

    def translate_interval(self, start, end):
        # TODO: handle end better, it should sometimes stick to the
        # same interval as start on transformations
        return (self.translate_offs(start), self.translate_offs(end))

    def edit(self, offs, length, newstr):
        '''replace 'length' characters at offs with newstr.

        If length is a string, require that the text matches at offs
        and replace that instead.'''
        self.move(offs, length, offs, newstr)

    def move(self, src_offs, length, dest_offs, newstr):
        '''Remove length characters of the text that started at offset
        src_offs in the original file, and insert newstr in the
        location that started at dest_offs in the original file.

        'length' is either the number of characters, or the string to
        remove (in which case it must match the previous text).
        '''
        if not isinstance(length, int):
            # if caller happens to know the previous string, it may be
            # passed verbatim instead of the length, for
            # readability
            prev = self.contents[src_offs : src_offs + len(length)]
            assert prev == length, 'mismatch: %r != %r' % (prev, length)
            length = len(length)
        # cannot move interval into the middle of itself
        assert not src_offs < dest_offs < src_offs + length
        self.applied_translations.append((src_offs, src_offs + length,
                                          dest_offs, len(newstr)))
        if dest_offs <= src_offs:
            self.contents = (self.contents[:dest_offs]
                             + newstr
                             + self.contents[dest_offs:src_offs]
                             + self.contents[src_offs + length:])
        else:
            self.contents = (self.contents[:src_offs]
                             + self.contents[src_offs + length:dest_offs]
                             + newstr
                             + self.contents[dest_offs:])

    def commit(self, f):
        f.write(self.contents)

class TempFile(object):
    '''Like NamedTemporaryFile(delete=True),
    but fills file with contents and opens it read-only'''
    def __init__(self, contents):
        self.contents = contents
    def __enter__(self):
        f = tempfile.NamedTemporaryFile(delete=False)
        with f:
            f.write(self.contents)
        self.name = f.name
        self.f = open(f.name)
        self.f.__enter__()
        return self
    def __exit__(self, *args):
        try:
            return self.f.__exit__(*args)
        finally:
            os.remove(self.name)

class test_SourceFile(unittest.TestCase):
    def test_move_offs(self):
        s = "0123456789"
        with TempFile(s.encode()) as tf:
            f = SourceFile(tf.name)
            self.assertEqual(f.read_chunk(0, 10), s)
            # Translate '123' into 'abcd' and move it forward
            f.move(1, 3, 8, "abcd")
            # string was moved correctly
            self.assertEqual(f.read_chunk(0, 11), "04567abcd89")
            # when reading chunks, offsets have been translated to new positions
            def trans(i):
                translated = f.translate_offs(i)
                return f.read_chunk(translated, translated + 1)
            self.assertEqual([trans(i) for i in [0, 1, 3, 4, 7, 8]],
                             ["0", "a", "c", "4", "7", "8"])
            # Translate 'b' (formerly '2'), into 'ef', and move it backward
            f.move(6, 1, 2, "ef")
            self.assertEqual(f.read_chunk(0, 12), "04ef567acd89")
            self.assertEqual([trans(i) for i in [0, 1, 2, 3, 4]],
                             ["0", "a", "e", "c", "4"])
            # change 'ef' into 'g'
            f.move(2, 2, 2, "g")
            self.assertEqual(f.read_chunk(0, 11), "04g567acd89")
            self.assertEqual([trans(i) for i in [1, 2, 3, 4, 5]],
                             ["a", "g", "c", "4", "5"])

            # string instead of src length is permitted
            f.move(0, '04', 0, 'xy')
            self.assertEqual(f.read_chunk(0, 4), "xyg5")
            # .. but requires that the string matches previous text
            with self.assertRaises(AssertionError):
                f.move(0, 'xz', 0, 'xy')

    def test_decode_loc(self):
        global _line_offsets
        old = _line_offsets
        try:
            with TempFile(b'a\nbc\nd') as f:
                self.assertEqual(decode_loc(f.name + ":3:2"), (f.name, 6))

            _line_offsets = {'a:b': [0, 3, 5]}
            self.assertEqual(decode_loc('a:b:1:1'), ('a:b', 0))
            self.assertEqual(decode_loc('a:b:2:1'), ('a:b', 3))
            self.assertEqual(decode_loc('a:b:3:3'), ('a:b', 7))
            # source location is incorrect, this causes predictable
            # incorrect result instead of crash
            self.assertEqual(decode_loc('a:b:1:10'), ('a:b', 9))
        finally:
            _line_offsets = old

    def test_read_tokens(self):
        find_lexer(Path(__file__))
        from dml.messages import ESYNTAX

        with TempFile(b"75 /**/ xyz;") as tf:
            f = SourceFile(tf.name)
            self.assertEqual(list(f.read_tokens(1)), [
                (0, '5', 'ICONST'),
                (6, 'xyz', 'ID'),
                (0, ';', 'SEMI')])
        with TempFile(b"\n\n\n\nggg\\") as tf:
            f = SourceFile(tf.name)
            try:
                for _ in f.read_tokens(2):
                    pass
            except ESYNTAX as e:
                self.assertIn('%s:5:4:' % (tf.name,), str(e))
            else:
                self.fail('expected ESYNTAX')

# transformations are applied in phases (lowest first), because
# some transformations are known to be non-commutative.  We still
# try to keep transformation order close to the input order,
# mainly to allow us to do randomized testing of commutativity.
class Transformation(abc.ABC):
    phase = 0
    def __init__(self, loc, params):
        (self._path, _) = decode_loc(loc)
        self.loc = loc
        self.params = params
    def offset(self, f, loc=None):
        '''Calculate translated offset in a given file'''
        if loc is None:
            loc = self.loc
        (path, offs) = decode_loc(loc)
        # all locations in one tag should point to the same file
        assert os.path.normcase(path) == os.path.normcase(self._path)
        return f.translate_offs(offs)

    @abc.abstractmethod
    def apply(self, f): "Apply this transformation on a SourceFile object"

class Replace(Transformation):
    def apply(self, f):
        offset = self.offset(f)
        [exp_before, after] = self.params
        act_before = f.read_chunk(offset, offset + len(exp_before))
        assert act_before == exp_before, act_before
        f.edit(offset, act_before, after)

def replace_const(before, after):
    '''Simple transformation that replaces one hardcoded string with
    another at a given location'''
    def constructor(loc, params):
        [] = params
        return Replace(loc, [before, after])
    return constructor

class PSHA1(Transformation):
    # diagnostic message suggesting that everything is broken; should
    # come first
    phase = -10
    def apply(self, f):
        [expected] = self.params
        with open(f.path, 'rb') as file_obj:
            actual = hashlib.sha1(file_obj.read()).hexdigest()  # nosec
        assert actual == expected, (
            "SHA1 sum mismatch on source DML file:"
            + " sha=%s when generating tagfile;" % (expected,)
            + " sha=%s when invoking port-dml" % (actual,)
            + " Did you modify the DML file before running port-dml?")

class PNOTHROW(Transformation):
    def apply(self, f):
        offset = self.offset(f)
        line = f.read_line(offset)
        assert line.startswith('nothrow')
        # prune any space after 'nothrow' as well
        f.edit(offset, 'nothrow' + lspace(line[7:]), '')

class PINPARAMLIST(Transformation):
    phase = -1 # must happen before PTHROWS
    def apply(self, f):
        offset = self.offset(f)
        line = f.read_line_up_to(offset)
        # Points to first token after method ref. Skip spaces backward.
        offset = offset - len(rspace(line))
        f.edit(offset, 0, '()')

class PSTRUCTDECL(Transformation):
    def apply(self, f):
        [offset, ident_offs, rbrace_offs] = [
            self.offset(f, loc) for loc in [self.loc] + self.params]
        line = f.read_line(ident_offs)
        ident_skip = line.find('{')
        if ident_skip == -1:
            ident_skip = len(line)
        ident = line[:ident_skip].rstrip()
        f.edit(offset, 0, 'typedef ')
        f.move(8 + ident_offs - 1, ident_skip,
               8 + rbrace_offs + 1, ' %s;' % (ident,))

class PTYPEDOUTPARAM(Transformation):
    phase = -1
    def apply(self, f):
        # Include identifier in conversion, to make this
        # transformation commute with moving the parameter around
        [type] = self.params
        offset = self.offset(f)
        ident = f.read_regexp(offset, ident_re)
        f.edit(offset, ident, '%s %s' % (type, ident))

class PINARGTYPE(Transformation):
    # must come after PINLINEDECL
    phase = 1
    def apply(self, f):
        [decl] = self.params
        offset = self.offset(f)
        tokens = f.read_tokens(offset)
        (skip, tok, kind) = next(tokens)
        assert skip == 0
        length = len(tok)
        if kind == 'INLINE':
            (skip, tok, kind) = next(tokens)
            length += skip + len(tok)
        f.edit(offset, length, decl)

class PINVOKE(Transformation):
    # before PEVENT_NO_ARG, PEVENT_UINT64_ARG
    phase = 0
    def apply(self, f):
        [offs, expr_offs, outarg_offs, semi_offs] = [
            self.offset(f, loc)
            for loc in [self.loc] + self.params[:3]]
        [self.num_out] = self.params[3:]
        # Include identifier in conversion, to make this
        # transformation commute with moving the parameter around
        call_len = expr_offs - offs
        # "call x();" -> "x();"
        f.edit(offs, call_len, "")
        if outarg_offs >= semi_offs:
            # no out args to take care of
            return
        [expr_offs, outarg_offs, semi_offs] = [
            o - call_len for o in [expr_offs, outarg_offs, semi_offs]]
        stmt = f.read_chunk(offs, semi_offs)
        # Outarg_offs points to ->, find the (
        outarg_start = outarg_offs - offs + len('->')
        outarg_start = semi_offs - offs - len(stmt[outarg_start:].lstrip())
        outarg_end = semi_offs - offs - len(rspace(stmt))
        if self.num_out == 1:
            # skip parentheses
            outarg_start += 1
            outarg_end -= 1
        expr_end_offs = offs + len(stmt[:outarg_offs - offs].rstrip())
        if self.num_out != 0:
            # Move outargs to left: "f() -> (a, b);" => "(a, b) = f() -> ;"
            f.move(offs + outarg_start, outarg_end - outarg_start,
                   offs,
                   stmt[outarg_start:outarg_end] + " = ")
            expr_end_offs += outarg_end - outarg_start + 3
            semi_offs += 3
        junk_length = semi_offs - expr_end_offs
        # cleanup: "(a, b) = f() -> ;" => "(a, b) = f();"
        f.edit(expr_end_offs, junk_length, "")

class PAFTER(Transformation):
    def apply(self, f):
        [call_offs, expr_offs, lparen_offs, rparen_offs] = [
            self.offset(f, loc)
            for loc in [self.loc] + self.params]
        f.edit(call_offs, (expr_offs - call_offs), '')
        f.edit(rparen_offs, ')', ' s:')
        f.edit(lparen_offs, '(',
               '' if str(f.read_chunk(lparen_offs - 1, lparen_offs)).isspace()
               else ' ')

class PRETVAL(Transformation):
    # after POUTARGRETURN, PRETVAL_END, PDOLLAR_QUALIFY, PVAL
    phase = 2
    def apply(self, f):
        (body_start, body_end, rparen, outargs) = self.params
        body = str(f.read_chunk(self.offset(f, body_start),
                                self.offset(f, body_end)))
        assert body.startswith('{')
        body = body[1:]
        # detect indentation
        end_brace_indent = rspace(body)
        linesep = end_brace_indent + '    '
        names = []
        # If there is only one outarg, then chances are that
        # POUTARGRETURN removed all references to it. Detect this by
        # lexing the body: any identifier 'val' is likely a reference
        # to outarg 'val'. An identifier preceded by '.' does not
        # count; this covers the common case when a 1.2 method returns
        # `val = $this`, which in this phase is translated to `return
        # this.val`, which should not trigger a declaration.
        if len(outargs) == 1:
            [(_, name)] = outargs
            # e.g., an outarg named `data` is lexed as a 'DATA' token
            from dml import dmllex12
            kind = (name.upper() if name.upper() in dmllex12.reserved_idents
                    else 'ID')
            tokens = list(f.read_tokens(self.offset(f, body_start),
                                        self.offset(f, body_end)))
            add_locals = any(
                (kind2, tok2) == (kind, name) and kind1 != 'DOT'
                for ((_, _, kind1), (_, tok2, kind2))
                in zip(tokens, tokens[1:]))
        else:
            add_locals = True
        # Process arguments from right to left. This happens to make
        # some location calculations easier.
        for [(start, name), (end, _)] in reversed(list(zip(
                outargs, outargs[1:] + [(rparen, None)]))):
            # First, remove name from declaration
            start = self.offset(f, start)
            decl = str(f.read_chunk(start, self.offset(f, end)))
            name_idx = decl.rfind(name)
            decl = decl[:name_idx + len(name)]
            cutstart_idx = len(decl[:name_idx].rstrip())
            if ' ' not in decl:
                # Untyped output. Guess it's a uint64
                decl = "uint64 " + decl
            names.append(name)
            cutlen = len(name) + name_idx - cutstart_idx
            f.edit(start + cutstart_idx, cutlen, '')
            # Second, add temporary local variable
            if add_locals:
                f.edit(self.offset(f, body_start), '{',
                       '{%slocal %s;' % (linesep, decl))
        if len(names) > 1:
            f.edit(self.offset(f, rparen) + 1, 0,
                   ' /* %s */' % (', '.join(reversed(names))))

class PRETVAL_END(Transformation):
    # before PRETVAL
    phase = 0
    def apply(self, f):
        (body_end, outargs) = self.params
        body_end_offs = self.offset(f, body_end)
        end_brace_indent = f.read_line_up_to(body_end_offs)
        if len(outargs) == 1:
            [ret_expr] = outargs
        else:
            ret_expr = '(%s)' % (', '.join(outargs))
        f.edit(body_end_offs, 0,
               '    return %s;\n%s' % (ret_expr, end_brace_indent))

class PRETURNARGS(Transformation):
    # before POUTARGRETURN and PRETVAL
    phase = 0
    def apply(self, f):
        offs = self.offset(f)
        (names,) = self.params
        text = f.read_chunk(offs, offs + 6)
        assert text == 'return'
        ret_expr = ', '.join(names)
        if len(names) > 1:
            ret_expr = '(%s)' % ret_expr
        f.edit(offs, 'return', 'return ' + ret_expr)

class POUTARGRETURN(Transformation):
    # after PRETURNARGS and PRETVAL_END
    phase = 1
    def apply(self, f):
        offs = self.offset(f)
        (expr_start, return_start) = self.params
        expr_offs = self.offset(f, expr_start)
        # First, remove '\n    return outarg;'
        if return_start is not None:
            return_offs = self.offset(f, return_start)
            ret_prefix = '\n' + f.read_line_up_to(return_offs)
            if not ret_prefix.isspace():
                ret_prefix = ret_prefix[len(ret_prefix.rstrip()):]
            ret_chars = len(ret_prefix)
            ret_tokens = f.read_tokens(return_offs)
            (skip, tok, kind) = next(ret_tokens)
            assert kind == 'RETURN', kind
            ret_chars += skip + len(tok)
            (skip, tok, kind) = next(ret_tokens)
            assert kind == 'ID', kind
            ret_chars += skip + len(tok)
            (skip, tok, kind) = next(ret_tokens)
            assert kind == 'SEMI', kind
            ret_chars += skip + len(tok)
            f.edit(return_offs - len(ret_prefix), ret_chars, '')

        # Second, change 'outarg = f();' to 'return f();'
        f.edit(offs, expr_offs - offs, 'return ')

class PSOFT_RESET_VALUE(Transformation):
    def apply(self, f):
        offs = self.offset(f)
        prefix = f.read_line_up_to(offs)
        tokens = f.read_tokens(offs)
        (skip, tok, kind) = next(tokens)
        assert kind == 'PARAM'
        assert tok == 'param'
        param_offs = offs + skip + len(tok)
        (skip, tok, kind) = next(tokens)
        assert kind == 'ID'
        param_offs += skip
        assert tok == 'soft_reset_value', tok
        f.edit(param_offs, tok, 'soft_reset_val')
        f.edit(offs, 0, 'is soft_reset_val;\n' + prefix)

class PMISS_PATTERN(Transformation):
    def apply(self, f):
        offs = self.offset(f)
        prefix = f.read_line_up_to(offs)
        # site points at 'miss_pattern'
        param_start = prefix.rfind('param')
        assert param_start > 0
        offs = offs - len(prefix) + param_start
        indent = rspace(prefix[:param_start])
        f.edit(offs, 0, 'is miss_pattern_bank;\n' + indent)

def instantiate_template(f, decl_offset, new_tpl):
    tokens = f.read_tokens(decl_offset)
    offs = decl_offset
    desc_offs = None
    for (skip, tok, kind) in tokens:
        if kind == 'IS':
            offs += skip + len(tok)
            break
        # A string constant is very likely a desc string
        # (though you might write 'register r @ xyz == "abc" ? 1 : 2')
        if kind == 'SCONST' and not desc_offs:
            desc_offs = offs + skip
        if kind in ('SEMI', 'LBRACE'):
            f.edit(desc_offs or offs + skip, 0, 'is %s ' % (new_tpl,))
            return
        offs += skip + len(tok)
    (skip, tok, kind) = next(tokens)
    offs += skip
    if kind == 'ID':
        length = len(tok)
        tpls = [str(tok)]
    else:
        assert tok == '('
        length = len(tok)
        tpls = []
        for (skip, tok, kind) in tokens:
            length += skip + len(tok)
            if kind == 'ID':
                tpls.append(str(tok))
            elif kind == 'RPAREN':
                break
            else:
                assert kind == 'COMMA'

    f.edit(offs, length, '(%s)' % (', '.join(sorted(tpls + [new_tpl])),))

class PATTRIBUTE(Transformation):
    # after PEVENT_UINT64_ARG
    phase = 2
    uint64_event_sites = set()

    def remove_param_decl(self, f, param_site):
        offs = self.offset(f, param_site)
        # skip spaces backward
        offs -= len(rspace(f.read_line_up_to(offs)))
        line = f.read_line(offs) + '\n'
        end_of_param = line.find(';') + 1
        assert end_of_param > 0
        # skip spaces forward, including newline
        end_of_param += len(lspace(line[end_of_param:]))
        f.edit(offs, end_of_param, '')

    def apply(self, f):
        offs = self.offset(f)
        (template, allocate_type_param, type_param) = self.params
        if template:
            if os.path.normcase(self.loc) in self.uint64_event_sites:
                template = template.replace('custom_', 'uint64_')
            instantiate_template(f, offs, template)
        if allocate_type_param:
            if template is None:
                # Add `.val` manually if removing allocate_type param
                # without adding `is <type>_attr`.
                alloc_type_offs = self.offset(f, allocate_type_param)
                alloc_type_str = f.read_line(alloc_type_offs)
                m = re.search('"(.*)"', alloc_type_str)
                if m is not None:
                    f.edit(alloc_type_offs, 0, f'session {m.group(1)} val;\n')
            self.remove_param_decl(f, allocate_type_param)
        if type_param:
            self.remove_param_decl(f, type_param)

class PEVENT_NO_ARG(Transformation):
    # after PINVOKE
    phase = 1
    def apply(self, f):
        start_offs = self.offset(f)
        line = f.read_line_up_to(start_offs)
        start_offs += len(line.rstrip().rstrip(',')) - len(line)
        (end_site,) = self.params
        end_offs = self.offset(f, end_site)
        end_offs -= 1
        f.edit(start_offs, end_offs - start_offs, '')

class PEVENT_UINT64_ARG(Transformation):
    # after PINVOKE
    phase = 1
    def apply(self, f):
        start_offs = self.offset(f)
        (end_site, event_site) = self.params
        PATTRIBUTE.uint64_event_sites.add(event_site)
        # remove `cast(..., void *)`
        end_offs = self.offset(f, end_site)
        line = f.read_line_up_to(end_offs)
        voidp_chars = len(line) - line.rindex(',')
        f.edit(end_offs - voidp_chars, voidp_chars + 1, '')
        f.edit(start_offs, 'cast(', '')

class RemoveMethod(Transformation):
    # after PRETVAL
    phase = 3
    def apply(self, f):
        offs = self.offset(f)
        (end_site,) = self.params
        end_offs = self.offset(f, end_site)
        offs -= len(rspace(f.read_line_up_to(offs)))
        end_offs += len(f.read_regexp(end_offs, re.compile(' *[}] *'))) + 1
        extra_blank = f.read_line(end_offs)
        sys.stderr.write(repr([end_offs, extra_blank]))
        if not extra_blank.strip():
            end_offs += len(extra_blank) + 1

        f.edit(offs, end_offs - offs, '')


class POVERRIDE(Transformation):
    class EPOVERRIDE(PortingFailure): tag = 'EPOVERRIDE'
    def apply(self, f):
        offs = self.offset(f)
        (new_tpl,) = self.params
        line = f.read_line(offs)
        if not line.startswith('template'):
            tokens = f.read_tokens(offs)
            (_, objkind, _) = next(tokens)
            (_, objname, _) = next(tokens)
            raise self.EPOVERRIDE(
                "Need to insert 'is %s' so it applies to %s %s."
                % (new_tpl, objkind, objname)
                + " Please check if %s should be applied directly to %s"
                % (new_tpl, objname)
                + " or on a parent object, and do that manually")
        instantiate_template(f, offs, new_tpl)

class POVERRIDE_IMPORT(Transformation):
    def apply(self, f):
        (path,) = self.params
        offsets = []
        offset = 0
        tokens = []
        texts = []
        for (pad, text, kind) in f.read_tokens(0):
            offset += pad
            offsets.append(offset)
            tokens.append(kind)
            texts.append(text)
            offset += len(text)

        # insert the import after 'dml 1.3;' if no import is found
        assert tokens[0:3] == ['ID', 'FCONST', 'SEMI']
        last_import_semi = 2
        # if there are any imports, insert the extra import last.
        for i in range(len(tokens) - 2):
            if tokens[i:i+3] == ['IMPORT', 'SCONST', 'SEMI']:
                # semi
                last_import_semi = i + 2
            if texts[i + 1] == '"%s"' % (path,):
                # already imported
                return
        offset = offsets[last_import_semi] + 1
        f.edit(offset, 0, '\nimport "%s";' % (path,))

class PBEFAFT(Transformation):
    # must come after PINLINEDECL
    phase = 1
    def apply(self, f):
        offs = self.offset(f)
        (write_start_site, method_decl, default_call,
         return_stmt) = self.params[:4]
        write_start = self.offset(f, write_start_site)
        f.edit(offs, write_start - offs, 'method %s ' % (method_decl,))

        [bef_site, bef_body_start, bef_body_end] = [
            self.offset(f, site) if site else None
            for site in self.params[4:7]]

        if bef_site:
            indent = f.read_next_indent(bef_body_start)
            end_brace_indent = rspace(f.read_line_up_to(bef_body_end))
            last_line = bef_body_end - len(end_brace_indent)
            default_line = '%s%s;' % (indent[1:], default_call)
            f.edit(last_line, 0, default_line + '\n')
            [aft_site, aft_body_start, aft_body_end] = [
                self.offset(f, site) if site else None
                for site in self.params[7:]]
            if aft_site:
                # 'before_write' appears before 'after_write' in
                # normal code, though there is of course no guarantee
                assert aft_site > bef_site
                aft_indent = len(rspace('\n' + f.read_line_up_to(aft_site)))
                aft_body = f.read_chunk(aft_body_start + 1, aft_body_end)
                aft_proto_len = aft_body_start + 1 - aft_site
                f.edit(aft_site - aft_indent, aft_indent + aft_proto_len, '')
                return_line = indent + return_stmt if return_stmt else ''
                f.move(aft_site - aft_indent, aft_body_end - aft_body_start,
                       last_line + len(default_line),
                       aft_body.rstrip() + return_line)
            else:
                assert not return_stmt
        else:
            [aft_site, aft_body_start] = [
                self.offset(f, site) if site else None
                for site in self.params[7:9]]
            indent = f.read_next_indent(aft_body_start)
            f.edit(aft_body_start + 1, 0, '%s%s;' % (indent, default_call))
            aft_body_end = self.offset(f, self.params[9])
            end_line = aft_body_end - len(rspace(
                f.read_line_up_to(aft_body_end)))
            if return_stmt:
                return_line = indent[1:] + return_stmt + '\n'
                f.edit(end_line, 0, return_line)

class PABSTRACT_TEMPLATE(Transformation):
    def apply(self, f):
        offs = self.offset(f)
        (name,) = self.params
        indent = rspace(f.read_line_up_to(offs))
        f.edit(offs, 0, 'is %s;\n%s' % (name, indent))

class PTRAMPOLINE(Transformation):
    def apply(self, f):
        offs = self.offset(f)
        (trampoline_dml14, trampoline_dml12) = self.params
        indent = rspace(f.read_line_up_to(offs))

        # emit 1.2 compatibility trampoline only if --compat was passed
        if f.compat:
            if trampoline_dml14 is None:
                trampoline = trampoline_dml12
            else:
                trampoline = '%s\n\n%s' % (trampoline_dml14, trampoline_dml12)
        else:
            if trampoline_dml14 is None:
                return
            else:
                trampoline = trampoline_dml14
        f.edit(offs, 0,
               trampoline.replace('\n', '\n' + indent).replace(
                   '\n%s\n' % (indent,), '\n\n')
               + '\n\n' + indent)

class PIMPORT_DML12COMPAT(POVERRIDE_IMPORT):
    def __init__(self, loc, params):
        super(PIMPORT_DML12COMPAT, self).__init__(
            loc, params + ['dml12-compatibility.dml'])
    def apply(self, f):
        if not f.compat:
            return
        super(PIMPORT_DML12COMPAT, self).apply(f)

class PCHANGE_INARGS(Transformation):
    # must happen after PINLINEDECL
    phase = 2
    def apply(self, f):
        offs = self.offset(f)
        (new_decl,) = self.params
        f.edit(offs, len(f.read_to_char(offs, ')')), new_decl)

class PBITNEQ(Transformation):
    # before PVAL
    phase = 0
    def apply(self, f):
        start_offs = self.offset(f)
        [end_site] = self.params
        end_offs = self.offset(f, end_site)
        (pad, token, kind) = next(f.read_tokens(end_offs))
        from dml import dmllex14
        if kind in ('ID', 'RBRACKET', 'THIS', 'REGISTER', 'SIGNED',
                    'UNSIGNED') or kind in dmllex14.reserved_idents:
            end_offs += pad + len(token)
        line = f.read_line_up_to(start_offs)
        if line.rstrip().endswith('!'):
            # !f => f == 0
            f.edit(end_offs, 0, ' == 0')
            not_offset = start_offs - 1 - len(line) + len(line.rstrip())
            f.edit(not_offset, '!', '')
        else:
            # f => f != 0
            f.edit(end_offs, 0, ' != 0')

class PVAL(Transformation):
    # must happen after PBITNEQ
    phase = 1
    def apply(self, f):
        offs = self.offset(f)
        offs += f.skip_token(offs)
        f.edit(offs, 0, '.val')

class PPARAMETER(Replace):
    # must happen before all transformations that remove parameter declarations
    phase = -1
    def __init__(self, loc, params):
        assert params == []
        Replace.__init__(self, loc, ['parameter', 'param'])

class PNODOLLAR(Replace):
    # must happen before all transformations that use read_tokens
    phase = -1
    def __init__(self, loc, params):
        assert params == []
        Replace.__init__(self, loc, ['$', ''])

class PARRAY(Transformation):
    def apply(self, f):
        in_offs = self.offset(f)
        dotdot_offs = self.offset(f, self.params[0])
        f.edit(in_offs, dotdot_offs + 2 - in_offs, '< ')
        # 'in 0..5': start=5, end=5, repl='6'             => ' < 6'
        # 'in 0..a-1': start=-, end=1, repl=''            => ' < a'
        # 'in 0..expr': start=expr, end=expr, repl=' + 1' => ' < expr + 1'
        (repl_start_site, repl_end_site, replacement) = self.params[1:]
        start_offs = self.offset(f, repl_start_site)
        if not replacement:
            start_offs -= len(rspace(f.read_line_up_to(start_offs)))
        end_offs = self.offset(f, repl_end_site)
        (pad, token, kind) = next(f.read_tokens(end_offs))
        end_offs += pad + len(token)
        if replacement == ' + 1':
            start_offs = end_offs
        f.edit(start_offs, end_offs - start_offs, replacement)

        # scrub parenthesis after 'in 0..(n-1)' => '< (n)'
        openparen_offs = self.offset(f) + 2
        closeparen_offs = start_offs + len(replacement)
        if (f.read_chunk(openparen_offs, openparen_offs + 1) == '('
            and f.read_chunk(closeparen_offs, closeparen_offs + 1) == ')'):
            f.edit(closeparen_offs, ')', '')
            f.edit(openparen_offs, '(', '')

class PHASHELSE(Transformation):
    def apply(self, f):
        offs = self.offset(f)
        [else_token] = self.params
        else_offs = offs + len(f.read_to_char(offs, else_token)) - 1
        f.edit(else_offs, '', '#')

class PANDOR(Transformation):
    # after PIFAND
    phase=1
    def apply(self, f):
        offs = self.offset(f)
        if self.loc in PIFAND.and_sites:
            return
        [start_site, end_site, op, op_repl, suff_repl] = self.params
        start_offs = self.offset(f, start_site)
        end_offs = self.offset(f, end_site)
        # end_offs points at the last token of the expression; skip
        # one more token
        end_offs += f.skip_token(end_offs)
        # avoid the double parenthesis in
        # if (X && Y) -> if ((X #? Y #: false))
        (pad, token, kind) = next(f.read_tokens(end_offs))
        skip_paren = (kind == 'RPAREN'
                      and f.read_line_up_to(start_offs).endswith('('))
        f.edit(end_offs, '', suff_repl + ('' if skip_paren else ')'))
        f.edit(offs, op, op_repl)
        if not skip_paren:
            f.edit(start_offs, '', '(')

class PIFAND(Transformation):
    and_sites=set()
    def apply(self, f):
        and_offs = self.offset(f)
        [if_site, end_site] = self.params
        # this transformation replaces PANDOR; and_sites is used to
        # suppress it
        self.and_sites.add(self.loc)
        if_offs = self.offset(f, if_site)
        end_offs = self.offset(f, end_site)
        cond_offs = if_offs + f.skip_tokens(if_offs, 2)
        cond = f.read_chunk(cond_offs, and_offs)
        indent = f.read_line_up_to(if_offs)
        indent = indent[:len(indent)-len(indent.lstrip())]
        end = end_offs + len(f.read_line(end_offs))
        f.edit(end, '', '\n%s}' % indent)
        # indent if block
        block = f.read_chunk(and_offs, end_offs)
        while '\n' in block:
            index = block.rfind('\n')
            f.edit(and_offs + index + 1, '', '    ')
            block = block[:index]
        after_and = f.read_line(and_offs)[2:]
        skip_after_and = 2 + len(after_and) - len(after_and.lstrip())
        f.edit(and_offs, skip_after_and, '')
        f.move(cond_offs, cond, if_offs, cond.rstrip() + ') {\n'
               + indent + '    ')
        f.edit(if_offs, '', '#if (')

class PSTRINGIFY(Transformation):
    # must happen before other transformations that cannot handle the # token
    phase = -1
    def apply(self, f):
        offs = self.offset(f)
        [end_site] = self.params
        end_offs = self.offset(f, end_site)
        expr_end_offs = end_offs + f.skip_token(end_offs)
        f.edit(expr_end_offs, 0, ')')
        f.edit(offs, '#', 'stringify(')

class PNO_WUNUSED(Transformation):
    # before PWUNUSED
    phase = -1
    used = set()
    def apply(self, f):
        [kind, name] = self.params
        self.used.add((self.loc, kind))

class PWUNUSED(Transformation):
    unused = set()
    def apply(self, f):
        [kind, name] = self.params
        if (self.loc, kind) not in PNO_WUNUSED.used:
            self.unused.add((self.loc, kind, name))
    @staticmethod
    def report(f, dest):
        for (loc, kind, name) in sorted(PWUNUSED.unused):
            (path, orig_offset) = decode_loc(loc)
            new_offset = f.translate_offs(orig_offset)
            line = f.read_chunk(0, new_offset).count('\n') + 1
            if kind == 'if':
                msg = 'true branch never taken'
            elif kind == 'else':
                msg = 'false branch never taken'
            elif kind == 'method':
                name = name.rsplit('___default')[0]
                msg = 'method %r never invoked' % (name,)
            elif kind == 'template':
                msg = 'template %r never instantiated' % (name,)
            else:
                assert False
            sys.stderr.write('%s:%d: warning WUNUSED: %s during build.'
                             % (dest, line, msg)
                             + ' Only partial porting possible\n')
            sys.stderr.write('%s: original location\n' % (loc,))

tags = {
    'PSHA1': PSHA1,
    'PVERSION': replace_const('1.2', '1.4'),
    'PTHROWS': replace_const('{', 'throws {'),
    'PNOTHROW': PNOTHROW,
    'PINPARAMLIST': PINPARAMLIST,
    'PSTRUCTDECL': PSTRUCTDECL,
    'PFIELDRANGE': replace_const('', ' @ '),
    'PINLINEDECL': Replace,
    'PTYPEDOUTPARAM': PTYPEDOUTPARAM,
    'PINARGTYPE': PINARGTYPE,
    'PINVOKE': PINVOKE,
    'PLOGKIND': Replace,
    'PAFTER': PAFTER,
    'PAUTO': replace_const('auto', 'local'),
    'PRETVAL': PRETVAL,
    'PRETVAL_END': PRETVAL_END,
    'PRETURNARGS': PRETURNARGS,
    'POUTARGRETURN': POUTARGRETURN,
    'PSESSION': Replace,
    'PHARD_RESET_VALUE': replace_const('hard_reset_value', 'init_val'),
    'PSOFT_RESET_VALUE': PSOFT_RESET_VALUE,
    'PMISS_PATTERN': PMISS_PATTERN,
    'PATTRIBUTE': PATTRIBUTE,
    'PEVENT': PATTRIBUTE,
    'PEVENT_NO_ARG': PEVENT_NO_ARG,
    'PEVENT_UINT64_ARG': PEVENT_UINT64_ARG,
    'PEVENT_REMOVE_INFO': RemoveMethod,
    'POVERRIDE': POVERRIDE,
    'POVERRIDE_IMPORT': POVERRIDE_IMPORT,
    'PBEFAFT': PBEFAFT,
    'PABSTRACT_TEMPLATE': PABSTRACT_TEMPLATE,
    'PTRAMPOLINE': PTRAMPOLINE,
    'PIMPORT_DML12COMPAT': PIMPORT_DML12COMPAT,
    'PCHANGE_INARGS': PCHANGE_INARGS,
    'PBITNEQ': PBITNEQ,
    'PVAL': PVAL,
    'PNODOLLAR': PNODOLLAR,
    'PDOLLAR_QUALIFY': Replace,
    'PCONSTANT': replace_const('constant', 'param'),
    'PPARAMETER': PPARAMETER,
    'PARRAY_I': replace_const('', 'i < '),
    'PARRAY': PARRAY,
    'PHASH': replace_const('', '#'),
    'PHASHELSE': PHASHELSE,
    'PANDOR': PANDOR,
    'PIFAND': PIFAND,
    'PSTRINGIFY': PSTRINGIFY,
    'PWUNUSED': PWUNUSED,
    'PNO_WUNUSED': PNO_WUNUSED,
    'PRENAME_TEMPLATE': Replace,
    'PUNDEFOFFS': replace_const('undefined', 'unmapped_offset'),
    'PINT1': replace_const('int1', 'uint1'),
}

def is_device_file(path):
    lexer = init_lexer(path, 1)
    with open(path) as f:
        lexer.input(f.read())
    tokens = [lexer.token() for _ in range(4)]
    return tokens[3] is not None and tokens[3].type == 'DEVICE'

def main(argv):
    parser = argparse.ArgumentParser(
        description='Convert a DML source file from DML 1.2 to DML 1.4')
    parser.add_argument("--tags", dest="tags", type=str,
                        help="File containing the output of 'dmlc -P -T'")
    parser.add_argument("--src", dest="src", type=str,
                        required=True, help="Source (DML 1.2) file")
    parser.add_argument("--dest", dest="dest", type=str,
                        help="Destination (DML 1.4) file")
    parser.add_argument("--compat", dest="compat", action="store_true",
                        help="Emit extra code to make code work better when"
                        " imported from DML 1.2")

    if '--unittest' in argv[1:]:
        # Invoked by t126
        unittest.main(argv=argv[:1])

    args = parser.parse_args(argv[1:])

    if args.tags:
        tagfile = open(args.tags)
        tagfilename = args.tags
    else:
        tagfile = sys.stdin
        tagfilename = "<stdin>"

    find_lexer(Path(__file__))

    # Unordered set of decoded lines
    already_added = set()
    # Maps phase to ordered list
    transformations = {}
    for (lineno, line) in enumerate(tagfile, 1):
        try:
            # Extract lines on the form "/path/foo.dml:1:3: porting PXYZ: args"
            # which apply to args.src
            (loc, sep, msg) = line.partition(': ')
            if not sep:
                continue
            (kind_tag, sep, params) = msg.partition(': ')
            if not kind_tag.startswith('porting '):
                continue
            (kind, tag) = kind_tag.split()
            if loc == '<unknown>':
                # sometimes happens for libs like utility.dml
                continue
            (path, row, col) = loc.rsplit(':', 2)
            if not os.path.samefile(path, args.src):
                continue
            key = (row, col, tag, params)
            # avoid duplicate transformations
            if key in already_added:
                continue
            already_added.add(key)
            t = tags[tag](loc, ast.literal_eval(params))
            transformations.setdefault(t.phase, []).append((t, lineno, line))
        except:
            sys.stderr.write("Unexpected error on this porting tag:\n")
            sys.stderr.write(line)
            sys.stderr.write("%s:%d: found here\n" % (tagfilename, lineno))
            raise

    if not transformations:
        sys.stderr.write('%s:0: error: no tags found matching file %r' % (
            tagfilename, args.src))
        exit(1)

    if args.compat and is_device_file(args.src):
        sys.stderr.write('%s:0: warning: file contains device statement,'
                         % (args.src,) + ' ignoring --compat flag\n')
        args.compat = False
    src = SourceFile(args.src, args.compat)
    errors = 0
    for phase in sorted(transformations):
        for (t, lineno, line) in transformations[phase]:
            try:
                t.apply(src)
            except PortingFailure as e:
                (loc, _) = line.split(' porting ', 2)
                sys.stderr.write("%s error%s: %s\n" % (
                    loc, '' if e.tag is None else ' ' + e.tag, e))
                sys.stderr.write("%s:%d: found here\n" % (tagfilename, lineno))
                errors += 1
            except:
                sys.stderr.write("Unexpected error on this porting tag:\n")
                sys.stderr.write(line)
                sys.stderr.write("%s:%d: found here\n" % (tagfilename, lineno))
                traceback.print_exc()
                errors += 1

    PWUNUSED.report(src, args.dest)

    with (open(args.dest, 'w', newline='') if args.dest else sys.stdout) as f:
        src.commit(f)
    if errors:
        total = sum(map(len, transformations.values()))
        sys.stderr.write(f'''\
*** Failed to apply {errors} out of {total} porting tags; partial result saved to {args.dest}. Consider applying the failed tags manually.
''')
        exit(2)

if __name__ == '__main__':
    main(sys.argv)
