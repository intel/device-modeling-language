# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Lexer

from ply import lex
from .logging import report, DumpableSite
from .messages import *
import re

# Reserved words allowed as identifiers
reserved_idents_common = (
    'ATTRIBUTE', 'BANK', 'BITORDER', 'CONNECT', 'CONSTANT', 'DATA',
    'DEVICE', 'EVENT', 'FIELD', 'FOOTER', 'GROUP', 'HEADER', 'IMPLEMENT',
    'IMPORT', 'INTERFACE', 'LOGGROUP', 'METHOD', 'PORT', 'SIZE',
    'SUBDEVICE', 'NOTHROW', 'THEN', 'THROWS', '_HEADER', 'PROVISIONAL',
)

reserved = reserved_idents_common + (
    # Particular to DML (keywords - not allowed as identifiers)
    'AFTER', 'ASSERT', 'BITFIELDS', 'CALL', 'CAST', 'DEFINED', 'ERROR',
    'FOREACH', 'IN', 'IS', 'LAYOUT', 'LOCAL', 'LOG', 'SELECT',
    'SIZEOFTYPE', 'TYPEOF', 'UNDEFINED', 'VECT', '_WARNING', 'WHERE',
    'EACH', 'SESSION', 'SEQUENCE',

    # ANSI C reserved words
    'AUTO', 'BREAK', 'CASE', 'CHAR', 'CONST', 'CONTINUE', 'DEFAULT',
    'DO', 'DOUBLE', 'ELSE', 'ENUM', 'EXTERN', 'FLOAT', 'FOR', 'GOTO',
    'IF', 'INT', 'LONG', 'REGISTER', 'RETURN', 'SHORT', 'SIGNED',
    'SIZEOF', 'STATIC', 'STRUCT', 'SWITCH', 'TYPEDEF', 'UNION',
    'UNSIGNED', 'VOID', 'VOLATILE', 'WHILE',

    # Particular to C99 and C++
    'DELETE',           # C++
    'INLINE',           # C99 and C++
    'NEW',              # C++
    'RESTRICT',         # C99
    'TEMPLATE',         # C++
    'THROW',            # C++
    'TRY',              # C++
    'CATCH',            # C++
    'THIS',             # C++

    # C++ reserved words being reserved by DML for future use
    'CLASS', 'NAMESPACE', 'PRIVATE', 'PROTECTED', 'PUBLIC',
    'USING', 'VIRTUAL',
    )

common_tokens = reserved + (
    # Literals (identifier, integer constant, hex constant, binary
    # constant, float constant, string constant, char const)
    'ID', 'ICONST', 'HCONST', 'BCONST', 'FCONST', 'SCONST', 'CCONST',

    # Operators (+, -, *, /, %, |, &, ~, ^, <<, >>, ||, &&, !, <, <=, >,
    # >=, ==, !=)
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MOD',
    'BOR', 'BAND', 'BNOT', 'BXOR', 'LSHIFT', 'RSHIFT',
    'LOR', 'LAND', 'LNOT',
    'LT', 'LE', 'GT', 'GE', 'EQ', 'NE',

    # Assignment (=, *=, /=, %=, +=, -=, <<=, >>=, &=, ^=, |=)
    'EQUALS', 'TIMESEQUAL', 'DIVEQUAL', 'MODEQUAL', 'PLUSEQUAL', 'MINUSEQUAL',
    'LSHIFTEQUAL', 'RSHIFTEQUAL', 'BANDEQUAL', 'BXOREQUAL', 'BOREQUAL',

    # Increment/decrement (++, --)
    'PLUSPLUS', 'MINUSMINUS',

    # Structure dereference (->)
    'ARROW',

    # Conditional operator (?)
    'CONDOP',

    # Delimiters ( ) [ ] { } , . ; :
    'LPAREN', 'RPAREN',
    'LBRACKET', 'RBRACKET',
    'LBRACE', 'RBRACE',
    'COMMA', 'PERIOD', 'SEMI', 'COLON',

    # Ellipsis (...)
    'ELLIPSIS',

    # DML extensions (non-C)
    'AT',
    'CBLOCK'            # raw sections of text
    )

# Completely ignored characters
t_ignore           = ' \t\x0c'

# Newlines
def t_NEWLINE(t):
    r'(\r?\n)+'
    t.lineno += t.value.count("\n")

# Operators
t_PLUS             = r'\+'
t_MINUS            = r'-'
t_TIMES            = r'\*'
t_DIVIDE           = r'/'
t_MOD              = r'%'
t_BOR              = r'\|'
t_BAND             = r'&'
t_BNOT             = r'~'
t_BXOR             = r'\^'
t_LSHIFT           = r'<<'
t_RSHIFT           = r'>>'
t_LOR              = r'\|\|'
t_LAND             = r'&&'
t_LNOT             = r'!'
t_LT               = r'<'
t_GT               = r'>'
t_LE               = r'<='
t_GE               = r'>='
t_EQ               = r'=='
t_NE               = r'!='

# Assignment operators

t_EQUALS           = r'='
t_TIMESEQUAL       = r'\*='
t_DIVEQUAL         = r'/='
t_MODEQUAL         = r'%='
t_PLUSEQUAL        = r'\+='
t_MINUSEQUAL       = r'-='
t_LSHIFTEQUAL      = r'<<='
t_RSHIFTEQUAL      = r'>>='
t_BANDEQUAL        = r'&='
t_BOREQUAL         = r'\|='
t_BXOREQUAL        = r'\^='

# Increment/decrement
t_PLUSPLUS         = r'\+\+'
t_MINUSMINUS       = r'--'

# ->
t_ARROW            = r'->'

# ?
t_CONDOP           = r'\?'

# Delimiters
t_LPAREN           = r'\('
t_RPAREN           = r'\)'
t_LBRACKET         = r'\['
t_RBRACKET         = r'\]'
t_LBRACE           = r'\{'
t_RBRACE           = r'\}'
t_COMMA            = r','
t_PERIOD           = r'\.'
t_SEMI             = r';'
t_COLON            = r':'
t_ELLIPSIS         = r'\.\.\.'


# DML-specific (non-C)
t_AT = r'@'

def t_CBLOCK(t):
    r'%{([^%]|%[^}])*%}'
    t.lineno += t.value.count('\n')
    t.value = t.value[2:-2]
    return t

# This is a hack. the underlying regexp handling in Python that the lex
# implementation uses cannot handle too many (> 100) "named groups", so
# we cannot have one rule per reserved word. This means that keywords
# *must* have the same shape as identifiers.
keywords_common = {r.lower(): r for r in reserved}

# Floating-point literal
# Keep this before t_INT
def t_FCONST(t):
    r'[0-9]*(\.[0-9]+([eE]-?[0-9]+)?|([eE]-?[0-9]+))'
    t.value = float(t.value)
    return t

def rangecheck_int(t, value):
    if value >= 1 << 64:
        try:
            syntax_error(t, t.value, 'too large integer constant')
        except ESYNTAX as e:
            report(e)
        return value & ((1 << 64) - 1)
    return value

# Hexadecimal integer literal
# Keep this before t_INT
def t_HCONST(t):
    r'0x[0-9a-fA-F_]*[0-9a-fA-F]'
    t.value = rangecheck_int(t, int(t.value[2:].replace('_', ''), 16))
    return t

# Binary integer literal
# Keep this before t_INT
def t_BCONST(t):
    r'0b[01_]*[01]'
    t.value = rangecheck_int(t, int(t.value[2:].replace('_', ''), 2))
    return t

# Integer literal (TODO: handle uUlL suffixes)
def t_ICONST(t):
    r'[0-9](?:[0-9_]*[0-9])?'
    #r'\d+([uU]|[lL]|[uU][lL]|[lL][uU])?'
    t.value = rangecheck_int(t, int(t.value.replace('_', ''), 10))
    return t


escapes = {
    ord(b'\\'): b'\\',
    ord(b'"'): b'"',
    ord(b'n'): b'\n',
    ord(b'r'): b'\r',
    ord(b't'): b'\t',
    ord(b'b'): b'\b',
    }

def syntax_error(t, tokenstr, reason):
    raise ESYNTAX(DumpableSite(t.lexer.file_info, t.lexpos), tokenstr, reason)

def t_SCONST(t):
    r'"(?:[^\x00-\x1f\x7f"\\]|\\.)*"'
    s = t.value[1:-1].encode('utf-8')
    orig_str = s
    bs = -1
    while True:
        bs = s.find(b'\\', bs + 1)
        if bs < 0:
            break
        c = s[bs + 1]
        if c in escapes:
            s = s[:bs] + escapes[c] + s[bs + 2:]
        elif c == ord(b'x') and re.match(b'[0-9a-fA-F][0-9a-fA-F]', s[bs + 2:]):
            code = int(s[bs + 2 : bs + 4], 16)
            if code >= 0x80 and re.search(b'[\x80-\xff]', orig_str):
                syntax_error(t, s[bs : bs + 4].decode('ascii'),
                             "Hex escape above \\x7f in Unicode string")
            s = s[:bs] + bytes([code]) + s[bs + 4:]
        else:
            syntax_error(t, t.value[1:-1],
                         "unrecognised character escape sequence")
    t.value = s
    return t

char_escapes = {
    b'\\': b'\\',
    b"'": b"'",
    b'n': b'\n',
    b'r': b'\r',
    b't': b'\t',
    b'b': b'\b',
    }

def t_CCONST(t):
    r"'(?:[^\x00-\x1f\x7f-\xff'\\]|\\.)[^']*'"
    c = t.value[1:-1].encode('utf-8')
    if c.startswith(b'\\'):
        if c[1:] in char_escapes:
            c = char_escapes[c[1:]]
        else:
            syntax_error(t, t.value[1:-1],
                         "unrecognised character escape sequence")
    elif len(c) != 1:
        syntax_error(t, t.value[1:-1],
                     "character constants must be a single ASCII char")
    t.value = ord(c)
    return t

# Comments
def t_comment(t):
    r'/\*([^*]|\*+[^/*])*\*+/'
    t.lineno += t.value.count('\n')

def t_cppcomment(t):
    r'//.*'

def t_error(t):
    syntax_error(t, t.value[0], "illegal character")
    t.skip(1)
