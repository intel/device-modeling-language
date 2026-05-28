# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# PLY discovers t_* rules from module namespace
from .dmllex import *  # noqa: F403
from . import errors as E
from .logging import report, DumpableSite

hashids = {'#' + kw: 'HASH' + kw.upper()
           for kw in [
                   'if',
                   'else',
                   'foreach',
                   'select',
           ]}

tokens = (common_tokens  # noqa: F405
          + ('HASHCONDOP', 'HASHCOLON')
          + ('DISCARD',)
          + tuple(hashids.values()))

t_HASHCONDOP = r'\#\?'
t_HASHCOLON = r'\#:'
t_DISCARD = r'_'

keywords_dml14 = dict(keywords_common)  # noqa: F405
for kw in ['param', 'saved', 'async', 'await', 'with', 'shared', 'stringify',
           'export', 'as', 'independent', 'startup', 'memoized', 'hook']:
    keywords_dml14[kw] = kw.upper()
    tokens += (kw.upper(),)

keywords_dml14['_'] = 'DISCARD'

reserved_idents = reserved_idents_common + (  # noqa: F405
    'PARAM', 'SAVED', 'INDEPENDENT', 'STARTUP', 'MEMOIZED')

def t_ID(t):
    r'[A-Za-z_][\w_]*'
    t.type = keywords_dml14.get(t.value, 'ID')
    return t

def t_HASHID(t):
    r'\#[A-Za-z_][\w_]*'
    value = hashids.get(t.value)
    if not value:
        report(E.SYNTAX(DumpableSite(t.lexer.file_info, t.lexpos), t.value,
                       "illegal # symbol"))
        return None
    t.type = value
    return t
