# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from .dmllex import *

hashids = {'#' + kw: 'HASH' + kw.upper()
           for kw in [
                   'if',
                   'else',
                   'foreach',
                   'select',
           ]}

tokens = (common_tokens
          + ('HASHCONDOP', 'HASHCOLON')
          + tuple(hashids.values()))

t_HASHCONDOP = r'\#\?'
t_HASHCOLON = r'\#:'

keywords_dml14 = dict(keywords_common)
for kw in ['param', 'saved', 'async', 'await', 'with', 'shared', 'stringify',
           'export', 'as', 'independent', 'startup', 'memoized', 'hook']:
    keywords_dml14[kw] = kw.upper()
    tokens += (kw.upper(),)

reserved_idents = reserved_idents_common + (
    'PARAM', 'SAVED', 'INDEPENDENT', 'STARTUP', 'MEMOIZED')

def t_ID(t):
    r'[A-Za-z_][\w_]*'
    t.type = keywords_dml14.get(t.value, 'ID')
    return t

def t_HASHID(t):
    r'\#[A-Za-z_][\w_]*'
    value = hashids.get(t.value)
    if not value:
        report(ESYNTAX(DumpableSite(t.lexer.file_info, t.lexpos), t.value,
                       "illegal # symbol"))
        return None
    t.type = value
    return t
