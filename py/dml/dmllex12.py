# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from .dmllex import *

tokens = common_tokens + ('DOLLAR', 'DOTDOT', 'HASH')
t_DOLLAR = r'\$'
t_DOTDOT = r'\.\.'
t_HASH = r'\#'

keywords_dml12 = dict(keywords_common)
for kw in ['parameter', 'trait']:
    keywords_dml12[kw] = kw.upper()
    tokens += (kw.upper(),)
    reserved_idents = reserved_idents_common + (kw.upper(),)

def t_ID(t):
    r'[A-Za-z_][\w_]*'
    t.type = keywords_dml12.get(t.value, 'ID')
    return t
