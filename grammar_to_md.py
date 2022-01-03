# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Convert a parser.out file fron yacc.py to jDOCU format

import sys
import re
from pathlib import Path

(dmlpath, infile, outfile) = sys.argv[1:]
sys.path.append(dmlpath)
from dml.dmllex14 import reserved_idents, keywords_dml14
from dml.dmllex12 import keywords_dml12

r = re.compile(r'^Rule ..... ([A-Za-z_][\w_]*) -> (.*)$')

terminals = { 'PLUS'           : '"<b>+</b>"',
              'MINUS'          : '"<b>-</b>"',
              'TIMES'          : '"<b>*</b>"',
              'DIVIDE'         : '"<b>/</b>"',
              'MOD'            : '"<b>%</b>"',
              'BOR'            : '"<b>|</b>"',
              'BAND'           : '"<b>&amp;</b>"',
              'BNOT'           : '"<b>~</b>"',
              'BXOR'           : '"<b>^</b>"',
              'LSHIFT'         : '"<b>&lt;&lt;</b>"',
              'RSHIFT'         : '"<b>&gt;&gt;</b>"',
              'LOR'            : '"<b>||</b>"',
              'LAND'           : '"<b>&amp;&amp;</b>"',
              'LNOT'           : '"<b>!</b>"',
              'LT'             : '"<b>&lt;</b>"',
              'LE'             : '"<b>&lt;=</b>"',
              'GT'             : '"<b>&gt;</b>"',
              'GE'             : '"<b>&gt;=</b>"',
              'EQ'             : '"<b>==</b>"',
              'NE'             : '"<b>!=</b>"',
              'EQUALS'         : '"<b>=</b>"',
              'TIMESEQUAL'     : '"<b>*=</b>"',
              'DIVEQUAL'       : '"<b>/=</b>"',
              'MODEQUAL'       : '"<b>%=</b>"',
              'PLUSEQUAL'      : '"<b>+=</b>"',
              'MINUSEQUAL'     : '"<b>-=</b>"',
              'LSHIFTEQUAL'    : '"<b>&lt;&lt;=</b>"',
              'RSHIFTEQUAL'    : '"<b>&gt;&gt;=</b>"',
              'BANDEQUAL'      : '"<b>&amp;=</b>"',
              'BXOREQUAL'      : '"<b>^=</b>"',
              'BOREQUAL'       : '"<b>|=</b>"',
              'PLUSPLUS'       : '"<b>++</b>"',
              'MINUSMINUS'     : '"<b>--</b>"',
              'ARROW'          : '"<b>-&gt;</b>"',
              'CONDOP'         : '"<b>?</b>"',
              'HASHCONDOP'     : '<b>#?</b>',
              'LPAREN'         : '"<b>(</b>"',
              'RPAREN'         : '"<b>)</b>"',
              'LBRACKET'       : '"<b>[</b>"',
              'RBRACKET'       : '"<b>]</b>"',
              'LBRACE'         : '"<b>{</b>"',
              'RBRACE'         : '"<b>}</b>"',
              'COMMA'          : '"<b>,</b>"',
              'PERIOD'         : '"<b>.</b>"',
              'SEMI'           : '"<b>;</b>"',
              'COLON'          : '"<b>:</b>"',
              'HASHCOLON'      : '<b>#:</b>',
              'AT'             : '"<b>@</b>"',
              'DOLLAR'         : '"<b>$</b>"',
              'DOTDOT'         : '"<b>..</b>"',
              'HASH'           : '"<b>#</b>"',
              'CBLOCK'         : '"<b>%{ ... %}</b>"',
              'HASHIF'         : '"<b>#if</b>"',
              'HASHELSE'       : '"<b>#else</b>"',
              'HASHSELECT'     : '"<b>#select</b>"',
              'HASHFOREACH'    : '"<b>#foreach</b>"',
              'ID'             : '<i>identifier</i>',
              'CCONST'         : '<i>char-literal</i>',
              'SCONST'         : '<i>string-literal</i>',
              'ICONST'         : '<i>integer-literal</i>',
              'HCONST'         : '<i>hex-literal</i>',
              'BCONST'         : '<i>binary-literal</i>',
              'FCONST'         : '<i>float-literal</i>',
              'ELLIPSIS'       : '<i>"..."</i>',
              '<empty>'        : '&lt;empty&gt;'
             }

for k in (reserved_idents
          + tuple(keywords_dml14.values())
          + tuple(keywords_dml12.values())):
    terminals[k] = '<b>%s</b>' % k.lower()

nonterminals = []
rules = []
current_term = None
current_prods = []

for line in Path(infile).read_text().splitlines():
    m = r.match(line)
    if not m:
        continue

    term = m.group(1)
    prod = m.group(2).split()
    if term == current_term:
        current_prods.append(prod)
    else:
        nonterminals.append(term)
        current_term = term
        current_prods = [prod]
        rules.append((current_term, current_prods))

with open(outfile, 'w') as f:
    f.write('# Formal Grammar\n')
    f.write('<dl>\n')
    for term, prods in rules:
        f.write(f'<dt><i>{term}</i> &rarr;</dt>\n')
        s = ''
        f.write('<dd>\n')
        for prod in prods:
            if 'error' in prod:
                continue
            f.write(s + ' ')
            for x in prod:
                if x in nonterminals:
                    f.write(f'<i> {x} </i> ')
                else:
                    f.write(f'{terminals[x]} ')
            s = '<br/><b>|</b>'
        f.write('</dd>\n')
    f.write('</dl>\n')
