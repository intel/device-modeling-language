# © 2021 Intel Corporation
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

terminals = { 'PLUS'           : '<code>+</code>',
              'MINUS'          : '<code>-</code>',
              'TIMES'          : '<code>*</code>',
              'DIVIDE'         : '<code>/</code>',
              'MOD'            : '<code>%</code>',
              'BOR'            : '<code>|</code>',
              'BAND'           : '<code>&amp;</code>',
              'BNOT'           : '<code>~</code>',
              'BXOR'           : '<code>^</code>',
              'LSHIFT'         : '<code>&lt;&lt;</code>',
              'RSHIFT'         : '<code>&gt;&gt;</code>',
              'LOR'            : '<code>||</code>',
              'LAND'           : '<code>&amp;&amp;</code>',
              'LNOT'           : '<code>!</code>',
              'LT'             : '<code>&lt;</code>',
              'LE'             : '<code>&lt;=</code>',
              'GT'             : '<code>&gt;</code>',
              'GE'             : '<code>&gt;=</code>',
              'EQ'             : '<code>==</code>',
              'NE'             : '<code>!=</code>',
              'EQUALS'         : '<code>=</code>',
              'TIMESEQUAL'     : '<code>*=</code>',
              'DIVEQUAL'       : '<code>/=</code>',
              'MODEQUAL'       : '<code>%=</code>',
              'PLUSEQUAL'      : '<code>+=</code>',
              'MINUSEQUAL'     : '<code>-=</code>',
              'LSHIFTEQUAL'    : '<code>&lt;&lt;=</code>',
              'RSHIFTEQUAL'    : '<code>&gt;&gt;=</code>',
              'BANDEQUAL'      : '<code>&amp;=</code>',
              'BXOREQUAL'      : '<code>^=</code>',
              'BOREQUAL'       : '<code>|=</code>',
              'PLUSPLUS'       : '<code>++</code>',
              'MINUSMINUS'     : '<code>--</code>',
              'ARROW'          : '<code>-&gt;</code>',
              'CONDOP'         : '<code>?</code>',
              'HASHCONDOP'     : '<code>#?</code>',
              'LPAREN'         : '<code>(</code>',
              'RPAREN'         : '<code>)</code>',
              'LBRACKET'       : '<code>[</code>',
              'RBRACKET'       : '<code>]</code>',
              'LBRACE'         : '<code>{</code>',
              'RBRACE'         : '<code>}</code>',
              'COMMA'          : '<code>,</code>',
              'PERIOD'         : '<code>.</code>',
              'SEMI'           : '<code>;</code>',
              'COLON'          : '<code>:</code>',
              'HASHCOLON'      : '<code>#:</code>',
              'AT'             : '<code>@</code>',
              'DOLLAR'         : '<code>$</code>',
              'DOTDOT'         : '<code>..</code>',
              'HASH'           : '<code>#</code>',
              'CBLOCK'         : '<code>%{ ... %}</code>',
              'HASHIF'         : '<code>#if</code>',
              'HASHELSE'       : '<code>#else</code>',
              'HASHSELECT'     : '<code>#select</code>',
              'HASHFOREACH'    : '<code>#foreach</code>',
              'ID'             : '<i>identifier</i>',
              'CCONST'         : '<i>char-literal</i>',
              'SCONST'         : '<i>string-literal</i>',
              'ICONST'         : '<i>integer-literal</i>',
              'HCONST'         : '<i>hex-literal</i>',
              'BCONST'         : '<i>binary-literal</i>',
              'FCONST'         : '<i>float-literal</i>',
              'ELLIPSIS'       : '<code>...</code>',
              '<empty>'        : '&lt;empty&gt;'
             }

for k in (reserved_idents
          + tuple(keywords_dml14.values())
          + tuple(keywords_dml12.values())):
    terminals[k] = '<code>%s</code>' % k.lower()

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
