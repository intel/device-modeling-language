# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
import re
from pathlib import Path

(infile, outfile) = sys.argv[1:]

comment_re = re.compile(r'/\*\*((?:[^*]|\*+[^/*])*)\*+/')

f = Path(infile).read_text()


def strip_max_common_indent(elem):
    n = min(len(line) - len(line.lstrip())
            for line in elem.split('\n') if line.strip())
    rex = re.compile(r'\n' + ' ' * n)
    return rex.sub('\n', elem)


elements = [strip_max_common_indent(e) for e in comment_re.findall(f)]

Path(outfile).write_text(''.join(elements))
