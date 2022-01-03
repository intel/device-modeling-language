# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
[path_to_dml, outfile] = sys.argv[1:]
sys.path.append(path_to_dml)

from dml import messages
from dml.messages import PortingMessage

portings = []

for n in dir(messages):
    o = getattr(messages, n)
    if (isinstance(o, type) and issubclass(o, PortingMessage)
        and o is not PortingMessage):
        portings.append(o)

portings.sort(key=lambda x: x.__name__)

with open(outfile, 'w') as f:
    f.write("# Language differences handled by the port-dml script\n\n")

    f.write("<dl>\n")
    for m in portings:
        assert m.__doc__
        f.write(f"  <dt>{m.__name__}</dt>\n")
        doc = '\n'.join(line[4:] if line.startswith('    ') else line
                        for line in m.__doc__.strip().splitlines())
        f.write(f"  <dd>\n\n{doc}\n</dd>\n")
    f.write("</dl>\n")
