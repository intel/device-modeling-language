# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Convert .md files tailored for dodoc, into files suitable for
# publication on a github wiki

import sys
from pathlib import Path
import re
import json
import functools
import traceback
import validate_md_links

(toc, outdir, *dirs) = sys.argv[1:]

toc = json.loads(Path(toc).read_text())
outdir = Path(outdir)
dirs = list(map(Path, dirs))

def toc_md_files(toc, indices):
    yield (validate_md_links.lookup(toc['file'], dirs), indices)
    chapter = 1
    appendix = ord('A')
    for child in toc.get('children', []):
        if child.get('appendix'):
            i = chr(appendix)
            appendix += 1
        else:
            i = str(chapter)
            chapter += 1
        yield from toc_md_files(child, indices + [i])

link_re = re.compile(r'\[.*?\]\(([^#)]*).*?\)', re.DOTALL)

assert link_re.findall('[blah\nblah](foo.html#xyz) [glurp](#bar)') == [
    'foo.html', '']

title_re = re.compile('(?:<!--.*?-->)?\\s*?# ([^\n]*)', re.DOTALL)
assert title_re.match('''<!--
 linjal passare
-->

# skruv spik hammare

tving vattenpass
''').groups() == ('skruv spik hammare',)

copyright = '''<!--
  © 2021-2022 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->
'''

frontpage_links = []

filename_map = {}
bodies = []
for (path, indices) in toc_md_files(toc, []):
    contents = path.read_text()
    title_match = title_re.match(contents)
    title = f"{'.'.join(map(str, indices))}. {title_match.group(1)}"
    basename = title.replace(' ', '-')
    assert validate_md_links.valid_chars.issuperset(basename), path
    body = contents[title_match.end():].lstrip()
    if not indices:
        assert not body
        continue
    frontpage_links.append(
        '#' * len(indices) + f'# [{title}]({basename})')
    filename = basename + '.md'
    filename_map[path.with_suffix('.html').name] = basename
    bodies.append((outdir / filename, body))

for (path, body) in bodies:
    for match in reversed(list(link_re.finditer(body))):
        start = match.start(1)
        end = match.end(1)
        if end > start:
            body = body[:start] + filename_map[match[1]] + body[end:]
    path.write_text(copyright + body)

# When built in a Simics tree, this will detect the current Simics version
specs = outdir / '..' / '..' / '..' / '..' / 'package-specs.json'
pkgs = json.loads(specs.read_bytes()) if specs.is_file() else {}
version = pkgs.get('Simics-Base-linux64', {}).get('version', '6.0.xxx')

head = [copyright,
        f'This is the reference manual of DML 1.4, as of Simics {version}, '
        'converted to GitHub markdown.']
(outdir / 'DML-1.4-Reference-Manual.md').write_text(
    '\n'.join(head + frontpage_links))
