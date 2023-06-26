# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Convert .md files tailored for dodoc, into files suitable for
# publication on a github wiki

import sys
from pathlib import Path
import re
import json
import validate_md_links
import tarfile
from io import BytesIO

(toc, outfile, package_specs, *dirs) = sys.argv[1:]

toc = json.loads(Path(toc).read_text())
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
  © 2021-2023 Intel Corporation
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
    bodies.append((filename, body))

def add_to_tar(tf, path, contents):
    ti = tarfile.TarInfo(name=path)
    contents = contents.encode('utf-8')
    ti.size = len(contents)
    tf.addfile(ti, BytesIO(contents))

# Detect the current Simics version.
# Currently this only works when run as part of a Simics build.
specs = json.loads(Path(package_specs).read_bytes())
[base_pkg] = [pkg for pkg in (specs if isinstance(specs, list)
                              else specs.values())
              if pkg['package-name'] == 'Simics-Base'
              and pkg['host'] == 'linux64']
version = base_pkg['version']

with tarfile.open(outfile, "w:gz") as tgz:
    for (path, body) in bodies:
        for match in reversed(list(link_re.finditer(body))):
            if match[1].startswith('https://'):
                continue
            start = match.start(1)
            end = match.end(1)
            if end > start:
                body = body[:start] + filename_map[match[1]] + body[end:]
        add_to_tar(tgz, path, copyright + body)

    head = [copyright,
            f'This is the reference manual of DML 1.4, as of Simics {version}, '
            'converted to GitHub markdown.']
    add_to_tar(tgz, 'DML-1.4-Reference-Manual.md',
               '\n'.join(head + frontpage_links))
