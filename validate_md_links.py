# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
from pathlib import Path
import re
import json
import functools
import traceback

def lookup(f, dirs):
    for d in dirs:
        candidate = d / f
        if candidate.is_file():
            return candidate
    assert False, f

def toc_md_files(toc, dirs):
    yield lookup(toc['file'], dirs)
    for child in toc.get('children', []):
        yield from toc_md_files(child, dirs)

anchor_re = re.compile(r'<a id="(.*?)"')

assert anchor_re.findall('<a id="comparison-to-c"/>') == ['comparison-to-c']

def char_range(low, high):
    return map(chr, range(ord(low), ord(high) + 1))

valid_chars = set('- _.,')
valid_chars.update(char_range('0', '9'))
valid_chars.update(char_range('A', 'Z'))
valid_chars.update(char_range('a', 'z'))

@functools.lru_cache(None)
def anchors(text):
    anchors = {''}
    ambiguous_anchors = set()
    anchors.update(anchor_re.findall(text))
    for line in text.splitlines():
        # This also falsely creates anchors when a ``` line starts with #
        if line.startswith('#'):
            line = line.lstrip('#').strip()
            line = line.replace('\\_', '_')
            if not valid_chars.issuperset(line):
                # disallow references to titles with special characters
                continue
            key = line.lower().replace(' ', '-')
            if key in anchors:
                ambiguous_anchors.add(key)
            else:
                anchors.add(key)
    return anchors - ambiguous_anchors

link_re = re.compile(r'\[.*?\]\((.*?)\)', re.DOTALL)

assert link_re.findall('[blah\nblah](foo.html#xyz) [glurp](#bar)') == [
    'foo.html#xyz', '#bar']

def report(path, match, message):
    line = md_files[path][:match.start()].count('\n') + 1
    sys.stderr.write(f'{path}:{line}: error: {message}\n')

def main():
    (toc, *dirs) = sys.argv[1:]
    toc = json.loads(Path(toc).read_text())
    dirs = list(map(Path, dirs))

    md_files = {f: f.read_text() for f in toc_md_files(toc, dirs)}

    ok = True
    for path in md_files:
        for match in link_re.finditer(md_files[path]):
            [link] = match.groups()
            if link.count('#') == 0 and link.endswith('.html'):
                (f, anchor) = (link, '')
            elif link.count('#') == 1:
                (f, anchor) = link.split('#')
            else:
                report(path, match, f"malformed link, expected '#': {link}")
                ok = False
                continue
            if f:
                assert f.endswith('.html')
                f = f[:-5] + '.md'
                f = lookup(Path(f), dirs)
                assert f in md_files
            else:
                f = path
            if anchor not in anchors(md_files[f]):
                report(path, match, f"broken link: {link}")
                ok = False

    sys.exit(0 if ok else 1)

if __name__ == '__main__':
    main()
