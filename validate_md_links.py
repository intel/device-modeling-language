# © 2021-2023 Intel Corporation
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

third_party_trademarks = {'synopsys', 'coverity'}

third_party_trademark_re = re.compile(
    r'(?i:(' + '|'.join(third_party_trademarks)
    + r'))([*®™]|&reg;|&trade;|&copy;|\\\*)?')

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

class Error(Exception): pass

def validate_link(link, path, dirs, md_files):
    if link.count('#') == 0 and link.endswith('.html'):
        (f, anchor) = (link, '')
    elif link.count('#') == 1:
        (f, anchor) = link.split('#')
    elif link.startswith('https://'):
        return
    else:
        raise Error(f"malformed link, expected '#': {link}")
    if f:
        assert f.endswith('.html')
        f = f[:-5] + '.md'
        f = lookup(Path(f), dirs)
        assert f in md_files
    else:
        f = path
    if anchor not in anchors(md_files[f]):
        raise Error(f"broken link: {link}")

def main():
    (toc, *dirs) = sys.argv[1:]
    toc = json.loads(Path(toc).read_text())
    dirs = list(map(Path, dirs))

    md_files = {f: f.read_text() for f in toc_md_files(toc, dirs)}

    ok = True
    for path in md_files:
        for match in link_re.finditer(md_files[path]):
            [link] = match.groups()
            try:
                validate_link(link, path, dirs, md_files)
            except Error as message:
                line = md_files[path][:match.start()].count('\n') + 1
                sys.stderr.write(f'{path}:{line}: error: {message}\n')
                ok = False

        third_party_trademarks_referenced = {}
        third_party_trademarks_annotated = set()
        def check_third_party_trademarks():
            for (tm, line) in third_party_trademarks_referenced.items():
                if tm not in third_party_trademarks_annotated:
                    sys.stderr.write(
                        f"{path}:{line+1}: error: third party trademark "
                        + f"'{tm}' never annotated with '*' in this section\n")
                    nonlocal ok
                    ok = False
            third_party_trademarks_referenced.clear()
            third_party_trademarks_annotated.clear()

        for (i, line) in enumerate(md_files[path].split('\n')):
            if ' -- ' in f' {line} ':
                sys.stderr.write(
                    f'{path}:{i+1}: error: replace -- with &mdash;\n')
                ok = False

            # A third party trademark must be annotated at least once per
            # (sub)section
            if line.startswith('#'):
                check_third_party_trademarks()
            for match in third_party_trademark_re.finditer(line):
                annotated = match.group(2) in {'*', '\\*'}

                tm = match.group(1).lower()
                third_party_trademarks_referenced.setdefault(tm, i)
                if annotated:
                    third_party_trademarks_annotated.add(tm)

        check_third_party_trademarks()

    sys.exit(0 if ok else 1)

if __name__ == '__main__':
    main()
