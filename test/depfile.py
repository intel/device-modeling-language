# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

def parse_depfile(f):
    lines = []
    for line in f:
        line = line.rstrip()
        if lines and lines[-1] and lines[-1][-1] == '\\':
            lines[-1] = lines[-1][:-1] + ' ' + line
        else:
            lines.append(line)
    target_prereqs = {}
    for line in filter(None, lines):
        # finding : is awkward because of Windows c:\
        # MingW gcc 10.3 dep output path has inconsistent case
        (targets, prereqs) = (line + ' ').replace('d:', 'D:').split(': ')
        for target in targets.split():
            target_prereqs.setdefault(target, set()).update(prereqs.split())
    return target_prereqs


# unit test depfile parsing
assert parse_depfile([
    s + '\n' for s in [
        'a \\ ', ' b: c\\', 'd', ' ', 'a : e', 'f:']]) == {
            'a': {'c', 'd', 'e'},
            'b': {'c', 'd'},
            'f': set()}
