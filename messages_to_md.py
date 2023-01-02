# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
import argparse

def fmt_message(err):
    msg = err.fmt

    # Keep only the first line
    msg = msg.split('\n')[0]

    # Escape markup
    msg = msg.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')

    # Replace placeholders
    msg = msg.replace('%s', '...')
    msg = msg.replace('%d', '<i>N</i>')

    return msg

def extract_messages(sys_path):
    sys.path.append(sys_path)
    from dml import messages
    from dml.messages import DMLError, DMLWarning

    errors = []
    warnings = []

    for n in dir(messages):
        o = getattr(messages, n)
        #sys.stderr.write("%s: %r\n" % (n, o))
        if isinstance(o, type):
            if issubclass(o, DMLError) and o is not DMLError:
                errors.append(o)
            elif issubclass(o, DMLWarning) and o is not DMLWarning:
                warnings.append(o)

    errors.sort(key=lambda x: x.fmt)
    warnings.sort(key=lambda x: x.fmt)
    return (warnings, errors)

def print_message_table(f, messages):
    f.write("<dl>\n")
    for m in messages:
        assert m.__doc__
        f.write(f"  <dt><b>\n\n{fmt_message(m)} [{m.__name__}]</b></dt>\n")
        doc = '\n'.join(line[4:] if line.startswith('    ') else line
                        for line in m.__doc__.strip().splitlines())
        f.write(f"  <dd>\n\n{doc}\n</dd>\n")
    f.write("</dl>\n")

def print_messages(f, warnings, errors):
    f.write("""
# Messages

The following sections list the warnings and error messages from
`dmlc`, with some clarifications.

## Warning Messages

The messages are listed in alphabetical order; the corresponding tags
are shown within brackets, e.g., `[WNDOC]`.

""")

    print_message_table(f, warnings)

    f.write("""

## Error Messages

The messages are listed in alphabetical order; the corresponding tags
are shown within brackets, e.g., `[ENBOOL]`.

""")

    print_message_table(f, errors)

def matches_version(message, target):
    ver = message.version
    if not ver:
        return True
    if isinstance(ver, str):
        ver = (ver,)
    return target in ver

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('path')
    parser.add_argument('version')
    parser.add_argument('outfile')
    args = parser.parse_args()
    (warnings, errors) = extract_messages(args.path)
    with open(args.outfile, 'w') as f:
        print_messages(
            f, [w for w in warnings if matches_version(w, args.version)],
            [e for e in errors if matches_version(e, args.version)])
