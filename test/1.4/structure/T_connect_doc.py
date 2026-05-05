# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0
import stest

attr_doc = {name: doc for (name, _, doc, _) in obj.attributes}

stest.expect_equal(attr_doc['abcd'], '''\
desc

Required interfaces: <tt>a</tt>, <tt>b</tt>

Optional interfaces: <tt>c</tt>, <tt>d</tt>''')

stest.expect_equal(attr_doc['ac'], '''\
doc

Required interface: <tt>a</tt>

Optional interface: <tt>c</tt>''')

stest.expect_equal(attr_doc['none'], 'taken')
