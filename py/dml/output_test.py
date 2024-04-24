import unittest

from dml import output, logging

class TestOutput(unittest.TestCase):
    def test_linemark_validation(self):
        out = output.StrOutput()
        out.linemark(3, 'foo')
        out.out('foo')
        # ok, not beginning of line
        out.out('bar\n')
        out.linemark(4, 'foo')
        out.out('foo')
        out.out('bar\n')
        with self.assertRaisesRegex(logging.ICE, '.*missing linemark'):
            out.out('foo')
        out.linemark(5, 'foo')
        with self.assertRaisesRegex(logging.ICE, '.*missing linemark'):
            out.out('foo\nbar\n')
