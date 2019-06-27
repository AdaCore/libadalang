from __future__ import absolute_import, division, print_function

import collections
import os.path
import xml.etree.ElementTree as etree


StackFrame = collections.namedtuple('StackFrame', 'ip obj fn dir file line')
Error = collections.namedtuple('Error', 'message stack')


class Valgrind(object):
    """
    Helper to wrap the logic of memory checking using Valgrind.
    """

    def __init__(self, testsuite_dir, tmp_dir, suppression_file=None):
        self.testsuite_dir = testsuite_dir
        self.tmp_dir = tmp_dir
        self.suppression_file = suppression_file

    @property
    def report_file(self):
        return os.path.join(self.tmp_dir, 'valgrind-report.xml')

    def wrap_argv(self, argv):
        result = ['valgrind', '--xml=yes',
                  '--xml-file={}'.format(self.report_file),
                  '--suppressions={}'.format(os.path.join(
                      self.testsuite_dir, 'valgrind-suppressions.txt',
                  )),
                  '--leak-check=full']

        if self.suppression_file:
            result.append('--suppressions={}'.format(self.suppression_file))

        result.extend(argv)
        return result

    def parse_report(self):
        errors = []
        with open(self.report_file, 'r') as f:
            xml_root = etree.parse(f).getroot()

        for elt in xml_root:
            if elt.tag == 'error':
                what_elt = get_child(elt, 'xwhat')
                if what_elt is None:
                    what_elt = get_child(elt, 'what')
                message = ('Unknown reason'
                           if what_elt is None else
                           get_text_in_child(what_elt, 'text'))
                stack = []
                for frame in get_child(elt, 'stack'):
                    assert frame.tag == 'frame'
                    stack.append(StackFrame(
                        get_text_in_child(frame, 'ip'),
                        get_text_in_child(frame, 'obj'),
                        get_text_in_child(frame, 'fn'),
                        get_text_in_child(frame, 'dir'),
                        get_text_in_child(frame, 'file'),
                        get_text_in_child(frame, 'line'),
                    ))
                errors.append(Error(message, tuple(stack)))
        return errors

    @classmethod
    def format_report(cls, errors):
        result = []
        for error in errors:
            for frame in error.stack:
                result.append('  {}'.format(format_stack_frame(frame)))
            result.append(str(error.message))
            result.append('')
        return '\n'.join(result)


def format_stack_frame(frame):
    """Format a stack frame for pretty-printing."""
    result = ['At {}'.format(frame.ip)]
    if frame.fn:
        result.append(', in {}'.format(frame.fn))

    if frame.dir and frame.file and frame.line:
        result.append(', from {}:{}'.format(
            os.path.join(frame.dir, frame.file), frame.line
        ))
    else:
        result.append(', from {}'.format(frame.obj))

    return ''.join(result)


def get_child(node, tag):
    """
    Return the first child in `node` that has the given tag, or None if there
    is none.
    """
    for child in node:
        if child.tag == tag:
            return child
    return None


def get_text_in_child(node, tag):
    """
    Return the text contained in the node returned by `get_child`, or none
    if there is none.
    """
    child = get_child(node, tag)
    return None if child is None else child.text
