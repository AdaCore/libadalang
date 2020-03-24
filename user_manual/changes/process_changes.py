#! /usr/bin/env python

"""
This script is an entry point to process change files for Libadalang.
"""

from __future__ import absolute_import, division, print_function

import argparse as A
from collections import OrderedDict
from glob import glob
import os
import os.path as P
import sys
import yaml

from funcy import memoize


def print_err(*args, **kwargs):
    """
    Print a message on the standard error file.

    This forwards ``**args`` and ``**kwargs`` to the ``print`` function.
    """
    print(*args, file=sys.stderr, **kwargs)


# TODO: This is a workaround the fact that jsonschema is not available in
# production. Pending resolution of S905-005.
try:
    import jsonschema as J

    def validate_entry(tn, entry, no_schema=False):
        """
        Validate a yaml change entry.
        """
        try:
            J.validate(entry, json_schema())
        except J.ValidationError as e:
            print_err('Error when validating entry for {}'.format(tn))
            print_err(e)
            sys.exit(1)

except ImportError:
    print_err('WARNING: jsonschema is not available. Entries will not be'
              ' validated!')

    def validate_entry(tn, entry, no_schema=False):
        pass


def all_entries():
    """
    Iterator on all change entries. Each entry is a dict loaded from yaml
    containing the expected entry fields.

    This iterator will validate entries, e.g. make sure that the entries
    conform to the entry schema.

    :rtype: generator[dict]
    """

    entries_names = [f for f in glob('*.yaml') if f != 'entry_schema.yaml']
    for fname in entries_names:
        tn = fname.split('.')[0]
        with open(fname) as f:
            entry = yaml.safe_load(f)
            entry['tn'] = tn
            validate_entry(tn, entry)
            yield entry


types_to_header = OrderedDict((
    ('api-change', 'breaking changes'),
    ('new-feature', 'new features'),
    ('bugfix', 'bug fixes'),
))


def print_all_changes_rst():
    """
    Print RST on stdout for all the change entries for all change types.
    """

    for change_type in types_to_header.keys():
        print_rst(change_type)


def print_rst(change_type='api-change'):
    """
    Print RST on stdout for all the change entries for a given
    ``change_type``.
    """
    header_chunk = types_to_header[change_type]

    def header(title, header_char):
        """
        Format a header.
        """
        return '{}\n{}\n'.format(title, header_char * len(title))

    def field(name, value):
        """
        Format a field list entry.
        """
        return ':{}:\n    {}'.format(name, value)

    def format_apis(apis):
        """
        Format the API field content. If absent, return "all".
        """
        if apis:
            return ', '.join(apis)
        else:
            return 'all'

    entries = sorted((e for e in all_entries() if e['type'] == change_type),
                     key=lambda e: e['date'], reverse=True)

    if entries == []:
        return

    print(header('Libadalang API {}'.format(header_chunk), '#'))

    for entry in entries:
        print(header(entry['title'], '='))
        print(entry['description'])
        print(field('tn', entry['tn']))
        print(field('apis', format_apis(entry.get('apis'))))
        print()


@memoize
def json_schema():
    with open('entry_schema.yaml') as f:
        schema = f.read()
    return yaml.safe_load(schema)


def validate():
    """
    Validate all yaml entries.
    """
    def strip_title(title):
        return title.replace('``', '')

    # jsonschema validation happens as part of the iterator
    for entry in all_entries():
        if len(strip_title(entry['title'])) > 52:
            assert entry.get('short_title'), (
                'Entry {}: stripped title is more than 52 chars long, and no'
                ' short_title key'.format(entry['tn'])
            )


if __name__ == '__main__':
    os.chdir(P.dirname(P.abspath(__file__)))
    parser = A.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers()

    create_rst_parser = subparsers.add_parser(
        'create-rst', help=print_all_changes_rst.__doc__
    )
    create_rst_parser.set_defaults(func=lambda args: print_all_changes_rst())

    validate_parser = subparsers.add_parser('validate', help=validate.__doc__)
    validate_parser.set_defaults(func=lambda args: validate())

    args = parser.parse_args()
    args.func(args)
