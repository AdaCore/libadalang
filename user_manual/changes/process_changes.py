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


@memoize
def json_schema():
    with open('entry_schema.yaml') as f:
        schema = f.read()
    return yaml.safe_load(schema)


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


def header(title, header_char):
    """
    Format a header.
    """
    return '{}\n{}\n'.format(title, header_char * len(title))


def print_entry(entry, show_date=False):
    """
    Print one entry as RST.

    :param bool show_date: Whether to show the date.
    """
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

    print(header(entry['title'].strip(), '='))
    print(entry['description'])
    print(field('tn', entry['tn']))
    print(field('apis', format_apis(entry.get('apis'))))
    if show_date:
        print(field('date', entry['date']))
    print()


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


def rst(entries, args):
    """
    Print RST for all entries, structured by change type, with headers for each
    change type.
    """
    for change_type in types_to_header.keys():
        header_chunk = types_to_header[change_type]

        filtered_entries = sorted(
            (e for e in entries if e['type'] == change_type),
            key=lambda e: e['date'], reverse=True
        )

        if filtered_entries == []:
            continue

        print(header('Libadalang API {}'.format(header_chunk), '#'))

        for entry in filtered_entries:
            print_entry(entry, args.show_date)


def raw(entries, args):
    """
    Print all entries raw.
    """
    import pprint
    pp = pprint.PrettyPrinter(indent=4)
    for entry in entries:
        pp.pprint(entry)


def validate(entries, args):
    """
    Validate all yaml entries.
    """
    def strip_title(title):
        return title.replace('``', '')

    # jsonschema validation happens as part of the iterator
    for entry in entries:
        if len(strip_title(entry['title'])) > 52:
            assert entry.get('short_title'), (
                'Entry {}: stripped title is more than 52 chars long, and no'
                ' short_title key'.format(entry['tn'])
            )


if __name__ == '__main__':
    os.chdir(P.dirname(P.abspath(__file__)))
    parser = A.ArgumentParser(description=__doc__)

    parser.add_argument(
        "--filter", help="Python expression to filter the entries. "
        "`e` designates the entry"
    )

    subparsers = parser.add_subparsers()

    def create_command(func, name=None):
        def wrapper(args):
            if args.filter:
                import datetime
                globs = {'Date': datetime.date}
                direct_fields = [
                    'title', 'apis', 'description', 'tn', 'type', 'date'
                ]
                filter_fn = eval(
                    "lambda e, {}: {}".format(", ".join(direct_fields),
                                              args.filter),
                    globs
                )
                entries = [e for e in all_entries()
                           if filter_fn(e, *[e.get(f, None)
                                             for f in direct_fields])]
            else:
                entries = [e for e in all_entries()]
            func(entries, args)

        subp = subparsers.add_parser(
            name or func.__name__.replace("_", "-"), help=func.__doc__
        )
        subp.set_defaults(func=wrapper)
        return subp

    rst_cmd = create_command(rst)
    rst_cmd.add_argument('--show-date', action='store_true')

    create_command(raw)
    create_command(validate)

    args = parser.parse_args()
    args.func(args)
