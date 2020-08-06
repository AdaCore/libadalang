#! /usr/bin/env python

"""
This script is an entry point to process change files for Libadalang.
"""

from __future__ import absolute_import, division, print_function

import argparse as A
from collections import OrderedDict
import datetime
from glob import glob
import os
import os.path as P
import pprint
import sys
import tempfile
import webbrowser
import yaml


from docutils.core import publish_string
from funcy import memoize
import jsonschema


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


def validate_entry(tn, entry, no_schema=False):
    """
    Validate a yaml change entry.
    """
    try:
        jsonschema.validate(entry, json_schema())
    except jsonschema.ValidationError as e:
        print_err('Error when validating entry for {}'.format(tn))
        print_err(e)
        # We exit on validation errors because all the rest of the code might
        # fail.
        sys.exit(1)

    if len(entry['title'].replace("``", "")) > 52:
        st = entry.get('short_title')
        if not st or len(st) > 52:
            print_err(
                '{}: ERROR: stripped title is more than 52 chars long, and no'
                ' short_title key (or too long)'.format(entry['tn'])
            )


def header(title, header_char):
    """
    Format a header.
    """
    return '{}\n{}\n'.format(title, header_char * len(title))


def entry2rst(entry, show_date=False):
    """
    Print one entry as reST.

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

    out = []
    out.append(header(entry['title'].strip(), '='))
    out.append(entry['description'])
    out.append(field('tn', entry['tn']))
    out.append(field('apis', format_apis(entry.get('apis'))))
    if show_date:
        out.append(field('date', entry['date']))
    out.append('')

    return "\n".join(out)


def get_entries(*args):
    """
    Iterator on all change entries. Each entry is a dict loaded from yaml
    containing the expected entry fields.

    This iterator will validate entries, e.g. make sure that the entries
    conform to the entry schema.

    :param [str] *args: List of entry file names. If empty, return all the
        entries.
    :rtype: generator[dict]
    """

    if args:
        for a in args:
            if not a.endswith(".yaml") or not P.isfile(a):
                print("ERROR: not an entry file: {}".format(a))
        entries_names = args
    else:
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
    Print reST for all entries, structured by change type, with headers for
    each change type.
    """
    buffr = []

    # Generate one big reST document to hold all entries, grouped by change
    # type.
    for change_type in types_to_header.keys():
        # Collect all entries for this change type, more recent first
        filtered_entries = sorted(
            (e for e in entries if e['type'] == change_type),
            key=lambda e: e['date'], reverse=True
        )

        # No need to emit a section for this change type if it has no entry
        if filtered_entries == []:
            continue

        header_chunk = types_to_header[change_type]
        buffr.append(
            header('Libadalang API {}'.format(header_chunk), '#')
        )
        for entry in filtered_entries:
            entry_rst = entry2rst(entry, args.show_date)
            # Call for diagnostics
            publish_string(entry_rst, writer_name='html',
                           source_path=entry['tn'] + '.yaml')
            buffr.append(entry_rst)

    result = '\n'.join(buffr)

    # We always generate the HTML, even if we don't need a preview, so that we
    # check that the reST is correctly formatted.
    if not args.quiet:
        html = publish_string(result, writer_name='html')
        print(result)

    # If preview is requested, also write the result to a file and open a
    # browser to view it.
    if args.preview:
        fd, path = tempfile.mkstemp(suffix='.html')
        with os.fdopen(fd, 'w') as f:
            f.write(html)
        webbrowser.open(path)


def raw(entries, args):
    """
    Print all entries raw.
    """
    pp = pprint.PrettyPrinter(indent=4)
    for entry in entries:
        pp.pprint(entry)


if __name__ == '__main__':
    os.chdir(P.dirname(P.abspath(__file__)))
    parser = A.ArgumentParser(description=__doc__)

    parser.add_argument(
        "-F", "--filter", help="Python expression to filter the entries. "
        "`e` designates the entry"
    )

    subparsers = parser.add_subparsers()

    def create_command(func, name=None):
        def wrapper(args):
            if args.filter:
                globs = {'Date': datetime.date}
                direct_fields = [
                    'title', 'apis', 'description', 'tn', 'type', 'date'
                ]
                filter_fn = eval(
                    "lambda e, {}: {}".format(", ".join(direct_fields),
                                              args.filter),
                    globs
                )
                entries = [e for e in get_entries(*args.files)
                           if filter_fn(e, *[e.get(f, None)
                                             for f in direct_fields])]
            else:
                entries = [e for e in get_entries(*args.files)]
            func(entries, args)

        subp = subparsers.add_parser(
            name or func.__name__.replace("_", "-"), help=func.__doc__
        )
        subp.set_defaults(func=wrapper)
        subp.add_argument(
            'files', help='Entry files to process. All if not passed',
            type=str, nargs='*', metavar='F'
        )

        return subp

    rst_cmd = create_command(rst)
    rst_cmd.add_argument('--show-date', action='store_true')
    rst_cmd.add_argument(
        '--quiet', action='store_true',
        help="Generate and check the RST, but don't output it"
    )
    rst_cmd.add_argument('--preview', action='store_true')

    create_command(raw)

    args = parser.parse_args()
    args.func(args)
