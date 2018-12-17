"""
"""

from __future__ import absolute_import, division, print_function

import argparse as A
from datetime import date
from funcy import memoize
import json
from os import path as P
from pony.orm import db_session
import schema as S

parser = A.ArgumentParser()
parser.add_argument(
    'data_file',
    help='File containing the JSON data to populate the database',
    type=str
)
parser.add_argument(
    'project',
    help='Unique name of the project this data belongs to',
    type=str
)
parser.add_argument(
    '--run-id',
    help='Int id for the run to which we want to associate '
         'results. If not specified, create a new run id',
    type=int,
    default=-1
)


class Obj(object):
    def __init__(self, d):
        self.__dict__ = d


def parse_sloc(strn):
    import re
    return re.split(r"\-|:", strn)


@memoize
def create_or_get_file(db_project, path):
    print("Processing file {}".format(path))
    f, _ = S.File.get_or_create(full_path=P.abspath(path), project=db_project)
    S.db.commit()
    return f


@db_session()
def main():
    args = parser.parse_args()
    db_project, _ = S.Project.get_or_create(name=args.project)

    print('Run id = ', args.run_id)
    if args.run_id == -1:
        run_id = S.RunId(date=date.today())
    else:
        run_id = S.RunId.get(id=args.run_id)
    S.db.commit()
    print(run_id, run_id.id)

    with open(args.data_file) as f:
        for line in f.readlines():
            o = json.loads(line)
            rec = Obj(o)
            if rec.kind == 'node_resolution':
                db_file = create_or_get_file(db_project, rec.file)
                cur = S.db.get_connection().cursor()
                bindings = [db_file.id] + parse_sloc(rec.sloc)
                cur.execute("""
                    insert or ignore
                    into Node (file, start_line, start_column,
                               end_line, end_column)
                    values (?, ?, ?, ?, ?)
                """, bindings)

                node_id = cur.execute("""
                    select id from Node
                    where file=?
                    and start_line=? and start_column=?
                    and end_line=? and end_column=?
                """, bindings).fetchone()[0]

                cur.execute("""
                insert or ignore into NodeResolution
                  (node, run_id, success, exception_message, traceback)
                values (?, ?, ?, ?, ?)
                """, [node_id, run_id.id, rec.success,
                      getattr(rec, 'exception_message', ''),
                      getattr(rec, 'exception_traceback', '')])

    print("Committing...")
    S.db.commit()
    if args.run_id == -1:
        print("Run id={}".format(run_id.id))

if __name__ == '__main__':
    S.init_db()
    main()
