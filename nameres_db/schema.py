from __future__ import absolute_import, division, print_function

from datetime import date
from funcy import memoize
import pony.orm as P
import re

db = P.Database()


@classmethod
def get_or_create(cls, **kwargs):
    r = cls.get(**kwargs)
    if r is None:
        return cls(**kwargs), True
    else:
        return r, False

db.Entity.get_or_create = get_or_create


def insert_or_ignore(entity, **args):
    items = args.items()
    keys = [k for k, v in items]

    return db.execute("insert or replace into {} ({}) values ({})".format(
        entity.__name__,
        ", ".join(keys),
        ", ".join("${}".format(k) for k in keys),
    ), globals={}, locals=args).lastrowid


class Project(db.Entity):
    name = P.Required(str, unique=True)
    files = P.Set('File')

    @memoize
    def stats(self):
        print("In stats")
        ret = stats(self)
        print("LOL")
        print("Ret = ", ret)
        return ret

    @property
    def nb_failures(self):
        return self.stats()['nb_failures']

    @property
    def nb_successes(self):
        return self.stats()['nb_successes']


class File(db.Entity):
    full_path = P.Required(str, unique=True)
    project = P.Required(Project)
    nodes = P.Set('Node')


class RunId(db.Entity):
    date = P.Required(date)
    resolutions = P.Set('NodeResolution')


class Node(db.Entity):
    file = P.Required(File)
    start_line = P.Required(int)
    start_column = P.Required(int)
    end_line = P.Required(int)
    end_column = P.Required(int)
    resolutions = P.Set('NodeResolution')
    P.composite_key(file, start_line, start_column, end_line, end_column)

    @staticmethod
    def parse_sloc_dict(strn):
        return dict(
            zip(['start_line', 'start_column', 'end_line', 'end_column'],
                re.split(r"\-|:", strn))
        )


class NodeResolution(db.Entity):
    node = P.Required(Node)
    run_id = P.Required(RunId)
    success = P.Required(bool)
    exception_message = P.Optional(str)
    traceback = P.Optional(str)
    P.composite_key(node, run_id)


def init_db():
    db.bind(provider='sqlite', filename='nameres.db', create_db=True)
    db.generate_mapping(create_tables=True)


def stats(project=None, run_id=None):
    if not run_id:
        run_id = P.max(r.id for r in RunId)

    fail = P.select(n for n in NodeResolution
                    if not n.success and n.run_id.id == run_id)
    success = P.select(n for n in NodeResolution
                       if n.success and n.run_id.id == run_id)

    if project:
        fail = fail.filter(lambda n: n.node.file.project == project)
        success = success.filter(lambda n: n.node.file.project == project)

    nb_failures, nb_successes = P.count(fail), P.count(success),
    nb_total = nb_failures + nb_successes

    return {
        'nb_failures': nb_failures,
        'nb_successes': nb_successes,
        'failures_pct': nb_failures / float(nb_total) * 100,
        'successes_pct': nb_successes / float(nb_total) * 100
    }


def failures_by_exception(run_id=None):
    if not run_id:
        run_id = P.max(r.id for r in RunId)
    cur = db.get_connection().cursor()
    return cur.execute("""
        select exception_message, count(*)
        from NodeResolution
        where success = 0
              and run_id = ?
        group by exception_message order by count(*) desc
    """, [run_id]).fetchall()


def failures_by_traceback(run_id=None):
    if not run_id:
        run_id = P.max(r for r in RunId)
    cur = db.get_connection().cursor()
    return P.select(n for n in NodeResolution
                    if not n.success
                    and run_id == run_id)
    return cur.execute("""
        select traceback, exception_message, count(*)
        from NodeResolution
        where success = 0
              and run_id = ?
        group by traceback order by count(*) desc
    """, [run_id]).fetchall()
