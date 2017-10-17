#! /usr/bin/env python

"""
Utility script to run name resolution on a bunch of Ada files, and help analyze
the results.
"""

from __future__ import print_function, absolute_import, division

import click
from collections import defaultdict
import cPickle
from funcy import split_by, partition_by, cat, memoize, pairwise, chunks
from glob import glob
from IPython import embed
import os
import progressbar
import Queue
import subprocess
from tempfile import NamedTemporaryFile
from threading import Thread, Event
import time

from langkit.utils import col, Colors


def load_or_create(file_name, constructor):
    if os.path.isfile(file_name):
        with open(file_name) as f:
            return cPickle.load(f)
    else:
        return constructor()


def dump_to(obj, file_name):
    with open(file_name, "w") as f:
        cPickle.dump(obj, f)


def pmap(fn, collection, nb_threads=10):
    """
    Apply fn to each item in collection, returns a collection containing the
    results. Uses threads to do the processing.

    NOTES:
    - Since python uses a GIL, you won't get real parallelism except if your
      underlying function spawns processes (which is our case in the test
      driver).
    - Order is not preserved by that algorithm, so it is not strictly a
      parallel map.
    - Items from collection are all consumed at once at the beginning to
      simplify the algorithm.
    """

    # Create an in queue and an out queue. The in queue will contain all the
    # data to be processed by the worker threads. The out queue will contain
    # the results.
    in_queue, out_queue = Queue.Queue(), Queue.Queue()

    # Fill in the in queue with the items to process
    map(in_queue.put, collection)

    e = Event()
    e.set()

    def consume():
        """
        Consumer function to be executed by all threads. Consume one item,
        process it with fn, and put it in the out queue. Stop the thread when
        the queue is empty.
        """
        while e.is_set():
            try:
                out_queue.put(fn(in_queue.get_nowait()))
            except Queue.Empty:
                return

    # Create the worker threads and start them
    threads = [Thread(target=consume) for _ in range(nb_threads)]
    map(Thread.start, threads)

    try:
        while True:
            # If there is no item left in the in queue, wait for the threads to
            # end and return the elements remaining in the out queue.
            if in_queue.empty():
                while any(t.is_alive() for t in threads):
                    if not out_queue.empty():
                        yield out_queue.get()
                while not out_queue.empty():
                    yield out_queue.get()
                return
            # If the in queue still contains elements, just return the results
            # as they come.
            else:
                yield out_queue.get()
    except KeyboardInterrupt:
        print("Terminating threads")
        e.clear()
        map(Thread.join, threads)


def memoized_property(f):
    return property(memoize(f))


class Result(object):
    def __init__(self, lines, text):
        import re
        self.lines = lines
        self.text = text
        self.node = re.findall(
            "for node (.*?)$", text, re.MULTILINE
        )[0]

    @property
    def lineno(self):
        return self.node.split(" ")[1].split(":")[0]

    @staticmethod
    def construct(file_result, lines):
        text = "\n".join(lines)
        if "Resolution failed" in text:
            result = Failure(lines, text)
        else:
            result = Success(lines, text)
        result.file_result = file_result
        return result

    def __repr__(self):
        return "<{} {} {}>".format(
            self.__class__.__name__,
            self.file_result.file_name,
            self.node
        )


class Success(Result):
    pass


class Failure(Result):
    def __init__(self, lines, text):
        super(Failure, self).__init__(lines, text)
        self.exception = None

        if "exception" in self.text:
            self.exception = [l for l in lines if l.startswith('>')][0]
            self.traceback = split_by(lambda l: l != "Traceback:", lines)[1]

    def open_failure(self):
        text_file = NamedTemporaryFile()
        text_file.write(self.text)
        print(self.text)
        text_file.close()

        editor = os.environ.get('EDITOR', 'vim')

        subprocess.check_call([
            editor, "+{}".format(self.lineno), self.file_result.file_path
        ])

    def debug_nameres(self):
        self.file_result.rerun_nameres(True, ["-L{}".format(self.lineno)])


class FileResult(object):
    def __init__(self, file_name, dir):
        self.file_name = file_name
        self.dir = dir
        self.successes = []
        self.failures = []
        self.has_crashed = False

    def add(self, result):
        if isinstance(result, Success):
            self.successes.append(result)
        else:
            self.failures.append(result)

    @memoized_property
    def is_success(self):
        return len(self.failures) == 0

    @memoized_property
    def exceptions(self):
        return [f for f in self.failures if f.exception]

    def __repr__(self):
        return "<FileResult {} failures:{}>".format(
            self.file_name, len(self.failures)
        )

    @memoized_property
    def file_path(self):
        return os.path.join(self.dir, self.file_name)

    @staticmethod
    def nameres_files(dir, files, debug=False, project="", extra_args=[]):

        if len(files) == 1:
            print("Analyzing file {}".format(files[0]))

        project_flag = (
            "-P{}".format(project) if project else "--with-default-project"
        )
        extra_args = list(extra_args)
        try:
            args = (
                ["nameres", project_flag, '--all']
                + (['--debug'] if debug else [])
                + list(extra_args) + files
            )
            out = (subprocess.check_call
                   if debug else subprocess.check_output)(args, cwd=dir)
            if debug:
                return
            results = []
            for res in out.split("Analyzing ")[1:]:
                file_name, _, _, content = res.split("\n", 3)
                content = content.strip()
                file_result = FileResult(file_name, dir)
                content = list(pairwise(partition_by(
                    lambda l: 'Resolving xrefs' in l, content.splitlines())
                ))
                for header, res_lines in content:
                    file_result.add(
                        Result.construct(file_result, header + res_lines)
                    )
                results.append(file_result)
            return results
        except subprocess.CalledProcessError:
            print("Resolution crashed.")
            print("Command line: {}".format(" ".join(args)))
            return []
        except:
            return []
        finally:
            if debug:
                print("Command line: {}".format(" ".join(args)))

    def rerun_nameres(self, debug=False, extra_args=[]):
        extra_args = list(extra_args)
        res = self.nameres_files(self.dir, [self.file_name],
                                 debug=debug, extra_args=extra_args)
        if res:
            return res[0]


class Results(object):
    def __init__(self):
        self.successes = []
        self.failures = []
        self.crashes = []
        self.save = True

    def __len__(self):
        return len(self.successes) + len(self.failures)

    def add(self, results):
        for result in results:
            if result.is_success:
                self.successes.append(result)
            elif result.has_crashed:
                self.crashes.append(result)
            else:
                self.failures.append(result)

    @memoized_property
    def individual_successes(self):
        return cat(s.successes for s in self.successes)

    @memoized_property
    def individual_failures(self):
        return cat(s.failures for s in self.failures)

    @memoized_property
    def aggregated_exceptions(self):
        aggregated_exceptions = defaultdict(list)
        for failure in self.failures:
            for exc in failure.exceptions:
                aggregated_exceptions[exc.exception].append(exc)
        return aggregated_exceptions

    @memoized_property
    def exceptions_to_files(self):
        exceptions = defaultdict(set)
        for failure in self.failures:
            for exc in failure.exceptions:
                exceptions[exc.exception].add(failure.file_name)
        return sorted(exceptions.items(), key=lambda (k, v): len(v))

    def get_failure(self, filename):
        return [f for f in self.failures if f.file_name == filename][0]


@click.command()
@click.argument('dirs', nargs=-1)
@click.option('-j', default=1)
@click.option('--chunk-size', default=100)
@click.option('--nores', is_flag=True)
@click.option('--project', default="")
@click.option('--extra-arg', '-E', multiple=True)
def main(dirs, j, chunk_size, nores, project, extra_arg):

    print("NAMERES ARGS :::: ", extra_arg)

    print("Loading old results ..")
    prev_results = load_or_create("results_file", lambda: None)
    if nores:
        results = prev_results
        embed()
        return

    progress_file = open("progress_file", "a")

    def prn(*args):
        print(*args)
        print(*args, file=progress_file)

    results = Results()
    files = []
    for dir in dirs:
        dir_files = chunks(
            chunk_size,
            map(os.path.basename, sorted(glob('{}/*.ad?'.format(dir))))
        )
        files += [(dir, fs) for fs in dir_files]

    raw_results = pmap(
        lambda (dir, f): FileResult.nameres_files(
            dir, f, project=project, extra_args=extra_arg
        ),
        files, nb_threads=j
    )

    total_nb_files = sum(len(fs[1]) for fs in files)

    bar = progressbar.ProgressBar(max_value=total_nb_files)
    for subresults in raw_results:
        results.add(subresults)
        bar.update(len(results))

    prn("Report for {}:".format(time.strftime("%Y-%m-%d %Hh%M")))

    successes = len(results.successes)
    failures = len(results.failures)
    nb_files = successes + failures

    prn("Number of successful tests: {}".format(col(successes, Colors.GREEN)))
    prn("Number of failures: {}".format(col(failures, Colors.RED)))
    prn(
        "Number of crashes: {}".format(col(len(results.crashes), Colors.RED))
    )

    prn("Percentage of files passing: {:.2f}%".format(
        float(successes) / float(nb_files) * 100
    ))

    ind_successes = len(results.individual_successes)
    ind_failures = len(results.individual_failures)
    total_xrefs = ind_successes + ind_failures

    prn("Number of individual successes: {}".format(
        col(ind_successes, Colors.GREEN)
    ))
    prn("Number of individual failures: {}".format(
        col(ind_failures, Colors.RED)
    ))

    prn("Percentage of successes: {:.2f}%".format(
        float(ind_successes) / float(total_xrefs) * 100
    ))

    prn("Crashes: ")
    for crash in results.crashes:
        prn("    {}".format(crash.file_name))

    def print_exceptions():
        print("Exceptions:")
        for msg, files in results.exceptions_to_files:
            print("{}: {}".format(len(files), msg))

    previously_passing = set()
    previously_failing = set()
    if prev_results is not None:
        prn(col("Newly passing tests:", Colors.GREEN))
        previously_passing = set(r.file_name for r in prev_results.successes)
        now_passing = set(r.file_name for r in results.successes)
        for f in (now_passing - previously_passing):
            prn("    {}".format(f))

        prn(col("Newly failing tests:", Colors.RED))
        previously_failing = set(r.file_name for r in prev_results.failures)
        now_failing = set(r.file_name for r in results.failures)
        for f in (now_failing - previously_failing):
            prn("   {}".format(f))

    embed()

    if (previously_passing or previously_failing) and results.save:
        dump_to(results, "results_file")

    progress_file.close()


main()
