#!/usr/bin/env python3

import argparse
import datetime
import glob
import re
import sys
from typing import Match


class Updater:
    """Namespace for copyright updating code."""

    copyright_re_list = [
        re.compile(pattern)
        for pattern in [
            # For Ada/C source code
            r"(?P<copyright>Copyright \(C\) (?P<years>[^\n]*), AdaCore)",

            # For Sphinx's conf.py
            r"(?P<copyright>copyright = u'(?P<years>[^']*), AdaCore')",
        ]
    ]
    """
    List of regular expressions that match the copyright notices to update.
    """

    single_year_re = re.compile(r"(\d{4})")
    year_range_re = re.compile(r"(\d{4})-(\d{4})")

    files = [
        "ada/copyright.py",
        "extensions/src/*.*",
        "langkit/langkit/support/*.*",
        "langkit/langkit/adasat/src/*.*",
        "langkit/dev_guide/conf.py",
        "user_manual/conf.py",
    ]
    """List of glob patterns for the files to update."""

    def __init__(self, year: int):
        """
        :param year: New year to include in copyright notices.
        """
        self.year = year

    def do_repl(self, m: Match[str]) -> str:
        """
        Update a copyright notice.

        :param m: Match object for a regular expression in
            ``Updater.copyright_re``.
        :return: The updated copyright notice (to include ``self.year``).
        """
        year_first: int
        year_last: int

        # Detect either a year range or a single year in the current copyright
        # notice.
        years_str = m.group("years")
        m2 = self.year_range_re.search(years_str)
        if m2:
            year_first = int(m2.group(1))
            year_last = int(m2.group(2))
        else:
            m2 = self.single_year_re.search(years_str)
            assert m2
            year_first = year_last = int(m2.group(1))

        # Create the new year range
        if year_first > year_last:
            raise ValueError("invalid year range: {m2.group(0)}")
        if self.year < year_first:
            year_first = self.year
        if year_last < self.year:
            year_last = self.year

        # Format it
        years_str = (
            str(year_first)
            if year_first == year_last else
            f"{year_first}-{year_last}"
        )

        # Return the updated copyright notice
        return "".join([
            m.string[m.start("copyright"):m.start("years")],
            years_str,
            m.string[m.end("years"):m.end("copyright")],
        ])

    def patch_file(self, filename: str) -> None:
        """
        Update copyright notices in the source file designated by ``filename``.
        """
        print("Patching", filename)
        with open(filename, "r") as f:
            contents = f.read()
        for copyright_re in self.copyright_re_list:
            contents = re.sub(copyright_re, self.do_repl, contents)
        with open(filename, "w") as f:
            f.write(contents)

    def patch_all_files(self) -> None:
        """
        Update copyright notices for all files designated by ``Updater.files``.
        """
        for pattern in self.files:
            for f in glob.glob(pattern):
                self.patch_file(f)


args_parser = argparse.ArgumentParser(
    description="Update copyright notices to include current year"
)
args_parser.add_argument(
    "--year", type=int, default=datetime.date.today().year,
    help="Year to include in copyright notices (default: current year)"
)
args_parser.add_argument(
    "files", nargs="*",
    help="Files to update. If none passed, look for usual suspects in the"
    " Libadalang/Langkit/AdaSAT repositories."
)


def main(args: argparse.Namespace) -> int:
    updater = Updater(args.year)

    # Process explicitly requested source files, Libadalang/Langkit/AdaSAT
    # sources otherwise.
    if args.files:
        for f in args.files:
            updater.patch_file(f)
    else:
        updater.patch_all_files()

    return 0


if __name__ == "__main__":
    sys.exit(main(args_parser.parse_args()))
