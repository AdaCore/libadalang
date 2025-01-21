#! /usr/bin/env python

import argparse
import os.path
import sys

import langkit.scripts.lkm


parser = argparse.ArgumentParser(
    description="Run a Libadalang testsuite"
)
parser.add_argument(
    "--testsuite",
    help="Which testsuite to run (public, internal or acats).",
    choices={"public", "internal", "acats"},
    default="public",
)
parser.add_argument(
    "--build-dir",
    help="Build directory for Libadalang.",
    default="build",
)
parser.add_argument("--build-mode", help="Build mode for Libadalang.")
parser.add_argument(
    "--disable-ocaml",
    help="Disable tests involving the OCaml API.",
    action="store_true",
)
parser.add_argument(
    "--disable-java",
     help="Disable tests involving the Java API.",
    action="store_true",
)
parser.add_argument(
    "--acats-dir",
    help='The path to the acats repository. By default, use "acats" in the'
    " current directory.",
    default="acats",
)


def main(argv: list[str] | None = None) -> None:
    args, unknown_args = parser.parse_known_args(argv)

    lal_dir = os.path.abspath(os.path.dirname(__file__))
    config_file = os.path.join(lal_dir, "langkit.yaml")
    build_dir = os.path.join(lal_dir, args.build_dir)

    # Arguments for "lkm run"
    lkm_argv = ["run", f"--build-dir={args.build_dir}"]

    # Arguments for the testsuite script itself
    script_argv = [sys.executable, "-c", config_file]

    if args.build_mode:
        lkm_argv.append(f"--build-mode={args.build_mode}")

    match args.testsuite:
        case "public" | "internal":
            script_argv += [
                os.path.join(lal_dir, "testsuite", "testsuite.py"),
                "--show-error-output",
                "-dtmp",
            ]

            if not args.disable_ocaml:
                script_argv += [
                    "--with-ocaml-bindings", os.path.join(build_dir, "ocaml")
                ]

            if not args.disable_java:
                script_argv += [
                    "--with-java-bindings", os.path.join(build_dir, "java")
                ]

            if args.testsuite != "internal":
                script_argv.append("--skip-internal")

        case "acats":
            acats_dir = os.path.abspath(args.acats_dir)
            script_argv += [
                os.path.join(acats_dir, "run_acats_test.py"),
                "--acats-dir",
                acats_dir,
                "--mode=libadalang",
            ]

        case _:
            assert False

    langkit.scripts.lkm.main([*lkm_argv, *script_argv, *unknown_args])


if __name__ == '__main__':
    main()
