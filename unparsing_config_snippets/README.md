# Unparsing configuration snippets

This directory contains unparsing configuration snippets: partial unparsing
configurations that override the default one
(`extensions/default_unparsing_config.json`) for a few nodes. Each snippet is
both a testcase and a section of the GNATformat documentation.

## Layout

```
unparsing_config_snippets/
  <group>/             One documentation page (e.g. gnatpp)
    index.rst          Page title and introduction
    <snippet>/         One snippet, i.e. one section of the page
      snippet.json     The configuration snippet
      input.ada        Example input code
      test.out         Expected output (input.ada formatted with the snippet)
      test.yaml        Testcase description
      doc.rst          Section documenting the snippet
```

In the `gnatpp` group, each snippet mimics a `gnatpp` switch, and its
directory is named after that switch, without the leading dashes and without
the value for switches that take one (e.g. `separate-then`, `par-threshold`).

## Testcases

The testsuite turns each directory that contains an `input.ada` file into a
testcase: it runs `lal_unparse` on `input.ada` with the default configuration
overridden by `snippet.json` (79 columns, indentation 3, continuation
indentation 2), and compares the result with `test.out`. The testcase is named
after its directory, e.g. `unparsing__snippets__gnatpp__separate-then`.

`test.yaml` must contain:

```yaml
driver: unparser
is_snippet: true
```

It can also give the grammar rule to parse `input.ada` with (e.g.
`rule: stmt`); by default, `input.ada` is parsed as a compilation unit.

To run these testcases, and to update `test.out` after changing a snippet:

```sh
./testsuite/testsuite.py unparsing/snippets
./testsuite/testsuite.py unparsing/snippets --rewrite
```

Always review the updated `test.out` files.

## Documentation

`gnatformat-doc.anod` builds one page of the GNATformat user manual for each
group. The page contains the group's `index.rst`, followed by the `doc.rst`
file of each snippet, sorted by directory name. It prepends an include of
GNATformat's `defines.hrst`, so `index.rst` and `doc.rst` files can use its
substitutions (e.g. `|GNATformat|`).

`doc.rst` files are sections of the page: underline their title with `-` and
start them with a label (e.g. `.. _gnatpp-separate-then:`) so that other
sections can reference them with `:ref:`. Their `.. include::` directives are
relative to the snippet directory (`.. include:: snippet.json`).

When adding a snippet to the `gnatpp` group, follow the structure of the
existing `doc.rst` files (what the switch does, the GNATformat equivalent, an
example, and any differences with `gnatpp`), and add a row for it to the table
in `gnatpp/index.rst`. Check the claims against the actual behavior of
`gnatpp` with only that switch: its documentation is not always accurate.
