Libadalang changes directory
============================

This directory contains descriptions for changes in Libadalang. Each change is
summarized in a YAML file. There are three types of changes: new features, API
changes, and bug fixes.

The contents are used for two purposes:

1. Create changelogs in Libadalang docs.
2. Provide traceability for issues inside of AdaCore.

To see what should be documented in each YAML entry, please see
[entry_schema.yaml](./entry_schema.yaml), which uses the [JSON
Schema](https://json-schema.org/) schema description, but with YAML objects.

* Files of the form `S123-456.yaml` are old style ticket entries.

* Files in the `libadalang` and `langkit` directories, and of the form
  `123.yaml` are new style Gitlab ticket entries.
