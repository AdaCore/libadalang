Ada syntax highlighter
======================

This directory holds a syntax highlighter and a cross-referenced website
generator for Ada whose purpose is to demonstrate the use of Libadalang's Ada
API. In order to compile them, run:

```shell
gprbuild -P highlight.gpr -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable
```

Then, you can run the `highlight` program to produce a standalone HTML document
that contains a mere highlighted source:

```shell
bin/highlight --html example.adb > example.html
```

... or if you are using a terminal emulator that supports xterm-256color color
codes:

```shell
bin/highlight --term256 example.adb
```

You can also run the `ada2web` program to produce a set of inter-linked HTML
documents that contain cross-referenced highlighted sources code for a project.
This relies on GPR project files, so for instance, to highlight this very
highlighting project:

```shell
bin/ada2web -Phighlight.gpr highlight
```

The first argument (`-Phighlight.gpr`) tells to load the `highlight` project
tree, and the second one (`highlight`) tells to generate HTML documents only
for the sources of this project (i.e.excluding sources from dependencies, like
Libadalang's).
