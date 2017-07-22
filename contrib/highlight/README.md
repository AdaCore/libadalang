Ada syntax highlighter
======================

This directory holds a syntax highlighter for Ada whose purpose is to
demonstrate the use of Libadalang's Ada API. In order to compile it, run:

```shell
gprbuild -P highlight.gpr -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable
```

Then you can run it to produce either an HTML document:

```shell
obj/highlight --html example.adb > example.html
```

... or if you are using a terminal emulator that supports xterm-256color color
codes:

```shell
obj/highlight --term256 example.adb
```
