Material for the Ada Europe 2018 tutorial on Libadalang
=======================================================

This directory holds solutions and input sources for the Libadalang tutorial
that was held in Lisbon for Ada Europe 2018
(http://ae2018.di.fc.ul.pt/tutorials.html#T8).

In order to compile the solutions, run:

```shell
$ gprbuild -Pada_europe_2018.gpr -XLIBRARY_TYPE=relocatable -p
```

You can then run them:

```shell
obj/rewrite_self_args
obj/derivation_count
```

These work on the `material.gpr` project, and in particular all its sources
files, in the `material` directory.
