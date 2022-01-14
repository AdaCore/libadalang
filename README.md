Libadalang
==========

Libadalang is a library for parsing and semantic analysis of Ada code. It is
meant as a building block for integration into other tools. (IDE, static
analyzers, etc.)

Libadalang provides mainly the following services to users:

* Complete syntactic analysis with error recovery, producing a precise syntax
  tree when the source is correct, and a best effort tree when the source is
  incorrect.

* Semantic queries on top of the syntactic tree, such as, but not limited to:
  * Resolution of references (what a reference corresponds to)
  * Resolution of types (what is the type of an expression)
  * General cross references queries (find all references to this entity)

Libadalang does not (at the moment) provide full legality checks for the Ada
language.  If you want such a functionality, you’ll need to use a full Ada
compiler, such as GNAT.

While it can be used in Ada (2012+) and Python (3.7+), Libadalang also provides
a low-level C API (meant to write bindings to other languages) and an
experimental OCaml API.

If you have problems building or using Libadalang, or want to suggest
enhancements, please open [a GitHub
issue](https://github.com/AdaCore/libadalang/issues/). We also gladly accept
[pull requests](https://github.com/AdaCore/libadalang/pulls)!

Status of the project
---------------------

Libadalang is still in development and we allow ourselves some headroom in
terms of breaking backwards compatibility. If you want to use a stable version
of Libadalang, you'll need to build from one of the stable branches, such as
[19.1](https://github.com/AdaCore/libadalang/tree/19.1).

Libadalang currently:

* Is able to parse 100% of Ada 2012 syntax, and presents a well formed tree for
  it. Support for Ada 2022 constructs is a work in progress.

* Is able to recover most common syntax errors. The error messages are
  behind those of GNAT, but the recovery will potentially work better in many
  situations.

* Provides name resolution/navigation.

* Is able to handle some very simple incremental processing. Reparsing a source
  A and querying xref on a source B that depends on A is handled efficiently.

How to use Libadalang
---------------------

There are several ways to get Libadalang:

* Build it using the [Libadalang Alire
  crate](https://alire.ada.dev/crates/libadalang).  This will only let you
  build the current and the previous releases (i.e. not the development
  version), but is by far the easiest way, as [Alire](https://alire.ada.dev/)
  automatically deals with dependencies.

* Build it from Git repository sources: install all dependencies, generate its
  code and to build it. Please refer to the [User
  Manual](user_manual/building.rst) for detailed instructions.

* **Important**: if you are an AdaCore customer with a GNAT Pro subscription,
  please get Libadalang through GNATtracker, as this is the only version of
  Libadalang that is covered by your support contract.

To learn how to use the API from Libadalang's the development branch, you can
read the [AdaCore Live
Docs](http://docs.adacore.com/live/wave/libadalang/html/libadalang_ug/index.html)
(updated daily).

Quick overview
--------------

Libadalang has a Python API, for easy prototyping and explorative programming.
It ships with an executable named `playground`, that allows you to analyze Ada
files and play with them in an interactive Python console.

Given the following `main.adb` Ada file:

~~~ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
    Put_Line ("Hello World");
end Main;
~~~

You can start the playground on it:

~~~sh
% playground main.adb

--
-- libadalang playground
--

The file(s) passed as argument have been put into the `u` variable, or units if
there are multiple.

Enjoy!

In [1]: print(u.root.text)
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
    Put_Line ("Hello World");
end Main;

In [2]: print(u.root.findall(mdl.CallExpr))
[<CallExpr 5:5-5:29>]

In [3]: print(u.root.findall(mdl.CallExpr)[0].text)
Put_Line ("Hello World")
~~~

The playground embeds the [IPython](https://ipython.org/) interactive Python
console, so you have a modern interactive programming environment. You can use
tab completion to explore the Libadalang API.

Libadalang and ASIS
-------------------

ASIS is widely used for static analysis of Ada code, and is an ISO standard. It
is still the go-to tool if you want to create a tool that analyses Ada code.
Also, as explained above, Libadalang is not mature yet, and cannot replace ASIS
in tools that require semantic analysis.

However, there are a few reasons you might eventually choose to use Libadalang
instead of ASIS:

1. The ASIS standard has not yet been updated to the 2012 version of Ada. More
   generally, the advantages derived from ASIS being a standard also means that
   it will evolve very slowly.

2. Syntax only tools will derive a lot of advantages on being based on
   Libadalang:

   * Libadalang will be completely tolerant to semantic errors. For example, a
     pretty-printer based on Libadalang will work whether your code is
     semantically correct or not, as long as it is syntactically correct.

   * Provided you only need syntax, Libadalang will be much faster than ASIS'
     main implementation (AdaCore's ASIS), because ASIS always does complete
     analysis of the input Ada code.

3. The design of Libadalang's semantic analysis is lazy. It will only process
   semantic information on-demand, for specific portions of the code. It means
   that you can get up-to-date information for a correct portion of the code
   even if the file contains semantic errors.

4. Libadalang has bindings to C and Python, and its design makes it easy to
   bind to new languages.

5. Libadalang is suitable to write tools that work on code that is evolving
   dynamically. It can process code and changes to code incrementally. Thus, it
   is suitable as an engine for an IDE, unlike AdaCore's ASIS implementation.

6. Libadalang is not tied to a particular compiler version. This combined with
   its staged and error tolerant design means that you can use it to detect
   bugs in Ada compilers/tools.

Implementation
--------------

The Libadalang project is based on the
[Langkit](https://github.com/AdaCore/langkit) framework, so its
Ada/Python/C/OCaml source code is not checked in this repository: it is instead
generated from the Langkit language specification that you can find in
[ada/](ada/). This language specification, while embedded in Python syntax, is
mostly its own language, the Langkit DSL, that is used to specify the part of

See the [Developer Manual](dev_manual) for more information about Libadalang's
development.
