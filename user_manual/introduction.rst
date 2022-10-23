************
Introduction
************

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
language.  If you want such a functionality, you'll need to use a full Ada
compiler, such as GNAT.

Need
####

The need for Libadalang arises from the conflation of different goals that we
have while designing Ada tooling at AdaCore. Here are those goals:

* We need to make tooling that is Ada aware, both at the syntactic and the
  semantic level.

* We need to avoid repeating ourselves, that is to avoid duplicating the same
  code in dozens of places in our codebase, so we want to have a unified
  approach to this problem.

* We need in some cases (such as IDEs) to make tooling that can work with
  incorrect and/or evolving Ada code.

* We need a tool that can work incrementally, e.g. doesn't have to start from
  scratch if you change a variable name in the dependency of a file you want to
  analyze.

Enter Libadalang
################

We are going to base our examples on this simple snippet of Ada code:

.. code-block:: ada
    :linenos:

    procedure Test (A : Foo; B : Bar) is
    begin
        for El : Foo_Elem of A loop
            if not El.Is_Empty then
                return B.RealBar (El);
            end if;
        end loop;
    end Test;

In the following examples, we will show how to accomplish the same thing with
Libadalang, with the Ada API, and with the Python API.

The Ada API is great for integration in existing Ada programs, and for
situations where you need some speed and static safety guarantees.

The Python API is a good fit for rapid prototyping of programs using
Libadalang. We greatly encourage even Ada die-hard to try out the Python API
for exploring the API, the tree structures and available accessors. They map
directly to the Ada API, so the knowledge is shared.

Parsing a file
**************

Let's say we did put the content of the above Ada snippet in the ``test.adb``
file. Here is how you can parse the resulting file with Libadalang.

.. code-block:: ada

    with Ada.Text_IO;          use Ada.Text_IO;
    with Libadalang.Analysis;  use Libadalang.Analysis;

    procedure Main is
       Ctx  : Analysis_Context := Create_Context;
       Unit : Analysis_Unit := Ctx.Get_From_File ("test.adb");
    begin
       Unit.Print;
       Put_Line ("Done.");
    end Main;

.. attention::

   You will need to make Libadalang visible to ``gprbuild``, which implies that
   the project file for the above main needs to start with

   .. code-block:: ada

      with "libadalang";

   And Libadalang needs to be in your ``GPR_PROJECT_PATH``.

.. attention::

   You need to have a valid GNAT toolchain in your ``PATH``, and it also needs
   to be the same GNAT toolchain that you use to compile the Ada project you're
   analyzing.

This snippet will create an analysis context, which usually corresponds to the
context of your whole analysis - be it just one file, a whole project, or
several projects - and parse our Ada file and return the resulting analysis
unit instance. Calling the ``Print`` primitive on the instance will dump the
resulting tree.

.. code::

    CompilationUnit[1:2-5:11]
    | body:
    | | LibraryItem[1:2-5:10]
    | | | is_private: False
    | | | item:
    | | | | SubprogramBody[1:2-5:10]
    | | | | | overriding: unspecified
    | | | | | subp_spec:
    | | | | | | SubprogramSpec[1:2-1:35]
    | | | | | | | name:
    | | | | | | | | Id[1:12-1:16]
    | | | | | | | | | tok: Test
    | | | | | | | params:
    ... continued

Exploring the tree
******************

The first thing you can do with this is explore the syntax tree through simple
accessors.

.. code-block:: ada

    with Ada.Text_IO;          use Ada.Text_IO;
    with Libadalang.Analysis;  use Libadalang.Analysis;

    procedure Main is
       Ctx  : Analysis_Context := Create_Context;
       Unit : Analysis_Unit    := Ctx.Get_From_File ("test.adb");
       CU   : Compilation_Unit := Unit.Root.As_Compilation_Unit;
       Bod  : Library_Item     := CU.F_Body.As_Library_Item;
       Subp : Subp_Body        := Bod.F_Item.As_Subp_Body;
    begin
       Subp.Print;
    end Main;

This code will access the ``Subp_Body`` node of the Test subprogram that
constitutes the main element of our file. But as you can see, even if it is
precise, this is not a very practical way of exploring the tree.
