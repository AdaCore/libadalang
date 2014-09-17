***********************************************
Libadalang introduction and high level approach
***********************************************

Welcome to you! If you are reading this document, you are either interrested
into libadalang, or trying to contribute to it as a developer. This
introduction will go over the high level goals of the project, as well as over
the technical approach that has been chosen to achieve those goals. It is
probably a good idea to read this first, since the approach is not
straightforward, and diving into the code head on might prove difficult.

Need
####

The need for libadalang arises from the conflation of different goals that we
have while designing Ada tooling at AdaCore. Here are those goals:

* We need to make tooling that is Ada aware, both at the syntactic and the
  semantic level

* We need avoid repeating ourselves, that is to avoid duplicating the same code
  in dozens of places in our codebase, so we want to have an unified approach
  to this problem.

* We need in some cases (such as IDEs) to make tooling that can work with
  incorrect and/or evolving Ada code. This is perhaps the most important point,
  because our front-end and other tools based on it are not designed in such
  a way that would make this use case easy. In effect, what we need is
  a semantic interface that would abstract this problem from the client, and
  allow him to deal with the semantic and syntactic structure of the code only.

As a corollary to those primary needs, here are some other secondary goals for
libadalang:

* We want libadalang to be, as you may have guessed, a library that you can
  call from anywhere and is not tied to any particular tool

* We want it to have a high level, stable, easy and fun to use API, that makes
  high level operations on trees easy, while having enough flexibility to
  express complex queries and refactorings.

* We want libadalang to be fast, fast enough to deal effortlessly with
  multi-million of lines code bases, so that it can replace existing ad-hoc but
  fast solutions that are built into our IDEs for code navigation and
  completion.

Desired feature set
###################

In this section we are going to brush quickly over the things that we want to
be able to do with libadalang.

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

Please note that the syntax of the API is absolutely not final! The examples
below will be updated once the API begins to exist.

Exploring the abstract syntax tree
**********************************

The first thing we want to be able to do on this code is to explore the
resulting AST in a straightforward way:

::

    for_loop = ast.statements.first()
    if_st = for_loop.statements.first()
    return_st = if_st.statements.first()
    return_expr = return_st.expr()

This is straightforward, we really just want to access to the tree structure,
with maybe a few shortcuts to be able to explore the tree directly.

TODO: complete this part
