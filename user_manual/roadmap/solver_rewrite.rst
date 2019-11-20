Adalog solver rewrite
=====================

Global goal
-----------

Rewrite Adalog into a way that allows us to:

1. Express analyses about whether an equation is sound or not
2. Refactor an equation into the list of all its disjunctions.
3. Easily weed out branches that cannot provide a solution (because they
   cannot provide a sound solution where all variables are defined)
4. Write a much simpler interpreter

In this document I will try to summarize how we expect to do that.

Expand high level operations to low level ones
----------------------------------------------

The first pass we want to go through, is to get rid of some of the
existing Adalog relations, by expanding them to simpler bricks.

One of the goals is to be able to clearly express which variables a
relation **defines** or **uses**.

Introduce a ``Val`` operation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We want to introduce a ``Val`` low level operation that simply accesses
the value of a logic variable. The operation ``Val (X)`` **uses** ``X``.

Expand ``Unify`` to ``Assign`` and ``Val``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We want to transform

.. code:: ocaml

   Unify X <=> Y

into

.. code:: ocaml

   X <= Val (Y) or Y <= Val (X)

This way, we have two assign operations, one that **uses** ``X`` and
**depends on** ``Y``, the other that **uses** ``Y`` and **depends on**
``X``.

Expand ``Member`` into ``Any`` and ``Assign``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This was the original implementation of ``Member``, that was removed
because of performance in the old execution scheme. In the new scheme,
this will be necessary in order to do full disjunction expansion.

What we want to do is to then expand

.. code:: ocaml

   Member (X, {1, 2, 3, 4, 5})

into

.. code:: ocaml

   Any (X <= 1, X <= 2, X <= 3, X <= 4, X <= 5)

Develop relations
-----------------

Here, we want to apply a development rule, similar to arithmetic
development rules, which is

.. code:: ocaml

   A and (B or C)  => (A and B) or (B and C)

or, with variadic logic operations:

.. code:: ocaml

   All(A, B, C, ..., Any(D, E, F))

      <=>

   Any(
       All(D, A, B, C, ...),
       All(E, A, B, C, ...),
       All(F, A, B, C, ...)
   )

Applying this transformation recursively, until we reach a point where
there is only one top-level disjunction, and all other relations are
either conjunctions or non-composite relations. Since an All(All) can be
inlined, the expected max depth of the relation is 3. At this stage the
relation is effectively a list of either conjunctions or non-composite
relations. The latter being a very unlikely and pretty simple corner
case, let’s assume that we have a disjunction list of all possible
conjunctions.

The algorithm
~~~~~~~~~~~~~

   Note: Here we assume that every ``Any`` can only contain ``All``
   relations or atomic relations, because other ``Any`` relations would
   have been inlined.

Given an ``All`` relation that contains number of ``Any`` between
``1 .. N``, we want to:

-  Sort the relation to have the atomic relations first, and the
   ``Any``\ s afterwards.

-  Take the first ``Any``, and “inline” every other relation (including
   subsequent ``Any``\ s) of the parent ``All`` inside each of its
   branches.

-  If there were remaining ``Any`` relations, run this algorithm
   recursively on every resulting ``All`` relations that is inside the
   first ``Any``.

-  Return the first ``Any``, destroy self

Exponential time resolution optimization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is likely that it is during that expansion phase that we want to
optimize out exponential time resolution problems. Assuming we start
with an ``All`` relation that contains an ``Any``, we can try to do an
optimization similar to the one described in the `Exponential resolution
in Adalog <https://hackmd.io/oaacRJy6TauNDEcgGEtPhQ>`__ document.

The above document informally describes the construction of a constraint
map, mapping specific variables to “constraints”. We can define more
formally those constraints using the terms defined in this document:

   A constraint is any atomic relation that is part of the All relation,
   and that **uses** the value of a logic variable.

We can then create a mapping from logic variables to constraints,
mapping the variable used by a relation to the relation, for every
atomic relation that **uses** a logic variable.

Then, before inlining Self’s relations in the ``Any`` sub-branches, we
can iterate over every of those sub-branches:

1. If the sub-branch is an atomic relation, check if it **defines** the
   value of a variable, and also that it doesn’t **use** any other
   variable. If that is the case, check if there is one or several
   constraints for this variable in the constraint set. If that is the
   case, execute the relation, then the constraints. If the result is
   negative, we can get rid of the relation right away.

2. If the sub-branch is an ``All``, run the above algorithm over all of
   its atomic components.

When transforming children ``All`` relations, we pass down the existing
set of constraints. It will then be added to the one that will be
constructed in the recursive call.

Check completeness
------------------

We define an equation as being complete if at least one of the ``All``
branches in the toplevel ``Any`` binds every variables involved in the
equation. After development, we want to check that this condition is
satisfied. If not, return a special error.

Topo sort/check out for cycles
------------------------------

Once we’ve transformed a relation - any relation, not necessarily the
top level one - into the form above, we can do a topological sort of the
relations inside a conjunction.

After this transformation, every relation inside a conj is assumed to be
an atomic relation. Every relation **uses** a logic variable, or
**defines** a logic variable, or both (but not for the same variable).
This \*allows us to build a dependency graph. If we do a topological
sort of this dependency graph, it allows us to:

1. Check for cycles. For example, such a conjunction would be detected
   and flagged as incorrect

.. code:: ocaml

   All(X <= Val (Y), Y <= Val (X))

because, expanded into a dependency list it gives the following:

.. code:: ocaml

   [(Uses(X), Defines(Y)), (Uses(Y), Defines(X))]

which in turns forms the following dependency graph

.. code:: graphviz

   digraph g{
     y_to_x -> x_to_y
     x_to_y -> y_to_x
     y_to_x [label="X ← Val (Y)",shape=box,style=rounded];
     x_to_y [label="Y ← Val (X)",shape=box,style=rounded];
   }

Execute the resulting relation
------------------------------

Executing the resulting relations should be extremely simple, since you
have a structure like this:

.. code:: ocaml

   Any(
       All(A11, A12, A13, ...),
       All(A21, A22, A23, ...),
       All(A31, A32, A33, ...),
       ...
   )

Where every ``A..`` relation is an atomic relation that you can execute
in order. The execution algorithm is then to:

1. Take every all branch one after the other.
2. Execute every atomic relation in it one after the other. If one
   returns false, switch to the next all branch, and reset all
   variables.
3. If we get to the end of the All without a failure, then we have a
   solution.

This is very easy to execute, and almost completely stateless, unlike
the previous interpreter. Writing an interpreter should be trivial, and
writing a JIT should even be possible if needed :)
