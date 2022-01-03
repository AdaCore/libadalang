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

Exponential time resolution optimization (1)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Exponential time resolution optimization (2)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Following the removal of the ``eq_prop`` feature of propagate relations,
predicates generally used in Libadalang equations work with two logic variables
instead of a single one, which essentially cancels the optimization (1).
Indeed, it is easily implementable for predicates with one logic var (the
var-to-constraint map can reflect relations that use only one var), and much
harder for predicates taking more variables.

As a consequence, the (1) optimization is not enough to efficiently solve the
following real-world equation:

.. code-block:: ada

    procedure Test is
       type Rec is record
          X : Integer;
       end record;

       type Arr is array (Positive range <>) of Integer;

       function Foo (X : Integer) return Rec is ((X => X));
       function Foo (X : Integer) return Arr is (1 => X);
       function Foo (X : Integer) return Integer is (X);
       function Foo (X : Integer) return Float is (1.0);
       function Foo (X : Integer) return Duration is (1.0);

       V : Integer;
    begin
       V := Foo (Foo (Foo (1 + 2 * 3).X) (4 + 5 + 6 + 7));
       pragma Test_Statement;
    end Test;

The inefficiency is the same as before: the equation is mostly a conjuction of
disjunctions, and we spend a lot of time exploring all combinations.

Instead of stopping early the exploration of a branch in the combination
exploration recursion, a variation of the optimization would be to simplify the
big relation before exploring all combinations of anys.

For instance:

.. code-block:: text

    All(
        R1,
        Any(R_A1, R_A2, R_A3),
        Any(R_B1, R_B2, R_B3)
    )

We could first look for a contradiction between ``R1`` and in any of the
``R_*`` relations, and remove relations that do have a contradiction. If both ``R1 &
R_A1`` and ``R1 & R_A3`` are false, we can simplify the above big relation to:

.. code-block:: text

    All(
        R1,
        R_A2,
        Any(R_B1, R_B2, R_B3)
    )

From there, we can search for contradictions between ``R1``, ``R_A2`` and the
remaining ``R_*`` relations. Assuming ``R_B1`` and ``R_B3`` do have a
contradiction, we are now left with:

.. code-block:: text

    All(R1, R_A2, R_B3)

i.e. the search space is much reduced.

In general, the simplification may not be able to remove all Anys, but the hope
is that it would be able to remove most of them, and reduce their size
otherwise, and thus restrict a lot the time taken to explore all combinations
(because there are much fewer combinations left). However, for Anys that stay
with more than 1 element, this algorithm will not be able to use constraints in
their alternatives to simplify other parts of the relation.

If this new optimization is too weak for equations that Libadalang produces in
practice (too many/too big Anys left), it may be valuable to run this
optimization later, at various stages during the development of the equation,
i.e. when exploring combinations from an All relation: at this point, the
current list of atoms may contain atoms from alternatives of already explored
Anys, and thus may provide more opportunities to run simplifications.

Here is pseudo Python code for the simplification algorithm:

.. code-block:: python

    def has_contradiction(atoms: List[Relation]) -> bool:
        """Return whether the given list of atoms contains a contradiction.

        This is similar to the regular resolution of a list of atoms, with an
        important difference: one atom uses a variable that no other atom
        defines will not cause this function to return True (the regular solver
        considers that the list of atoms has no solution in that case, i.e.
        "solve()" returns False in that case).
        """
        ...

    def split_all(relation: Relation) -> Tuple[List[Relation], List[Relation]]:
        """Split LogicAny from atoms in the given LogicAll relation."""
        ...

    def simplify(relation: Relation, outer_atoms: List[Relation]) -> Relation:
        """
        Try to simplify the given relation (a LogicAll relation). Return the
        simplified relation.
        """
        anys, self_atoms = split_all(relation)
        atoms = outer_atoms + self_atoms

        # Run a fixed-point algorithm: as long as "atoms" keeps growing, look
        # for new contradictions in "anys". Every time a LogicAny relation is
        # is simplified enough to
        atoms_changed = True
        while atoms_changed:
            atoms_changed = False
            if has_contradiction(atoms):
                return LogicFalse()

            # In every Any relation, go through alternatives ("alt_rel") and
            # recursively try to simplify them.
            any_rels = list(enumerate(anys))
            for i, any_rel in reversed(any_rels):
                alt_rels = list(enumerate(any_rel.sub_relations))
                for j, alt_rel in reversed(alt_rels):

                    if isinstance(alt_rel, LogicAll):
                        alt_rel = simplify(alt_rel, atoms)
                    elif has_contradiction(atoms + [any_rel]):
                        alt_rel = LogicFalse()

                    # If this alternative always lead to a contradiction: we
                    # can remove it from "any_rel"...
                    if isinstance(alt_rel, LogicFalse):
                        any_rel.sub_relations.pop(j)

                        # ... and if we have only one alternative left in
                        # "any_rel", this is no longer a disjunction: unwrap it
                        # and move its items to "anys"/"atoms".
                        if len(any_rel.sub_relations) == 1:
                            alt_rel = any_rel.sub_relations[0]
                            anys.pop(i)
                            if isinstance(alt_rel, LogicAll):
                                sub_anys, sub_atoms = split_all(alt_rel)
                                anys.extend(sub_anys)
                                atoms.extend(sub_atoms)
                                if sub_atoms:
                                    atoms_changed = True
                            else:
                                atoms.append(alt_rel)
                                atoms_changed = True

                    else:
                        # Now use the simplified sub-relation
                        any_rel.sub_relations[j] = alt_rel

        return LogicAll(atoms + anys)
