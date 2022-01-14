Better solver API
=================

LAL ticket: TC10-017

High level problem
------------------

As a follow-up to Fedor's ticket (TC09-047), we realize once again that the
way the solver is exposed in Libadalang is sub-optimal:

- We don't want to deal with explicitly mutable logic variables at the
  Libadalang level, where we have to manually memoize the results of solving,
  with all the problems that it causes.

- On the other hand, we don't either want to have a purely functional solver
  API that doesn't take into account what we need in LAL (e.g. attach logic
  variables to nodes), so that we also have to do the association of result to
  nodes manually in a further step.

This should be abstracted away at the Langkit level. At a high level what
we want is to be able to attach logic vars to nodes in the context of a
given equation, and trigger construction & resolution of the equation when
the value of the logic var is queried, in a paradigm where no side effects
are exposed. Here is how it could look like:

.. code:: java

   class Node {
       v : LogicVar[int]

       @solves(v) fun equation(): Equation {
           return self.v in [1, 2, 3, 4]
       }
   }

   // ...

   {
      val n : Node = // ...
      n.v // n.v is a logic domain, ie. a kind of generator you can iterate on
      n.v[0] // get the first result -> 1
   }

Internally, querying ``v`` would call equation to construct the equation, and
solve it. Regular memoization would be used to store the results and attach
them to nodes. The way equations are linked to variables (i.e. how do we know we
have to call equation) needs to be refined, but this is just a preliminary
idea.

This would also give us a higher level interface that we can seamlessly
transition to when we get rid of the solver's logic vars.
