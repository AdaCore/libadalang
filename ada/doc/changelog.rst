
Changelog for libadalang
========================

This page includes a changelog of every public facing change made in
libadalang, in chronologic order. It is useful if you're using a recent version
of libadalang, and you want to be aware of potential breaking API changes or
new features.

Add Paren_Expr
--------------

Changed on 2016-10-19

Now parenthesized operations will be explicitly translated into
``Paren_Expr`` nodes, so that the following code snippet::

    B + (C / (D + 12))

Will produce the following tree::

    BinOp[1:1-1:19]
    | left:
    | | Id[1:1-1:2]
    | | | tok: B
    | op: plus
    | right:
    | | ParenExpr[1:5-1:19]
    | | | expr:
    | | | | BinOp[1:6-1:18]
    | | | | | left:
    | | | | | | Id[1:6-1:7]
    | | | | | | | tok: C
    | | | | | op: div
    | | | | | right:
    | | | | | | ParenExpr[1:10-1:18]
    | | | | | | | expr:
    | | | | | | | | BinOp[1:11-1:17]
    | | | | | | | | | left:
    | | | | | | | | | | Id[1:11-1:12]
    | | | | | | | | | | | tok: D
    | | | | | | | | | op: plus
    | | | | | | | | | right:
    | | | | | | | | | | Int[1:15-1:17]
    | | | | | | | | | | | tok: 12

DerivedTypeDef new field subtype_indication
-------------------------------------------

Changed on 2016-10-06

DerivedTypeDef gains a new ``subtype_indication`` field, of type
``SubtypeIndication``. It replaces the three fields that corresponded to
the subtype indication before: ``null_exclusion``, ``name`` and
``constraint``.

Refactor type expressions, part 2
---------------------------------

Changed on 2016-10-06

* Remove the nested type expression variant, hoisting its components
  into the type expression.

* Rename ``TypeRef`` into ``SubtypeIndication``, to follow RM vocabulary
  more closely

* Rename ``type_expr`` field into ``subtype`` in ``SubtypeDecl``

* Rename the ``*_type_ref`` grammar rules into ``*_subtype_indication``

* Put the not null field into subtype indications, and remove it
  from type expression, because it was redundant for anonymous access
  definitions.

Allocator uses type_ref
-----------------------

Changed on 2016-10-05

The ``Allocator`` node now stores a ``TypeRef`` for its type rather than a
``TypeExpression``, because TypeExpression was too general.

The ``allocator`` grammar rule is altered in consequence.

The field storing the type in the ``Allocator`` node is also renamed from
``expr`` to ``type``.

discrete_subtype_def uses type_ref
----------------------------------

Changed on 2016-10-05

The `discrete_subtype_def` grammar rule is now::

    discrete_subtype_definition= discrete_range | type_ref

It used ``type_expression`` instead of ``type_ref`` before, which was
too general.

Remove redundant name in allocator grammar
------------------------------------------

Changed on 2016-09-16

The grammar rule for allocator becomes::

    allocator = "new" ["(" name ")"] type_expression

Which has the effect of changing the type of the ``Allocator.expr``
field to ``TypeExpression``

Refactor grammar for anonymous types
------------------------------------

Changed on 2016-09-16

Anonymous types declarations are now full type declarations without an
associated name. This will simplify type resolution.

New ``FullTypeDecl`` derived type, ``AnonymousTypeDecl``, that will be
used for every anonymous type declaration.

``AccessDef`` is now abstract, and ``SubprogramAccessDef`` and
``TypeAccessDef`` derive from it.

The ``AccessExpression`` hierarchy disappears completely.

Token_Type.Image returns more complete image
--------------------------------------------

Changed on 2016-09-15

Instead of returning just the text or the kind, ``Token_Type.Image``
will now return a full representation of the Token structure::

    <Token Kind=Identifier Text="B">

Also adds to new functions, ``Token_Type.Text``, which will return the
text of the token, either as a String or as a Text_Type.

Add function for iteration on token range
-----------------------------------------

Changed on 2016-09-14

The ``Token_Range`` function allows the user to conveniently iterate on
the stream of tokens encompassed by a specific AST Node. For example:

.. code-block:: ada

    --  Prints every token of Node
    for T of Node.Token_Range loop
        Put_Line (Image (T));
    end loop;

Add function for token equivalence
----------------------------------

Changed on 2016-09-13

The ``Is_Equivalent`` function allows the user to compare two tokens,
and the operation will return True if the Token have the same structure
(text and kind), even if they don't have the same position or file.

