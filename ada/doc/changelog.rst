
Changelog for libadalang
========================

This page includes a changelog of every public facing change made in
libadalang, in chronologic order. It is useful if you're using a recent version
of libadalang, and you want to be aware of potential breaking API changes or
new features.

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

