.. _line-wrapping-fill-string-concatenations:

Filling lines in string concatenations
--------------------------------------

By default, when a concatenation does not fit on one line, |GNATformat|
breaks before every ``&`` operator. This snippet keeps as many operands as
fit on each line and breaks before ``&`` when the next operand would exceed
the configured line width. Short concatenations still stay on one line.

The ``ConcatOp`` template uses a ``fill`` document, and the
``ConcatOperandList`` separator makes each break before ``&`` optional.
Apply this snippet after the tall layout to replace its mandatory breaks
between operands as well. The operand formatting, continuation indentation
and other operators keep their existing layouts.
The snippet applies to all uses of ``&``, including array concatenations,
as the layout does not depend on operand types.

.. include:: snippet.json
   :code: json

.. rubric:: Example

Input code:

.. include:: input.ada
   :code: ada

|GNATformat| output with the snippet above and a line width of 79:

.. include:: test.out
   :code: ada

Comments can force additional line breaks. String literals are not split,
so an individual operand that is too long can still exceed the line width.
