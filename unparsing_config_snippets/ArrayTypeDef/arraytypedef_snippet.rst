========================================================
``ArrayTypeDef`` rewrite to enforce vertical formatting
========================================================

This configuration snippet illustrates ``ArrayTypeDef`` node rewriting to enforce
the formatting of the array type declaration “vertically”, e.g. for multidimensional arrays,
each ``index_subtype_definition`` or ``discrete_subtype_definition`` goes on a separate line.
To achieve this, the rewritting will override the ``ConstraintList`` and
``UnconstrainedArrayIndexList`` of the ``ArrayTypeDef`` node.

.. rubric:: Input code

Consider the following code snippet:

.. include:: input.ada
   :code: ada


.. rubric:: Expected output code

The desired layout is structured as follows:

.. include:: test.out
   :code: ada
	     

.. rubric:: Configuration snippet

This result is obtained using the following custom configuration:

.. include:: snippet.json
   :code: json


.. note::

   This configuration snippet produces the same result as the ``gnatpp``
   :command:`--vertical-array-types` switch.
