========================================================================================
``CaseStmt``, ``CaseExpr`` and ``VariantPart`` rewrite to enforce vertical alternatives
========================================================================================

This configuration snippet illustrates ``AlternativesList`` node rewriting to enforce
the vertical formatting in ``CaseStmt``, ``CaseExpr`` and ``VariantPart`` by forcing
every ``|`` choice to break onto a new line regardless of whether it appears in
a case statement, case expression or record variant part.

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
   :command:`--vertical-case-alternatives` switch.
