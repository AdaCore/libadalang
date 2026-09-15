===========================================================
Named ``Aggregate`` rewrite to enforce vertical formatting
===========================================================

This configuration snippets illustrates how the ``AggregateAssoc`` node
is rewritten to force vertical formatting when named notation is used for
any component association, placing each component association on its own
line. Hence, the ``AggregateAssoc`` node undergoes a rewrite whenever named
notation is applied to one or more component associations.

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
