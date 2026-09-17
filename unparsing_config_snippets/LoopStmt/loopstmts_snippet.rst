=======================================================================
``ForLoopStmt`` and ``WhileLoopStmt`` rewrite to split before ``loop`` 
=======================================================================

This configuration snippet illustrates ``ForLoopStmt`` and ``WhileLoopStmt`` node
rewriting to enforce a line break before the ``loop`` keyword. This will place
the keyword ``loop`` in FOR and WHILE loop statements on a separate line.

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
   :command:`--separate-loop` switch.
