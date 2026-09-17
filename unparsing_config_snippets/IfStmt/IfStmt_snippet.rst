========================================================
``IfStmt`` rewrite to place ``then`` on a separate line 
========================================================

This configuration snippet illustrates ``IfStmt`` and ``ElsifStmtPart`` nodes
rewriting to enforce a line break before the ``then`` keyword. This will place
the keyword ``then`` in IF statements on a separate line.

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
   :command:`--separate-then` switch.
