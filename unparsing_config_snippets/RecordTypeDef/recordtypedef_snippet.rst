=================================================
``RecordDef`` rewrite to break before ``record``
=================================================

This configuration snippet illustrates ``RecordDef`` node rewriting to enforce
line split before the ``record`` keyword in a record type declaration, e.g. the
``record`` keyword goes on a separate line.

.. rubric:: Input code

Consider the following code:

.. include:: input.ada
   :code: ada


.. rubric:: Expected output code

The desired layout is structured as follows:

.. include:: test.out
   :code: ada
	     

.. rubric:: Configuration snippet

This result is produced using the following custom configuration:

.. include:: snippet.json
   :code: json


.. note::

   This configuration snippet produces the same result as the ``gnatpp``
   :command:`--split-line-before-record` switch.

