=======================================================
``ObjectDecl`` rewrite to enforce alignment on separators
=======================================================

This configuration snippet illustrates ``ObjectDecl`` node rewriting to enforce
alignment for ``:`` and ``:=`` separators across object declarations.
Inserting a blank line acts as a boundary that breaks the alignment.

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
