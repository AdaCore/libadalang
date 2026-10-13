.. _gnatpp-split-line-before-record:

``--split-line-before-record``
------------------------------

With this switch, ``gnatpp`` splits the line just before the ``record``
keyword in record type declarations.

.. rubric:: |GNATformat| equivalent

.. include:: snippet.json
   :code: json

.. rubric:: Example

Input code:

.. include:: input.ada
   :code: ada

|GNATformat| output with the configuration snippet above:

.. include:: test.out
   :code: ada
