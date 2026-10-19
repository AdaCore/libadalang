.. _gnatpp-vertical-enum-types:

``--vertical-enum-types``
-------------------------

With this switch, ``gnatpp`` formats enumeration type declarations
"vertically": the list of enumeration literals goes on a separate line, even
when it contains a single literal, and each enumeration literal goes on a
separate line.

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
