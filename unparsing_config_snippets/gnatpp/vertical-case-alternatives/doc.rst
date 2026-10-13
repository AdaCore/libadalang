.. _gnatpp-vertical-case-alternatives:

``--vertical-case-alternatives``
--------------------------------

With this switch, ``gnatpp`` formats case statements, case expressions and
variant parts with additional line breaks, so that each ``|`` choice goes on
a separate line.

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
