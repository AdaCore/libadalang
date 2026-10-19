.. _gnatpp-separate-loop:

``--separate-loop``
-------------------

With this switch, ``gnatpp`` places the ``loop`` keyword of FOR and WHILE loop
statements on a separate line.

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
