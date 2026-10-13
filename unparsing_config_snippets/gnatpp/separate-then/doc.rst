.. _gnatpp-separate-then:

``--separate-then``
-------------------

With this switch, ``gnatpp`` places the ``then`` keyword of IF statements on a
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
