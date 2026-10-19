.. _gnatpp-no-separate-loop-then:

``--no-separate-loop-then``
---------------------------

With this switch, ``gnatpp`` does not place the ``then`` keyword of IF
statements and the ``loop`` keyword of FOR and WHILE loop statements on a
separate line, even when the condition or the loop specification spans
several lines.

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
