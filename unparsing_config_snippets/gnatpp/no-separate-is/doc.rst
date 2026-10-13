.. _gnatpp-no-separate-is:

``--no-separate-is``
--------------------

With this switch, ``gnatpp`` does not place the ``is`` keyword of a
subprogram body on a separate line when the subprogram specification spans
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
