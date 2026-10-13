.. _gnatpp-vertical-named-aggregates:

``--vertical-named-aggregates``
-------------------------------

With this switch, ``gnatpp`` formats aggregates "vertically" when named
notation is used for component associations: each component association goes
on a separate line. Despite the name of the switch, ``gnatpp`` also does this
for aggregates that use only positional notation.

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

.. rubric:: Differences with ``gnatpp``

Aggregates in ``Depends`` aspects keep the default |GNATformat| layout, because
``gnatpp`` does not format them consistently.
