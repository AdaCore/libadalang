.. _gnatpp-vertical-array-types:

``--vertical-array-types``
--------------------------

With this switch, ``gnatpp`` formats array type definitions "vertically":
``of`` and the component type always go on a separate line and, in array type
declarations, so does ``array`` and the index definitions. The index
definitions stay on a single line when they fit there.

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

When the index definitions do not fit on a single line, ``gnatpp`` puts as
many of them as fit on each line, while this configuration snippet puts each
of them on its own line.
