.. _gnatpp-par-threshold:

``--par-threshold=1``
---------------------

With ``--par-threshold=nnn``, ``gnatpp`` puts each parameter specification on
its own line when a subprogram has more than ``nnn`` of them. With
``--par-threshold=1``, ``gnatpp`` also splits functions that have a single
parameter, and puts ``return`` on its own line. What follows a split
specification (``is``, ``is null``, ``renames``, ...) stays on its last line.

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

This configuration snippet only reproduces a threshold of 1.
