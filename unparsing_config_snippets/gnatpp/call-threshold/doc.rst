.. _gnatpp-call-threshold:

``--call-threshold=1``
----------------------

With ``--call-threshold=nnn``, ``gnatpp`` puts each parameter association on
its own line when a call or a generic instantiation has more than ``nnn``
associations and at least one of them uses named notation.

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
