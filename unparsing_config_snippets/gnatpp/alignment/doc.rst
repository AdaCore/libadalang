.. _gnatpp-alignment:

``--alignment``
---------------

By default, ``gnatpp`` vertically aligns the constructs below when they are
on consecutive lines; a blank line ends the alignment. ``-A0`` (or
``--no-alignment``) turns alignment off.

The ``gnatpp`` documentation describes ``-A1`` to ``-A5`` as a way to turn on
each alignment separately after ``-A0``. The current ``gnatpp`` turns all of
them on as soon as one of these switches is given, so in practice alignment is
either all on (the default) or all off.

.. list-table::
   :header-rows: 1
   :widths: 10 45 45

   * - Switch
     - ``gnatpp`` aligns
     - |GNATformat|
   * - ``-A1``
     - ``:`` in declarations
     - Default layout
   * - ``-A2``
     - ``:=`` in initializations in declarations
     - This configuration snippet
   * - ``-A3``
     - ``:=`` in assignment statements
     - Not supported
   * - ``-A4``
     - ``=>`` in associations
     - Default layout
   * - ``-A5``
     - ``at`` in the component clauses of record representation clauses
     - Default layout

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

* ``:=`` in assignment statements are not aligned. In the unparsing
  configuration, only blank lines and comments can end an alignment, so
  consecutive assignments would also be aligned across the other statements
  between them.

* ``gnatpp`` also aligns the types of parameter specifications after their
  modes (``E :    out Float``). |GNATformat| does not.

* In record representation clauses, ``gnatpp`` also aligns the bounds of the
  ranges (``0 ..  7``). |GNATformat| does not.
