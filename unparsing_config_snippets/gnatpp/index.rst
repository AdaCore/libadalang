Migrating from gnatpp switches
==============================

This page helps users of ``gnatpp`` get the same formatting with
|GNATformat|. Some ``gnatpp`` switches have a direct |GNATformat| counterpart,
or match |GNATformat|'s default behavior. For the others, the following
sections give a configuration snippet that makes |GNATformat| format code the
same way, or in a similar way.

Using a configuration snippet
-----------------------------

Save the snippet to a JSON file and pass that file to the
``--override-layout`` switch:

.. code-block:: sh

   gnatformat -P prj.gpr --override-layout separate_then.json

To make it permanent, use the ``Override_Layout`` attribute in the ``Format``
package of the project file:

.. code-block:: ada

   package Format is
      for Override_Layout ("Ada") use ("separate_then.json");
   end Format;

To mimic several ``gnatpp`` switches, pass several snippets. They are applied
in the order they are given:

.. code-block:: sh

   gnatformat -P prj.gpr --override-layout separate_then.json \
     --override-layout vertical_enum_types.json

Each snippet is designed to be used on its own. Two snippets may change the
layout of the same construct, in which case the last one wins. Check the
result when you combine snippets, and adapt them to your needs if necessary.

Switches covered by a configuration snippet
-------------------------------------------

.. list-table::
   :header-rows: 1
   :widths: 35 15 50

   * - ``gnatpp`` switch
     - Result
     - Notes
   * - :ref:`gnatpp-alignment`
     - Similar
     - ``:=`` in assignments and parameter modes are not aligned
   * - :ref:`gnatpp-call-threshold`
     - Same
     - Only for a threshold of 1
   * - :ref:`gnatpp-no-separate-is`
     - Same
     -
   * - :ref:`gnatpp-no-separate-loop-then`
     - Same
     -
   * - :ref:`gnatpp-par-threshold`
     - Same
     - Only for a threshold of 1
   * - :ref:`gnatpp-separate-loop`
     - Same
     -
   * - :ref:`gnatpp-separate-then`
     - Same
     -
   * - :ref:`gnatpp-split-line-before-record`
     - Same
     -
   * - :ref:`gnatpp-vertical-array-types`
     - Similar
     - Index definitions that do not fit on one line go on separate lines
   * - :ref:`gnatpp-vertical-case-alternatives`
     - Same
     -
   * - :ref:`gnatpp-vertical-enum-types`
     - Same
     -
   * - :ref:`gnatpp-vertical-named-aggregates`
     - Same
     -

Switches that need no configuration snippet
-------------------------------------------

The following ``gnatpp`` switches have a |GNATformat| switch (and the
corresponding ``Format`` project attribute) with the same effect, or match
|GNATformat|'s default behavior:

.. list-table::
   :header-rows: 1
   :widths: 40 60

   * - ``gnatpp`` switch
     - |GNATformat| equivalent
   * - ``-Mnnn``, ``--max-line-length=nnn``
     - ``--width=nnn`` (``Width`` attribute)
   * - ``-in``, ``--indentation=n``
     - ``--indentation=n`` (``Indentation`` attribute)
   * - ``-cln``, ``--indent-continuation=n``
     - ``--indentation-continuation=n`` (``Indentation_Continuation``
       attribute)
   * - ``--eol=lf``, ``--eol=crlf``
     - ``--end-of-line=lf``, ``--end-of-line=crlf`` (``End_Of_Line``
       attribute)
   * - ``-kL``, ``--keyword-lower-case`` (default);
       ``-kU``, ``--keyword-upper-case``
     - ``--keyword-casing=lower``, ``--keyword-casing=upper``
       (``Keyword_Casing`` attribute). Note that |GNATformat| keeps the
       casing of keywords by default.
   * - ``-nD``, ``--name-case-as-declared`` (default);
       ``-nL``, ``-nU``, ``-nM``
     - ``--identifier-casing=definition``, ``lower``, ``upper``, ``mixed``
       (``Identifier_Casing`` attribute). Note that |GNATformat| keeps the
       casing of identifiers by default.
   * - ``--ignore=filename``
     - ``--ignore=filename`` (``Ignore`` attribute)
   * - ``--pipe``
     - ``--pipe``
   * - ``--split-line-before-op``
     - Default behavior. Note that when an expression does not fit on one
       line, |GNATformat| puts each operand on its own line.
