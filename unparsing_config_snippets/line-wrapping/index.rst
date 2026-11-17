Customizing line wrapping
=========================

The following configuration snippets customize how |GNATformat| wraps code.
They override only the relevant parts of the default unparsing configuration.

Save a snippet to a JSON file and pass it to ``--override-layout``:

.. code-block:: sh

   gnatformat -P prj.gpr --override-layout fill_string_concatenations.json

To use it for all formatting of a project, set the ``Override_Layout``
attribute in its ``Format`` package:

.. code-block:: ada

   package Format is
      for Override_Layout ("Ada") use ("fill_string_concatenations.json");
   end Format;

Overrides are applied in the order they are given. If several snippets
configure the same construct, the last one wins.
