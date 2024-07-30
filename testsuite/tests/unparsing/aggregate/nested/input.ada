Formatted_Edits'
  (Unit => Unit,
   Edit => Text_Edit'(Location => Text_Edit_Sloc, Text => Prettier_Ada.Documents.Format (Long_Document, Format_Options)),
   Formatted   => Enclosing_Node,
   Diagnostics => Diagnostics)
