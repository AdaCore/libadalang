package Langkit_Support.PP_Utils is

   procedure Print_Indent (Level : Natural);
   --  Print a number of indents corresponding to Level

   procedure Put_Line (Level : Natural; Str : String);
   --  Equivalent to Ada.Text_IO.Put_Line, but print a number of indents
   --  corresponding to Level first.

end Langkit_Support.PP_Utils;
