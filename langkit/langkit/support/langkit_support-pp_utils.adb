with Ada.Text_IO; use Ada.Text_IO;

package body Langkit_Support.PP_Utils is

   ------------------
   -- Print_Indent --
   ------------------

   procedure Print_Indent (Level : Natural) is
   begin
      for I in 1 .. Level loop
         Put ("| ");
      end loop;
   end Print_Indent;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Level : Natural; Str : String) is
   begin
      Print_Indent (Level);
      Put_Line (Str);
   end Put_Line;

end Langkit_Support.PP_Utils;
