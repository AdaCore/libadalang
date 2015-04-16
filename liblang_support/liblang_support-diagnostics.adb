with Interfaces; use Interfaces;

package body Liblang_Support.Diagnostics is

   ----------------------
   -- To_Pretty_String --
   ----------------------

   function To_Pretty_String (D : Diagnostic) return String is
      Sloc : constant Source_Location := Start_Sloc (D.Sloc_Range);
   begin
      return ("Line" & Unsigned_32'Image (Sloc.Line)
              & ", column" & Unsigned_16'Image (Sloc.Column)
              & ": " & To_String (D.Message));
   end To_Pretty_String;

end Liblang_Support.Diagnostics;
