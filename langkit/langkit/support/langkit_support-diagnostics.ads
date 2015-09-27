with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Tokens; use Langkit_Support.Tokens;

package Langkit_Support.Diagnostics is

   type Diagnostic is record
      Sloc_Range : Source_Location_Range;
      --  The source location range that the diagnostics message refers to

      Message    : Unbounded_Wide_Wide_String;
      --  Message for this diagnostics.  Parsers allocates such messages and it
      --  is up to the user to free them.
   end record;

   function To_Pretty_String (D : Diagnostic) return String;

   package Diagnostics_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Diagnostic);

end Langkit_Support.Diagnostics;
