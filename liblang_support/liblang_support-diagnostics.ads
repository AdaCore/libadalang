with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Liblang_Support.Tokens; use Liblang_Support.Tokens;

package Liblang_Support.Diagnostics is

   type Diagnostic is record
      Sloc_Range : Source_Location_Range;
      Message    : Unbounded_String;
   end record;

   function To_Pretty_String (D : Diagnostic) return String;

   package Diagnostics_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Diagnostic);

end Liblang_Support.Diagnostics;
