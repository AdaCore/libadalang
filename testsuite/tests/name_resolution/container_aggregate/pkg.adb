pragma Ada_2022;

with Ada.Containers.Ordered_Sets;

package body Pkg is
   package Int_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Integer);
   subtype Set is Int_Sets.Set;

   procedure P (Arr : Arr_T) is
      Set1 : Set := [for E of Arr when E < 0 => E];
      pragma Test_Statement;
      Set2 : Set := [for E of Arr when E < 0 or else E > 0 => E];
      pragma Test_Statement;
      Set3 : Set := [for Item in 1 .. 5 => Item * 2];
      pragma Test_Statement;
   begin
      null;
   end P;
end Pkg;
