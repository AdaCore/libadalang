--  Check that the size for constrained arrays is correctly reported as
--  unsupported when it still depend on a value computed at runtime.

package Pkg is

   function Compute_Something return Integer with Import;
   Upper_Bound : constant Integer := Compute_Something;

   type Bool_Array is array (Natural range 1 .. Upper_Bound) of Boolean;
   type Rec_Type is record
      BA : Bool_Array;
   end record;

end Pkg;
