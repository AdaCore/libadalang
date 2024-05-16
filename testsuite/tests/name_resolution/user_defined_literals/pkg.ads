pragma Ada_2022;

package Pkg is
   pragma Elaborate_Body;

   type T is private with
      Integer_Literal => IL;

   type U is private;

   function IL (I : String) return T;
private
   type T is record
      A : Integer;
   end record;

   type U is new T;

   function IL (I : String) return T is
     (A => (Integer'Value (I)));
end Pkg;
