--  Check the handling of record variant parts in basic cases

package Pkg is

   generic
      type T is private;
   package G is
      type R is record
         Value : T;
      end record;
   end G;

   package Boolean_G is new G (Boolean);
   B : Boolean_G.R;
   pragma Test_Object_Type;

   package Integer_G is new G (Integer);
   I : Integer_G.R;
   pragma Test_Object_Type;

end Pkg;
