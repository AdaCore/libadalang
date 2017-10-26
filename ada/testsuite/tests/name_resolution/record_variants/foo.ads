--  Test that name resolution for record members works fine for members present
--  in variant parts.

package Foo is

   type Kind_Type is (Integer, Character, Boolean);

   type Record_Type (K : Kind_Type) is record
      case K is
         when Integer =>
            I : Integer;
         when Character =>
            C : Character;
         when others =>
            B : Boolean;
      end case;
   end record;

   R : Record_Type := (K => Integer, I => 0);

   pragma Test (R.K);
   pragma Test (R.I);
   pragma Test (R.C);
   pragma Test (R.B);
   pragma Test (R.Z);

end Foo;
