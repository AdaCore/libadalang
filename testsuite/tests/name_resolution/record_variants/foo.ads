--  Test that name resolution for record members works fine for members present
--  in variant parts.

package Foo is

   type Kind_Type is (Int, Char, Bool);

   type Record_Type (K : Kind_Type) is record
      case K is
         when Int =>
            I : Integer;
         when Char =>
            C : Character;
         when others =>
            B : Boolean;
      end case;
   end record;

   R : Record_Type := (K => Int, I => 0);

   pragma Test (R.K);
   pragma Test (R.I);
   pragma Test (R.C);
   pragma Test (R.B);
   pragma Test (R.Z);

   subtype Int_And_Char is Kind_Type range Int .. Char;
   subtype Good_Kinds is Int_And_Char;

   type Record_Type_2 (K : Kind_Type) is record
      case K is
         when Good_Kinds =>
            X : Integer;
         when others =>
            Y : Boolean;
      end case;
   end record;

   R2 : Record_Type_2 := (Bool, True);
   pragma Test_Statement;

   subtype Int_And_Bool_1 is Kind_Type
      with Predicate => Int_And_Bool_1 in Int | Bool;

   type Record_Type_3 (K : Kind_Type) is record
      case K is
         when Int_And_Bool_1 =>
            X : Integer;
         when others =>
            Y : Boolean;
      end case;
   end record;

   R3_A : Record_Type_3 := (Int, 12);
   pragma Test_Statement;

   R3_B : Record_Type_3 := (Char, True);
   pragma Test_Statement;

   R3_C : Record_Type_3 := (Bool, 42);
   pragma Test_Statement;
end Foo;
