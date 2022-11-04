procedure Ea is
   type T1 (D1 : Boolean;
            D2, D3 : Positive) is
      tagged record
         C1 : String (1 .. D2);
         C2 : String (1 .. D3);
         case D1 is
            when True =>
               C3 : Integer;
            when False =>
               C4 : Positive;
         end case;
      end record;

   type T2 is new T1 with record
      C5 : Float;
   end record;

   type T3 is new T2 with record
      C6 : Long_Float;
   end record;

   type T4 is new T3 with record
      C7 : Boolean;
   end record;

   I1 : T1 := (True, 1, 4, "1", "four", 0);
   pragma Test_Statement;

   I2 : T2 := (I1 with 3.14);
   pragma Test_Statement;

   I2_2 : T2 := (True, 1, 4, "1", "four", 0, 3.14);
   pragma Test_Statement;

   I3 : T3 := (I1 with 3.14, 3.1416);
   pragma Test_Statement;

   I4 : T3 := (I1 with C6 => 3.14, C5 => 3.1416);
   pragma Test_Statement;

   I5 : T4 := (I1 with 3.14, 3.1416, True);
   pragma Test_Statement;
begin
   null;
end Ea;
