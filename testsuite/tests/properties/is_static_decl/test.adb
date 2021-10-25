procedure Test_It is
   type T is range 1 .. 100;
   type NT is new Test_It.T'Base;
   type Arr is array (1 .. 10) of Integer;
   type Arr_2 is array (Integer range <>) of Integer;
   type Arr_3 is array (Integer range 1 .. 10) of Integer;

   Var : Integer := 10;
   type Arr_4 is array (Integer range 1 .. Var) of Integer;

   type Fixed_T is delta 0.1 range -1.0 .. 1.0;
   type Float_T is digits 3 range -10.0 .. 10.0;

   Fixed_Var : Fixed_T := 0.2;
   Float_Var : Float_T := 0.1;

   subtype Fixed_ST_1 is Fixed_T range -0.5 .. 0.5;
   subtype Fixed_ST_2 is Fixed_T delta 0.2;
   subtype Fixed_ST_3 is Fixed_T delta 0.2 range -0.5 .. 0.5;
   subtype Fixed_ST_4 is Fixed_T delta 0.2 range -Fixed_Var .. Fixed_Var;

   subtype Float_ST_1 is Float_T range -0.5 .. 0.5;
   subtype Float_ST_2 is Float_T digits 2;
   subtype Float_ST_3 is Float_T digits 2 range -0.5 .. 0.5;
   subtype Float_ST_4 is Float_T digits 2 range -Float_Var .. Float_Var;
begin
   null;
end Test_It;
