procedure Foo is

   type UArray_D1 is array (Natural range <>) of Integer;
   type UArray_D2 is array (Natural range <>, Boolean range <>) of Integer;

   type CArray_D1 is array (1 .. 9) of Integer;
   type CArray_D2 is array (1 .. 9, False .. True) of Integer;

   subtype SUArray is UArray_D1;
   subtype SCArray is UArray_D1 (1 .. 9);

   OU1 : UArray_D1 (1 .. 9);
   OU2 : UArray_D2 (1 .. 9, False .. True);

   OC1 : CArray_D1;
   OC2 : CArray_D2;

   OSU : SUArray (1 .. 9);
   OSC : SCArray;

   O_No_Type : No_Type;

begin
   OU1 (1);
   OU1 (1 .. 8);
   OU1 (1, True);
   OU2 (1);
   OU2 (1, True);

   OC1 (1);
   OC1 (1, True);
   OC2 (1);
   OC2 (1, True);

   OSU (1);
   OSU (1, True);
   OSC (1);
   OSC (1, True);
end Foo;
