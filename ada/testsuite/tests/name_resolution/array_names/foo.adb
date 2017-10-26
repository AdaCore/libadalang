--  Test name resolution on various CallExpr nodes. In particular, this tests
--  the filtering of object declarations that are arrays depending on the
--  arguments provided to the CallExpr.

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
   pragma Test (OU1 (1));
   pragma Test (OU1 (1 .. 8));
   pragma Test (OU1 (1, True));
   pragma Test (OU2 (1));
   pragma Test (OU2 (1, True));

   pragma Test (OC1 (1));
   pragma Test (OC1 (1, True));
   pragma Test (OC2 (1));
   pragma Test (OC2 (1, True));

   pragma Test (OSU (1));
   pragma Test (OSU (1, True));
   pragma Test (OSC (1));
   pragma Test (OSC (1, True));
end Foo;
