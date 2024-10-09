procedure Test is
   package Super is
      type T1 is tagged null record;
      procedure P (V : T1);

      type T2 is new T1 with null record;

      type T3 is new T2 with null record;
      procedure P (V : T3);

      type T4 is new T3 with null record;
      procedure P (V : T4);
   end Super;

   package body Super is
      procedure P (V : T1) is null;
      procedure P (V : T3) is null;
      procedure P (V : T4) is null;
   end Super;

   use Super;

   procedure Call (
      V2 : T2'Class;
      V3 : T3'Class;
      V4 : T4'Class) is
   begin
      --  Non-dispatching call to T1.P

      V2'Super.P;
      pragma Test_Statement;

      --  Non-dispatching call to T1.P

      V3'Super.P;
      pragma Test_Statement;

      --  Non-dispatching call to T3.P

      V4'Super.P;
      pragma Test_Statement;

      --  Non-dispatching call to T1.P

      V4'Super'Super.P;
      pragma Test_Statement;
   end;

   type Ref is access Super.T2'Class;
   Ptr : Ref := new Super.T3;

   type T5 is null record;
   procedure P (V : T5) is null;
begin
   --  Non-dispatching call to T1.P

   Ptr'Super.P;
   pragma Test_Statement;

   --  Super can only be applied on tagged type

   T5'Super.P;
   pragma Test_Statement (Expect_Fail => True);
end;
