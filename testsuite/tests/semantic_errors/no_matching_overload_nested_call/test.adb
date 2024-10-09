procedure Test is
   function Foo (X : Integer) return Integer is (X);
   function Foo (X : Boolean) return Boolean is (X);

   procedure Bar (X : Integer) is null;
begin
   Bar (Foo (2.5));
   pragma Test_Statement (Expect_Fail => True);

   --  Check when alternatives come from Standard
   declare
      generic
         type Int is (<>);
      package G1 is
         subtype T is Int;
      end G1;

      generic
         with package P is new G1 (<>);
      package G2 is
         pragma Assert (P."=" (P.T'First, 0));
         pragma Test_Statement (Expect_Fail => True);
      end G;
   begin
      null;
   end;
end Test;
