with Pkg; use Pkg;

procedure Test is
   X : Integer := Foo (2);
   pragma Test_Statement (Expect_Fail => True);
begin
   null;
end Test;
