procedure Test is
   type Callback is access procedure (X : Integer);
   type Int_Access is access Integer;

   procedure Foo (X : access Int_Access) is null;
   procedure Foo (X : Callback) is null;

   X : aliased Int_Access;
begin
   Foo (X'Access);
   pragma Test_Statement;
end Test;
