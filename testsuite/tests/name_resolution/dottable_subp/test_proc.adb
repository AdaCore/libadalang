procedure Test_Proc is
   type T12 is tagged record I : Integer; end record;
   subtype T1 is T12;

   procedure P (A : access T1'Class; B : Boolean := False) is null;

   type T2 is access all T1;

   X : T2 := null;
begin
   X.P;
   pragma Test_Statement;
end Test_Proc;
