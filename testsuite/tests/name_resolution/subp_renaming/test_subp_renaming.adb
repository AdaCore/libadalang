procedure Test_Subp_Renaming is
   procedure Foo (A : Float) is null;
   procedure Foo (A : Integer) is null;

   procedure Bar (I : Float) renames Foo;
begin
   null;
end Test_Subp_Renaming;
pragma Test_Block;
