package Foo is
   type Myint is new Integer;   --# decl
   function F return Myint is (1);
   X : Myint := F;             --# decl
   procedure Set_X (V : Myint); --# decl
end;
