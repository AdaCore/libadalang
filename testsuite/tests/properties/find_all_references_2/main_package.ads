package Main_Package is
   procedure Bar (A : Integer := 1);
   pragma Find_All_References (Any, "Bar", Follow_Renamings => True);
   procedure Baz (A : Integer);
   procedure Foo (A : Integer) renames Bar;
end Main_Package;
