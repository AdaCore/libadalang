package body Main_Package is
   procedure Bar (A : Integer := 1) is null;
   procedure Baz (A : Integer) renames Bar;
end Main_Package;
