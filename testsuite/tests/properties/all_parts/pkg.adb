package body Pkg is
   type T is null record;

   procedure Foo is separate;
   --% node.p_all_parts()

   procedure Bar (X : Integer) is null;
end Pkg;
