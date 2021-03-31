package Pkg is
   procedure Foo;

   type U is private;
   --% node.p_all_parts()

   procedure Bar
     (X : Integer --% node.parent.parent.p_all_parts()
     );

private
   type T;
   --% node.p_all_parts()

   type U is null record;
end Pkg;
