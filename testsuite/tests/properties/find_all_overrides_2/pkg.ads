package Pkg is
   type T is tagged limited private;

   procedure Foo (X : T) is null;
   --% node.p_find_all_overrides([node.unit])
private
   type T is tagged limited record
      I : Integer;
   end record;
end Pkg;
