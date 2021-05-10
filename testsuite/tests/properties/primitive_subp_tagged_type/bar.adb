package body Bar is
   procedure Foo is
      type T is tagged null record;

      procedure Lol (X : T);
      --% node.p_subp_spec_or_null().p_primitive_subp_tagged_type()
   begin
      null;
   end Foo;
end Bar;
