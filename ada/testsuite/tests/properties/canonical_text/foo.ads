procedure Foo is
   ["03C0"]
   --% $node.p_canonical_text
   : constant := 3.14;
   This_Is_["03C0"]
   --% $node.p_canonical_text
   : constant := 3.14;
   ["000003C0"]_Name
   --% $node.p_canonical_text
   : constant String := "["0003C0"]";
begin
   null;
end Foo;
