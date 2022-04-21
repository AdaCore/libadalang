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

   type C_Type is (
      'A'
      --% $node.p_canonical_text
      ,
      'a'
      --% $node.p_canonical_text
   );
begin
   null;
end Foo;
