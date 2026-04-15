procedure Foo is
begin
   if P1.P2.Kind not in Ada_Begin_Block_Range
           | Ada_Decl_Block_Range
           | Baaaaaaaaaaaaaaaaaar
     and then P1.P2.P3.Parent_Kind not in Ada_Begin_Block_Range
                   | Ada_Decl_Block_Range
                   | Bar
     and then
       P1.P2.P3.P4.Parent_Kiiiiiiind
       not in Ada_Begin_Block_Range | Ada_Decl_Block_Range | Baaaaaaaaaaaaaaaaar
   then
      Current_Indentation := @ + Indentation;
   end if;
end Foo;
