procedure Foo is
begin
   if Parent.Parent.Kind
      not in Ada_Begin_Block_Range
           | Ada_Decl_Block_Range
           | Baaaaaaaaaaaaaaaaaar
     and then Parent_Parent.Parent_Parent.Parent_Parent.Parent_Kind
              not in Ada_Begin_Block_Range
                   | Ada_Decl_Block_Range
                   | Bar
     and then Parent_Parent.Parent_Parent.Parent_Parent.Parent_Parent.Parent_Kind
              not in Ada_Begin_Block_Range | Ada_Decl_Block_Range | Baaaaaaaaaar
   then
      Current_Indentation := @ + Indentation;
   end if;
end Foo;
