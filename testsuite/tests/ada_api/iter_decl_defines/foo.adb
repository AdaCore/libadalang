procedure Foo is

   --  Decl_Defines used to crash on the Anonymous_Type_Decl in the Char_Set
   --  declaration because it has no name.

   Char_Set : array (Character) of Boolean;

   type Char_Set_Type is array (Character) of Boolean;
begin
   null;
end Foo;
