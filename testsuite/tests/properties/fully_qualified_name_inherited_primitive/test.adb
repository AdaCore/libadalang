procedure Test is
   package Pkg is
      type T is tagged null record;

      procedure Foo (X : T) is null;
   end Pkg;

   package Der is
      type U is new Pkg.T with null record;
      --% inherited_foo = node.p_get_primitives()[0]
      --% inherited_foo.p_fully_qualified_name
   end Der;

   generic
   package Gen is
      type V is new Pkg.T with null record;
   end Gen;

   package My_Gen is new Gen;
   --% v_def = node.p_designated_generic_decl.find(lal.BaseTypeDecl)
   --% inherited_foo = v_def.p_get_primitives()[0]
   --% inherited_foo.p_fully_qualified_name

   package Gen_Der is
      type W is new My_Gen.V with null record;
      --% inherited_foo = node.p_get_primitives()[0]
      --% inherited_foo.p_fully_qualified_name
   end Gen_Der;

   procedure In_Subp is
      type S is new Pkg.T with null record;
      --% inherited_foo = node.p_get_primitives()[0]
      --% inherited_foo.p_fully_qualified_name
   begin
      null;
   end In_Subp;
begin
   null;
end Test;
