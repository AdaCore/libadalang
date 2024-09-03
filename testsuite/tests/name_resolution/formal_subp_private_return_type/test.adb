procedure Test is
   generic
      type T is private;
   package Tree_G is
   end Tree_G;

   generic
      with package Tree is new Tree_G (<>);
   package Impl_G is
      generic
         with function New_Node return Tree.T;
      procedure Foo;
   end Impl_G;

   package body Impl_G is
      procedure Foo is
         X : Tree.T := New_Node;
      begin
         null;
      end Foo;
   end Impl_G;

   package Pkg is
   private
      type T is null record;
      package Tree is new Tree_G (T);
      package Impl is new Impl_G (Tree);

      function New_Node return T is (null record);

      procedure Foo is new Impl.Foo (New_Node);
      --% foo = node.p_designated_generic_decl.p_body_part()
      --% obj = foo.find(lal.ObjectDecl)
      --% obj.p_resolve_names
      --% obj.f_type_expr.p_designated_type_decl
   end Pkg;
begin
   null;
end Test;
