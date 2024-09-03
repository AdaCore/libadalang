procedure Test is
   generic
      type T is private;
   package Tree_G is
   end Tree_G;

   generic
      with package Tree is new Tree_G (<>);
      use Tree;
   package Impl_G is
      generic
         with function New_Node return T;
      procedure Foo;
   end Impl_G;

   package body Impl_G is
      procedure Foo is
         X : T := New_Node;
      begin
         null;
      end Foo;
   end Impl_G;

   package Tree is new Tree_G (Integer);
   package Impl is new Impl_G (Tree);
   --% impl_body = node.p_designated_generic_decl.p_body_part
   --% obj = impl_body.find(lal.ObjectDecl)
   --% obj.p_resolve_names
   --% obj.f_type_expr.p_designated_type_decl
begin
   null;
end Test;
