procedure Test is
   generic
      type T is range <>;
      Var : in out T;
   procedure Gen;

   procedure Gen is
   begin
      Var := 1;
   end Gen;

   type T is new Integer range 1 .. 10;

   X : T;

   procedure P is new Gen (T, X);
   --% gen=node.p_designated_generic_decl
   --% obj=gen.f_formal_part.find(lal.ObjectDecl)
   --% obj.p_fully_qualified_name
   --% obj.p_unique_identifying_name
begin
   P;
end Test;
