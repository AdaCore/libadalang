procedure Test is
   generic
   package Common_G is
      type U is null record;

      generic
         type T is private;
      package Shared_G is
         procedure Foo (X : T);
         procedure Bar (X : U);
      end Shared_G;
   end Common_G;

   package body Common_G is
      package body Shared_G is
         procedure Foo (X : T) is null;
         procedure Bar (X : U) is null;
      end Shared_G;
   end Common_G;

   generic
      with package Common is new Common_G;
   package Pkg is
      type T is null record;

      procedure Foo (X : T);
      procedure Bar (X : Common.U);
   end Pkg;

   package body Pkg is
      package Shared is new Common.Shared_G (T => T);

      procedure Foo (X : T) is
      begin
         Shared.Foo (X);
      end Foo;

      procedure Bar (X : Common.U) is
      begin
         Shared.Bar (X);
      end Bar;
   end Pkg;

   package My_Common is new Common_G;
   package My_Pkg is new Pkg (My_Common);
   --% pkg_body = node.p_designated_generic_decl.p_body_part
   --% pkg_body.findall(lal.CallStmt)[0].p_resolve_names
   --% pkg_body.findall(lal.CallStmt)[1].p_resolve_names
begin
   null;
end Test;


