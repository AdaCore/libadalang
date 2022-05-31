procedure Test is
   package Pkg is
      type T is null record;

      procedure Prim (X : T);
   end Pkg;

   package body Pkg is
      procedure Foo (X : T) is null;
      procedure Prim (X : T) renames Foo;
   end Pkg;

   package Der is
      type U is new Pkg.T;
   end Der;

   use Der;

   X : U;
begin
   Prim (X);
   --% called=node.f_call.p_referenced_decl()
   --% next=called.p_next_part_for_decl()
   --% renamed=next.f_renames.f_renamed_object
   --% renamed.p_referenced_decl()
end Test;
