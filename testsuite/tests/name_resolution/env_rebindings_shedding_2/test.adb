procedure Test is
   generic
      type E is private;
   package Common_G is
      generic
      package Op_G is
         procedure Bar (X : E);
      end Op_G;

      generic
         with package Op is new Op_G (<>);
      package Shared_G is
         procedure Foo (X : E);
      end Shared_G;

      generic
         with package Shared is new Shared_G (<>);
      package Iter_G is
         procedure Baz;
      end Iter_G;
   end Common_G;

   package body Common_G is
      package body Op_G is
         procedure Bar (X : E) is null;
      end Op_G;

      package body Shared_G is
         procedure Foo (X : E) is
         begin
            Op.Bar (X);
         end Foo;
      end Shared_G;

      package body Iter_G is
         procedure Baz is null;
      end Iter_G;
   end Common_G;

   generic
      with package Common is new Common_G (<>);
   package Pkg is
      generic
         with package Op is new Common.Op_G (<>);
      package Inner_G is
         procedure Bar;
      end Inner_G;
   end Pkg;

   package body Pkg is
      package body Inner_G is
         package Shared is new Common.Shared_G (Op => Op);
         package Iter is new Common.Iter_G (Shared);

         procedure Bar is null;
      end Inner_G;
   end Pkg;

   package My_Common is new Common_G (Integer);
   package My_Pkg is new Pkg (My_Common);
   --% pkg_body = node.p_designated_generic_decl.p_body_part
   --% iter_inst = pkg_body.findall(lal.GenericPackageInstantiation)[1]
   --% iter_gen = iter_inst.p_designated_generic_decl
   --% shared_formal = iter_gen.find(lal.GenericPackageInstantiation)
   --% shared_gen = shared_formal.p_designated_generic_decl
   --% shared_gen.p_body_part.find(lal.CallStmt).p_resolve_names
begin
   null;
end Test;

