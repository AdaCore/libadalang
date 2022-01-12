procedure Test is
   generic
      type T is private;
   package Pkg is
      type U is record
         X : T;
      end record;
   end Pkg;

   package My_Pkg is new Pkg (Integer);

   X, Y : My_Pkg.U;
   B : Boolean := My_Pkg."=" (X, Y);
   --% node.f_default_expr.f_name.p_referenced_decl()
   --  We use the inline playground driver in order to see the result as a
   --  SyntheticSubpDecl node. The name-resolution driver would print "None"
   --  even if the resolution succeeds because the synthesized defining does
   --  not have a corresponding source text.
begin
   null;
end Test;
