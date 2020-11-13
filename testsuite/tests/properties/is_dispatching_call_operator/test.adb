procedure Test is
   package Pkg is
      type T is tagged null record;

      function "+" (Self : T) return Boolean is (False);
      function "-" (Self, Other : T) return Boolean is (False);
   end Pkg;

   package Der is
      type U is new Pkg.T with null record;

      overriding function "+" (Self : U) return Boolean is (True);
      overriding function "-" (Self, Other : U) return Boolean is (True);
   end Der;

   use Pkg;
   use Der;

   V : Der.U := (null record);
   X : Pkg.T'Class := V;

   Un_Op_Dispatching : Boolean := Pkg."+" (X);
   --% node.f_default_expr.p_is_dispatching_call()

   Un_Op_Not_Dispatching : Boolean := Der."+" (V);
   --% node.f_default_expr.p_is_dispatching_call()

   Un_Op_Dispatching_2 : Boolean := +X;
   --% node.f_default_expr.p_is_dispatching_call()

   Un_Op_Not_Dispatching_2 : Boolean := +V;
   --% node.f_default_expr.p_is_dispatching_call()

   Bin_Op_Dispatching : Boolean := Pkg."-" (X, X);
   --% node.f_default_expr.p_is_dispatching_call()

   Bin_Op_Not_Dispatching : Boolean := Der."-" (V, V);
   --% node.f_default_expr.p_is_dispatching_call()

   Bin_Op_Dispatching_2 : Boolean := X - X;
   --% node.f_default_expr.p_is_dispatching_call()

   Bin_Op_Not_Dispatching_2 : Boolean := V - V;
   --% node.f_default_expr.p_is_dispatching_call()
begin
   null;
end Test;
