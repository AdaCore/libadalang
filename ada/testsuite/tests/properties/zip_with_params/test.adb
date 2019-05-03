procedure Main is
   procedure Foo (X : Integer; Y, Z : Boolean);

   type Array_Type is array (Integer range 1 .. 3) of Integer;

   generic
      type A is private;
      type B is private;
   package Pkg is
   end Pkg;

   package Inst_1 is new Pkg (Integer, Boolean);
   package Inst_2 is new Pkg (B => Boolean, A => Integer);

   type R (X : Integer; Y, Z : Boolean) is record
      null;
   end record;

   X_1 : R (Y => True, Z => False, X => 3);
   X_2 : R (42, Y => True, Z => False);
   X_3 : R (42, True, False);

   A : Array_Type;
   Tmp : Integer := A (2);

   type Expr_Kind is (Un_Op, Bin_Op, Lit);
   type Expr_Type;

   type Expr is access all Expr_Type;

   type Expr_Type (K : Expr_Kind) is record
      case K is
         when Un_Op => Target : Expr;
         when Bin_Op => Left, Right : Expr;
         when Lit => Val : Integer;
      end case;
   end record;

   E : Expr_Type := (Bin_Op, new Expr_Type'(K => Lit, Val => 12),
                             new Expr_Type'(Lit, 12));

begin
   Foo (Y => True, Z => False, X => 3);
   Foo (42, Y => True, Z => False);
   Foo (42, True, False);
end Main;
