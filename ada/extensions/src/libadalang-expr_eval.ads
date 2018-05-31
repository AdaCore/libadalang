with GNATCOLL.GMP.Integers;

with Libadalang.Analysis;

--  This package implements an expression evaluator for Libadalang. The aim
--  is to allow at least evaluation of expressions that fit the definitions
--  of static expression in the Ada Reference Manual.

package Libadalang.Expr_Eval is

   package LAL renames Libadalang.Analysis;

   subtype Big_Integer is GNATCOLL.GMP.Integers.Big_Integer;

   type Expr_Kind is (Enum_Lit, Int, Real);

   type Eval_Result (Kind : Expr_Kind) is limited record
      Expr_Type : LAL.Base_Type_Decl;

      case Kind is
         when Enum_Lit =>
            Enum_Result : LAL.Enum_Literal_Decl;
         when Int =>
            Int_Result : Big_Integer;
         when Real =>
            Real_Result : Long_Float;
      end case;
   end record;
   --  This data type represents the result of the evaluation of an expression
   
   function As_Int (Self : Eval_Result) return Big_Integer;
   --  Return the given evaluation result as an Integer, if applicable. This
   --  will work for enum or int results, not for real results.
   
   function Image (Self : Eval_Result) return String;
   --  Return a string representation of Self. Used for testing/debugging
   --  purposes.

   function Expr_Eval (E : LAL.Expr) return Eval_Result;
   --  Evaluate the expression passed as parameter

end Libadalang.Expr_Eval;
