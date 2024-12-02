--
--  Copyright (C) 2014-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with GNATCOLL.GMP; use GNATCOLL.GMP;
with GNATCOLL.GMP.Integers;
with GNATCOLL.GMP.Rational_Numbers;

with Libadalang.Analysis;

with Langkit_Support.Text; use Langkit_Support.Text;

--  This package implements an expression evaluator for Libadalang. The aim
--  is to allow at least evaluation of expressions that fit the definitions
--  of static expression in the Ada Reference Manual.

package Libadalang.Expr_Eval is

   package LAL renames Libadalang.Analysis;

   subtype Big_Integer is GNATCOLL.GMP.Integers.Big_Integer;

   subtype Rational is GNATCOLL.GMP.Rational_Numbers.Rational;

   subtype Double is GNATCOLL.GMP.Double;

   type Expr_Kind is (Enum_Lit, Int, Real, String_Lit);

   type Eval_Result (Kind : Expr_Kind := Int) is limited record
      Expr_Type : LAL.Base_Type_Decl;

      case Kind is
         when Enum_Lit =>
            Enum_Result : LAL.Enum_Literal_Decl;
         when Int =>
            Int_Result : Big_Integer;
         when Real =>
            Real_Result : Rational;
         when String_Lit =>
            String_Result : Unbounded_Text_Type;
            --  First and Last are used to keep track of the String objects
            --  bounds (this information is required to evaluate slices).
            First, Last   : Natural;
      end case;
   end record;
   --  This data type represents the result of the evaluation of an expression

   function As_Int (Self : Eval_Result) return Big_Integer;
   --  Return the given evaluation result as an Integer, if applicable. This
   --  will work for enum or int results, not for real results.

   function As_String (Self : Eval_Result) return Unbounded_Text_Type;
   --  Return the given evaluation result as a string, if applicable. This
   --  will only work for string results.

   function Image (Self : Eval_Result) return String;
   --  Return a string representation of Self. Used for testing/debugging
   --  purposes.

   function Expr_Eval (E : LAL.Expr) return Eval_Result;
   --  Evaluate the expression passed as parameter

   function Expr_Eval_In_Env
     (E : LAL.Expr; Env : LAL.Substitution_Array) return Eval_Result;
   --  Evaluate the given expression in the context of the given environment

end Libadalang.Expr_Eval;
