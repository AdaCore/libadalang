------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.   See the  GNU  General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with this software;  see file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNATCOLL.GMP.Integers;

use type GNATCOLL.GMP.Integers.Big_Integer;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;
with Libadalang.Sources;  use Libadalang.Sources;

with Langkit_Support.Text;

package body Libadalang.Expr_Eval is

   function Create_Enum_Result
     (Expr_Type : LAL.Base_Type_Decl;
      Value     : LAL.Enum_Literal_Decl) return Eval_Result
   is ((Kind => Enum_Lit, Expr_Type => Expr_Type, Enum_Result => Value));

   function Create_Int_Result
     (Expr_Type  : LAL.Base_Type_Decl;
      Value      : Big_Integer) return Eval_Result;

   function Create_Real_Result
     (Expr_Type  : LAL.Base_Type_Decl;
      Value      : Long_Float) return Eval_Result
   is ((Kind => Real, Expr_Type => Expr_Type, Real_Result => Value));

   function Copy (Result : Eval_Result) return Eval_Result;

   function Raise_To_N (Left, Right : Big_Integer) return Big_Integer;
   --  Raise Left to the power of Right and return the result. If Right is too
   --  big or if it is negative, raise a Property_Error.

   -----------------------
   -- Create_Int_Result --
   -----------------------

   function Create_Int_Result
     (Expr_Type  : LAL.Base_Type_Decl;
      Value      : Big_Integer) return Eval_Result is
   begin
      return Result : Eval_Result :=
        (Kind => Int, Expr_Type => Expr_Type, Int_Result => <>)
      do
         Result.Int_Result.Set (Value);
      end return;
   end Create_Int_Result;

   function Create_Int_Result
     (Expr_Type  : LAL.Base_Type_Decl;
      Value      : Integer) return Eval_Result is
   begin
      return Create_Int_Result
         (Expr_Type, GNATCOLL.GMP.Integers.Make (Integer'Image (Value)));
   end Create_Int_Result;

   ----------
   -- Copy --
   ----------
   function Copy (Result : Eval_Result) return Eval_Result is
   begin
      case Result.Kind is
         when Enum_Lit =>
            return Create_Enum_Result
              (Result.Expr_Type, Result.Enum_Result);
         when Int =>
            return Create_Int_Result
              (Result.Expr_Type, Result.Int_Result);
         when Real =>
            return Create_Real_Result
              (Result.Expr_Type, Result.Real_Result);
      end case;
   end Copy;

   ----------------
   -- Raise_To_N --
   ----------------

   function Raise_To_N (Left, Right : Big_Integer) return Big_Integer is
      use GNATCOLL.GMP;
      N : Unsigned_Long;
   begin
      if Right < 0 then
         raise Property_Error with "Expected natural exponent";
      end if;

      begin
         N := Unsigned_Long'Value (Right.Image);
      exception
         when Constraint_Error =>
            raise Property_Error with "Exponent is too large";
      end;

      return Left ** N;
   end Raise_To_N;

   ---------------
   -- Expr_Eval --
   ---------------

   function Expr_Eval (E : LAL.Expr) return Eval_Result is

      ---------------
      -- Eval_Decl --
      ---------------

      function Eval_Decl (D : LAL.Basic_Decl) return Eval_Result is
      begin
         if D.Is_Null then
            raise Property_Error with "Invalid decl";
         end if;
         case D.Kind is
            when Ada_Enum_Literal_Decl =>

               --  An enum literal declaration evaluates to itself
               return (Enum_Lit,
                       D.As_Enum_Literal_Decl.P_Enum_Type.As_Base_Type_Decl,
                       D.As_Enum_Literal_Decl);

            when Ada_Number_Decl =>

               --  A number declaration evaluates to the evaluation of its
               --  expression.
               return Expr_Eval (D.As_Number_Decl.F_Expr);

            when Ada_Object_Decl =>
               if D.As_Object_Decl.F_Default_Expr.Is_Null then
                  raise Property_Error with "Object decl does not have "
                    & "a default expression.";
               else
                  return Expr_Eval (D.As_Object_Decl.F_Default_Expr);
               end if;

            when others =>
               raise Property_Error with "Cannot transform decl " &
                  D.Kind'Image;
         end case;
      end Eval_Decl;

      type Range_Attr is (Range_First, Range_Last);

      function Eval_Range_Attr
        (D : LAL.Ada_Node; A : Range_Attr) return Eval_Result is
      begin
         case D.Kind is
            when Ada_Name =>
               return Eval_Range_Attr
                 (D.As_Name.P_Referenced_Decl_Internal (Try_Immediate => True)
                  .As_Ada_Node,
                  A);
            when Ada_Type_Decl =>
               return Eval_Range_Attr (D.As_Type_Decl.F_Type_Def.As_Ada_Node,
                                       A);
            when Ada_Type_Def =>
               case D.Kind is
                  when Ada_Signed_Int_Type_Def =>
                     declare
                        Rng : constant LAL.Expr :=
                          D.As_Signed_Int_Type_Def.F_Range.F_Range;
                     begin
                        case Rng.Kind is
                           when Ada_Bin_Op_Range =>
                              declare
                                 BO   : constant LAL.Bin_Op := Rng.As_Bin_Op;
                                 Expr : constant LAL.Expr :=
                                   (case A is
                                    when Range_First => BO.F_Left,
                                    when Range_Last  => BO.F_Right);
                              begin
                                 return Expr_Eval (Expr);
                              end;
                           when others =>
                              raise Property_Error with "Unsupported range"
                                & " expression: " & Rng.Text;
                        end case;
                     end;
                  when others =>
                     raise Property_Error with "Cannot get "
                       & A'Img & " attribute of type def " & D.Kind'Img;
               end case;
            when others =>
               raise Property_Error with "Cannot eval "
                 & A'Img & " attribute of " & D.Kind'Img;
         end case;
      end Eval_Range_Attr;

   begin
      case E.Kind is
         when Ada_Identifier | Ada_Dotted_Name =>
            return Eval_Decl
              (E.P_Referenced_Decl_Internal (Try_Immediate => True));

         when Ada_Char_Literal =>
            declare
               X : LAL.Basic_Decl := E.P_Referenced_Decl_Internal
                                       (Try_Immediate => True);
            begin
               if X = No_Basic_Decl then
                  return Create_Int_Result
                    (E.P_Expression_Type,
                     Langkit_Support.Text.Character_Type'Pos
                       (As_Char_Literal (E).P_Denoted_Value));
               else
                  return Eval_Decl
                    (E.P_Referenced_Decl_Internal (Try_Immediate => True));
               end if;
            end;

         when Ada_Int_Literal =>
            return (Int,
                    E.P_Universal_Int_Type.As_Base_Type_Decl,
                    E.As_Int_Literal.P_Denoted_Value);

         when Ada_Real_Literal =>
            return (Real,
                    E.P_Universal_Real_Type.As_Base_Type_Decl,
                    Long_Float'Value (E.Text));

         when Ada_Bin_Op =>
            declare
               BO : constant LAL.Bin_Op := E.As_Bin_Op;
               Op : constant LAL.Op := BO.F_Op;
               L  : constant Eval_Result := Expr_Eval (BO.F_Left);
               R  : constant Eval_Result := Expr_Eval (BO.F_Right);
            begin
               if L.Kind /= R.Kind then
                  raise Property_Error;
                  --  TODO??? There are actually some rules about implicit
                  --  conversions that we might have to implement someday.
               end if;

               case R.Kind is
                  when Int =>
                     return Create_Int_Result
                       (R.Expr_Type,
                        (case Op.Kind is
                         when Ada_Op_Plus => L.Int_Result + R.Int_Result,
                         when Ada_Op_Minus => L.Int_Result - R.Int_Result,
                         when Ada_Op_Mult  => L.Int_Result * R.Int_Result,
                         when Ada_Op_Div   => L.Int_Result / R.Int_Result,
                         when Ada_Op_Pow   =>
                            Raise_To_N (L.Int_Result, R.Int_Result),
                         when others   =>
                           raise Property_Error
                           with "Unhandled operator: " & Op.Kind'Img));
                  when Real =>
                     return Create_Real_Result
                       (R.Expr_Type,
                        (case Op.Kind is
                         when Ada_Op_Plus  => L.Real_Result + R.Real_Result,
                         when Ada_Op_Minus => L.Real_Result + R.Real_Result,
                         when Ada_Op_Mult  => L.Real_Result * R.Real_Result,
                         when Ada_Op_Div   => L.Real_Result / R.Real_Result,
                         when others   => raise Property_Error));
                  when Enum_Lit =>
                     raise Property_Error;
               end case;

            end;

         when Ada_Un_Op =>
            declare
               UO          : constant LAL.Un_Op := E.As_Un_Op;
               Op          : constant LAL.Op := UO.F_Op;
               Operand_Val : constant Eval_Result := Expr_Eval (UO.F_Expr);
            begin
               case Op.Kind is
                  when Ada_Op_Minus =>
                     case Operand_Val.Kind is
                        when Int =>
                           return Create_Int_Result
                             (Operand_Val.Expr_Type, -Operand_Val.Int_Result);
                        when Real =>
                           return Create_Real_Result
                             (Operand_Val.Expr_Type, -Operand_Val.Real_Result);
                        when Enum_Lit =>
                           raise Property_Error;
                     end case;
                  when Ada_Op_Plus =>
                     return Copy (Operand_Val);
                  when others =>
                     raise Property_Error
                     with "Unhandled operator: " & Op.Kind'Img;
               end case;
            end;

         when Ada_Attribute_Ref =>
            declare
               AR   : constant LAL.Attribute_Ref := E.As_Attribute_Ref;
               Attr : constant LAL.Identifier := AR.F_Attribute;
               Name : constant Wide_Wide_String :=
                  Canonicalize (Attr.Text).Symbol;
            begin
               if Name = "first" then
                  return Eval_Range_Attr
                    (As_Ada_Node (AR.F_Prefix), Range_First);
               elsif Name = "last" then
                   return Eval_Range_Attr
                    (As_Ada_Node (AR.F_Prefix), Range_Last);
               else
                  raise Property_Error
                    with "Unhandled attribute ref: " & Attr.Text;
               end if;
            end;
         when Ada_Paren_Expr =>
            return Expr_Eval (E.As_Paren_Expr.F_Expr);
         when others =>
            raise Property_Error with "Unhandled node: " & E.Kind'Img;
      end case;
   end Expr_Eval;

   ------------
   -- As_Int --
   ------------

   function As_Int (Self : Eval_Result) return Big_Integer is
   begin
      return Result : Big_Integer do
         case Self.Kind is
            when Int =>
               Result.Set (Self.Int_Result);
            when Real =>
               raise Property_Error;
            when Enum_Lit =>
               declare
                  Pos : constant Natural := Self.Enum_Result.Child_Index;
               begin
                  Result.Set (GNATCOLL.GMP.Long (Pos));
               end;
         end case;
      end return;
   end As_Int;

   -----------
   -- Image --
   -----------

   function Image (Self : Eval_Result) return String is
   begin
      return "<Eval_Result "
        & Self.Kind'Image & " "
        & (case Self.Kind is
           when Int => Self.Int_Result.Image,
           when Real => Self.Real_Result'Image,
           when Enum_Lit => Self.Enum_Result.Short_Image) & ">";
   end Image;

end Libadalang.Expr_Eval;
