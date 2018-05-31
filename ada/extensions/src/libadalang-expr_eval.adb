with GNATCOLL.GMP.Integers;

use type GNATCOLL.GMP.Integers.Big_Integer;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Sources;  use Libadalang.Sources;

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

   function Copy (Result : Eval_Result) return Eval_Result is
     (case Result.Kind is
      when Enum_Lit => Create_Enum_Result (Result.Expr_Type,
                                           Result.Enum_Result),
      when Int => Create_Int_Result (Result.Expr_Type,
                                     Result.Int_Result),
      when Real => Create_Real_Result (Result.Expr_Type,
                                       Result.Real_Result));

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
         case Kind (D) is
            when LAL.Ada_Enum_Literal_Decl =>

               --  An enum literal declaration evaluates to itself
               return (Enum_Lit,
                       As_Base_Type_Decl
                         (P_Enum_Type (As_Enum_Literal_Decl (D))),
                       As_Enum_Literal_Decl (D));

            when LAL.Ada_Number_Decl =>

               --  A number declaration evaluates to the evaluation of its
               --  expression.
               return Expr_Eval (F_Expr (As_Number_Decl (D)));

            when others =>
               raise LAL.Property_Error;
         end case;
      end Eval_Decl;

      type Range_Attr is (Range_First, Range_Last);

      function Eval_Range_Attr (D : LAL.Ada_Node; A : Range_Attr)
                                return Eval_Result is
      begin
         case Kind (D) is
            when LAL.Ada_Name =>
               return Eval_Range_Attr
                 (As_Ada_Node
                    (P_Referenced_Decl_Internal
                         (As_Name (D), Try_Immediate => True)), A);
            when LAL.Ada_Type_Decl =>
               return Eval_Range_Attr
                 (As_Ada_Node (F_Type_Def (As_Type_Decl (D))), A);
            when LAL.Ada_Type_Def =>
               case Kind (D) is
                  when LAL.Ada_Signed_Int_Type_Def =>
                     declare
                        Rng : LAL.Expr :=
                          F_Range (F_Range (As_Signed_Int_Type_Def (D)));
                     begin
                        case Kind (Rng) is
                           when LAL.Ada_Bin_Op_Range =>
                              declare
                                 BO   : constant LAL.Bin_Op := As_Bin_Op (Rng);
                                 Expr : constant LAL.Expr :=
                                   (case A is
                                    when Range_First => F_Left (BO),
                                    when Range_Last  => F_Right (BO));
                              begin
                                 return Expr_Eval (Expr);
                              end;
                           when others =>
                              raise LAL.Property_Error with "Unsupported range"
                                & " expression: " & Text (Rng);
                        end case;
                     end;
                  when others =>
                     raise LAL.Property_Error with "Cannot get "
                       & A'Img & " attribute of type def " & Kind (D)'Img;
               end case;
            when others =>
               raise LAL.Property_Error with "Cannot eval "
                 & A'Img & " attribute of " & Kind (D)'Img;
         end case;
      end Eval_Range_Attr;

   begin
      case Kind (E) is
         when LAL.Ada_Base_Id | LAL.Ada_Dotted_Name =>

            return Eval_Decl
              (P_Referenced_Decl_Internal
                 (E, Try_Immediate => True));

         when LAL.Ada_Int_Literal =>
            return (Int,
                    As_Base_Type_Decl (P_Universal_Int_Type (E)),
                    P_Denoted_Value (As_Int_Literal (E)));

         when LAL.Ada_Real_Literal =>
            return (Real,
                    As_Base_Type_Decl (P_Universal_Real_Type (E)),
                    Long_Float'Value
                      (Text (As_Real_Literal (E))));

         when LAL.Ada_Bin_Op =>
            declare
               BO : LAL.Bin_Op := As_Bin_Op (E);
               Op : LAL.Op := F_Op (BO);
               L : Eval_Result := Expr_Eval (F_Left (BO));
               R : Eval_Result := Expr_Eval (F_Right (BO));
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
                        (case Kind (Op) is
                         when Ada_Op_Plus => L.Int_Result + R.Int_Result,
                         when Ada_Op_Minus => L.Int_Result - R.Int_Result,
                         when Ada_Op_Mult  => L.Int_Result * R.Int_Result,
                         when Ada_Op_Div   => L.Int_Result / R.Int_Result,
                         when Ada_Op_Pow   =>
                            Raise_To_N (L.Int_Result, R.Int_Result),
                         when others   =>
                           raise Property_Error
                           with "Unhandled operator: " & Kind (Op)'Img));
                  when Real =>
                     return Create_Real_Result
                       (R.Expr_Type,
                        (case Kind (Op) is
                         when Ada_Op_Plus => L.Real_Result + R.Real_Result,
                         when Ada_Op_Minus => L.Real_Result + R.Real_Result,
                         when Ada_Op_Mult  => L.Real_Result * R.Real_Result,
                         when Ada_Op_Div   => L.Real_Result / R.Real_Result,
                         when others   => raise Property_Error));
                  when Enum_Lit =>
                     raise Property_Error;
               end case;

            end;
         when LAL.Ada_Un_Op =>
            declare
               UO          : LAL.Un_Op := As_Un_Op (E);
               Op          : LAL.Op := F_Op (UO);
               Operand_Val : Eval_Result := Expr_Eval (F_Expr (UO));
            begin
               case Kind (Op) is
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
                     with "Unhandled operator: " & Kind (Op)'Img;
               end case;
            end;
         when LAL.Ada_Attribute_Ref =>
            declare
               AR : LAL.Attribute_Ref := As_Attribute_Ref (E);
               Attr : LAL.Identifier := F_Attribute (AR);
               Name : Wide_Wide_String := Canonicalize (Text (Attr)).Symbol;
            begin
               if Name = "first" then
                  return Eval_Range_Attr
                    (As_Ada_Node (F_Prefix (AR)), Range_First);
               elsif Name = "last" then
                   return Eval_Range_Attr
                    (As_Ada_Node (F_Prefix (AR)), Range_Last);
               else
                  raise Property_Error
                    with "Unhandled attribute ref: " & Text (Attr);
               end if;
            end;
         when others =>
            raise Property_Error with "Unhandled node: " & Kind (E)'Img;
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
               raise LAL.Property_Error;
            when Enum_Lit =>
               declare
                  Pos : constant Natural := Child_Index (Self.Enum_Result) + 1;
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
              when Enum_Lit => Short_Image (Self.Enum_Result)) & ">";
   end Image;

end Libadalang.Expr_Eval;
