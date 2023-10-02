--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.GMP.Integers.Misc;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;
with Libadalang.Sources;  use Libadalang.Sources;

package body Libadalang.Expr_Eval is

   use type GNATCOLL.GMP.Integers.Big_Integer;
   use type GNATCOLL.GMP.Rational_Numbers.Rational;

   function "+" (S : Wide_Wide_String) return Unbounded_Wide_Wide_String
                 renames To_Unbounded_Wide_Wide_String;

   function Create_Enum_Result
     (Expr_Type : LAL.Base_Type_Decl;
      Value     : LAL.Enum_Literal_Decl) return Eval_Result;
   --  Helper to create Eval_Result values to wrap enumeration literals

   function Create_Int_Result
     (Expr_Type  : LAL.Base_Type_Decl;
      Value      : Big_Integer) return Eval_Result;
   function Create_Int_Result
     (Expr_Type  : LAL.Base_Type_Decl;
      Value      : Integer) return Eval_Result;
   --  Helpers to create Eval_Result values to wrap integers

   function Create_Real_Result
     (Expr_Type  : LAL.Base_Type_Decl;
      Value      : Rational) return Eval_Result;
   --  Helper to create Eval_Result values to wrap real numbers

   function Create_Bool_Result
     (Value : Boolean; N : LAL.Ada_Node'Class) return Eval_Result;
   --  Helper to create an Eval_Result with Enum_Lit_Kind which denotes
   --  the standard True or False literal decls from the standard Boolean
   --  type.
   --  todo: N is only used to access the fake free function P_Std_Entity.

   function Create_Result_From_Subst
     (Expr_Type  : LAL.Base_Type_Decl;
      Value      : Big_Integer) return Eval_Result;
   --  Helper to create Eval_Result values from a given substitution value.
   --  The resulting Eval_Result's Kind will depend on the given Expr_Type:
   --   - If the Expr_Type is an rnum Type, the Kind will be Enum_Lit.
   --   - If the Expr_Type is a real Type, the Kind will be Real.
   --   - If the Expr_Type is a integer Type, the Kind will be Int.

   procedure Raise_To_N (Left, Right : Big_Integer; Result : out Big_Integer);
   --  Raise Left to the power of Right and return the result. If Right is too
   --  big or if it is negative, raise a Property_Error.

   function To_Integer (Big_Int : Big_Integer) return Integer;
   --  Convert a Big_Integer to an Integer

   function As_Bool (Self : Eval_Result) return Boolean;
   --  Return ``Self`` as a Boolean, if it is indeed of type
   --  ``Standard.Boolean``.

   function Is_Std_Char_Type (Node : LAL.Base_Type_Decl) return Boolean;
   --  Return whether ``Node`` is a standard character type

   ----------------------
   -- Is_Std_Char_Type --
   ----------------------

   function Is_Std_Char_Type (Node : LAL.Base_Type_Decl) return Boolean is
   begin
      --  Note: The condition below was previously implemented with
      --  a membership expression ``Node_Type in Std_Char_Type | ...``
      --  but Ada specifies that the predefined `"="` operator must be
      --  used in that case even if user-defined operator hides it.
      --  However, the predefined operator is too strict for us in
      --  this case: we want the comparison to discard irrelevant
      --  metadata (like how the node was retrieved) and so need to
      --  make sure the custom equality operators are called instead.

      return (Node = Node.P_Std_Char_Type
              or else Node = Node.P_Std_Wide_Char_Type
              or else Node = Node.P_Std_Wide_Wide_Char_Type);
   end Is_Std_Char_Type;

   ------------------------
   -- Create_Enum_Result --
   ------------------------

   function Create_Enum_Result
     (Expr_Type : LAL.Base_Type_Decl;
      Value     : LAL.Enum_Literal_Decl) return Eval_Result is
   begin
      return ((Kind        => Enum_Lit,
               Expr_Type   => Expr_Type,
               Enum_Result => Value));
   end Create_Enum_Result;

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

   ------------------------
   -- Create_Real_Result --
   ------------------------

   function Create_Real_Result
     (Expr_Type  : LAL.Base_Type_Decl;
      Value      : Rational) return Eval_Result is
   begin
      return Result : Eval_Result :=
        (Kind => Real, Expr_Type => Expr_Type, Real_Result => <>)
      do
         Result.Real_Result.Set (Value);
      end return;
   end Create_Real_Result;

   ------------------------------
   -- Create_Result_From_Subst --
   ------------------------------

   function Create_Result_From_Subst
     (Expr_Type  : LAL.Base_Type_Decl;
      Value      : Big_Integer) return Eval_Result is
   begin
      if Expr_Type.P_Is_Enum_Type then
         declare
            Base_Type_Decl : constant LAL.Type_Decl :=
               P_Base_Subtype (Expr_Type).As_Type_Decl;

            Enum_Def : constant LAL.Enum_Type_Def :=
               F_Type_Def (Base_Type_Decl).As_Enum_Type_Def;
         begin
            return Create_Enum_Result
              (Expr_Type,
               Child
                 (F_Enum_Literals (Enum_Def),
                  Positive (To_Integer (Value) + 1)).As_Enum_Literal_Decl);
         end;
      end if;
      return Create_Int_Result (Expr_Type, Value);
   end Create_Result_From_Subst;

   ------------------------
   -- Create_Bool_Result --
   ------------------------

   function Create_Bool_Result
     (Value : Boolean; N : LAL.Ada_Node'Class) return Eval_Result
   is
      --  Get the standard Boolean type declaration
      Bool_Type : constant LAL.Base_Type_Decl :=
         N.P_Std_Entity (+"Boolean").As_Base_Type_Decl;
   begin
      --  Get the enumerator value declaration corresponding to
      --  Result in Standard's Boolean.
      return Create_Enum_Result
        (Bool_Type,
         N.P_Std_Entity (+To_Text (Value'Image))
         .As_Enum_Literal_Decl);
   end Create_Bool_Result;

   ----------------
   -- Raise_To_N --
   ----------------

   procedure Raise_To_N (Left, Right : Big_Integer; Result : out Big_Integer)
   is
      N : Unsigned_Long;
   begin
      if Right < 0 then
         raise Property_Error with "Exponent must be positive";
      end if;

      begin
         N := Unsigned_Long'Value (Right.Image);
      exception
         when Constraint_Error =>
            raise Property_Error with "Exponent is too large";
      end;

      Result.Set (Left ** N);
   end Raise_To_N;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Big_Int : Big_Integer) return Integer is
   begin
      return Integer'Value (Big_Int.Image);
   exception
      when Constraint_Error =>
         raise Property_Error with "out of range big integer";
   end To_Integer;

   ---------------
   -- Expr_Eval --
   ---------------

   function Expr_Eval (E : LAL.Expr) return Eval_Result is
   begin
      return Expr_Eval_In_Env (E, (1 .. 0 => <>));
   end Expr_Eval;

   ----------------------
   -- Expr_Eval_In_Env --
   ----------------------

   function Expr_Eval_In_Env
     (E : LAL.Expr; Env : LAL.Substitution_Array) return Eval_Result
   is
      type Range_Attr is (Range_First, Range_Last);
      --  Reference to either the 'First or the 'Last attribute

      function Eval_Decl (D : LAL.Basic_Decl) return Eval_Result;
      --  Helper to evaluate the value associated to a declaration

      function Eval_Range_Attr
        (D : LAL.Ada_Node; A : Range_Attr) return Eval_Result;
      --  Helper to evaluate a 'First or 'Last attribute reference

      function Eval_Function_Attr
        (AR : LAL.Attribute_Ref; Args : LAL.Assoc_List) return Eval_Result;
      --  Helper to evaluate function attribute references

      function Eval_Array_Index
        (Call_Expr : LAL.Call_Expr; Index : LAL.Expr) return Eval_Result;
      --  Helper to evaluate array indexes

      function Eval_Array_Slice
        (Call_Expr : LAL.Call_Expr; Bounds : LAL.Bin_Op) return Eval_Result;
      --  Helper to evaluate function attribute references

      function Expr_Eval (E : LAL.Expr) return Eval_Result;
      --  Helper to evaluate the given expr in the current environment. Note
      --  that this is a regular function (instead of an expression function)
      --  to workaround a GNAT bug.

      ---------------
      -- Eval_Decl --
      ---------------

      function Eval_Decl (D : LAL.Basic_Decl) return Eval_Result is
      begin
         if D.Is_Null then
            raise Property_Error with "Invalid decl";
         end if;

         --  Check if the environment contains a substitution for the given
         --  basic declaration. If so, return the value from the substitution.
         for Subst of Env loop
            if From_Decl (Subst) = D then
               return Create_Result_From_Subst
                 (Expr_Type => Value_Type (Subst).As_Base_Type_Decl,
                  Value => To_Value (Subst));
            end if;
         end loop;

         case D.Kind is
            when Ada_Enum_Literal_Decl =>

               --  An enum literal declaration evaluates to itself
               return (Enum_Lit,
                       D.As_Enum_Literal_Decl.P_Enum_Type.As_Base_Type_Decl,
                       D.As_Enum_Literal_Decl);

            when Ada_Synthetic_Char_Enum_Lit =>

               --  A synthesized character enum declaration evaluates to the
               --  evaluation of its expression.
               return Expr_Eval
                 (D.As_Synthetic_Char_Enum_Lit.P_Expr.As_Expr);

            when Ada_Number_Decl =>

               --  A number declaration evaluates to the evaluation of its
               --  expression.
               return Expr_Eval (D.As_Number_Decl.F_Expr);

            when Ada_Object_Decl_Range =>
               if not D.As_Object_Decl.F_Renaming_Clause.Is_Null then
                  return Expr_Eval
                    (D.As_Object_Decl.F_Renaming_Clause
                     .F_Renamed_Object.As_Expr);
               elsif not D.As_Object_Decl.F_Default_Expr.Is_Null then
                  return Expr_Eval (D.As_Object_Decl.F_Default_Expr);
               else
                  raise Property_Error with "Object decl does not have "
                    & "a default expression nor a renaming clause.";
               end if;

            when Ada_Anonymous_Expr_Decl =>
               return Expr_Eval (D.As_Anonymous_Expr_Decl.F_Expr);

            when Ada_Synthetic_Type_Predicate_Object_Decl =>
               return Eval_Decl (D.Parent.As_Basic_Decl);

            when others =>
               raise Property_Error
                 with "Cannot eval decl " & D.Kind'Image;
         end case;
      end Eval_Decl;

      ---------------------
      -- Eval_Range_Attr --
      ---------------------

      function Eval_Range_Attr
        (D : LAL.Ada_Node; A : Range_Attr) return Eval_Result is
      begin
         if D.Is_Null then
            raise Property_Error with "Cannot resolve attribute prefix";
         end if;

         case D.Kind is
         when Ada_Name =>
            return Eval_Range_Attr
              (D.As_Name.P_Referenced_Decl.As_Ada_Node, A);

         when Ada_Type_Decl =>
            return Eval_Range_Attr
              (D.As_Type_Decl.F_Type_Def.As_Ada_Node, A);

         when Ada_Subtype_Decl =>
            declare
               Subtype_Indication : constant LAL.Subtype_Indication :=
                  D.As_Subtype_Decl.F_Subtype;
               Constraint         : constant LAL.Range_Constraint :=
                  Subtype_Indication.F_Constraint.As_Range_Constraint;

               --  If the subtype declaration has a range constraint, evaluate
               --  this constraint. Else, recurse on the designated subtype.
               Target : constant LAL.Ada_Node :=
                 (if Constraint.Is_Null
                  then Subtype_Indication.P_Designated_Type_Decl.As_Ada_Node
                  else Constraint.F_Range.F_Range.As_Ada_Node);
            begin
               return Eval_Range_Attr (Target, A);
            end;

         when Ada_Bin_Op_Range =>
            declare
               BO   : constant LAL.Bin_Op := D.As_Bin_Op;
               Expr : constant LAL.Expr :=
                 (case A is
                  when Range_First => BO.F_Left,
                  when Range_Last  => BO.F_Right);
            begin
               return Expr_Eval (Expr);
            end;

         when Ada_Type_Def =>
            case D.Kind is
            when Ada_Derived_Type_Def =>
               declare
                  Cst  : constant LAL.Constraint :=
                     D.As_Derived_Type_Def.F_Subtype_Indication.F_Constraint;

                  --  If the derived type declaration has a range constraint,
                  --  evaluate it. Otherwise, recurse on the base type.
                  Target : constant Ada_Node :=
                    (if Cst.Is_Null
                     then D.Parent.As_Base_Type_Decl.P_Base_Type.As_Ada_Node
                     else Cst.As_Range_Constraint.F_Range.F_Range.As_Ada_Node);
               begin
                  return Eval_Range_Attr (Target, A);
               end;
            when Ada_Signed_Int_Type_Def =>
               return Eval_Range_Attr
                 (D.As_Signed_Int_Type_Def.F_Range.F_Range.As_Ada_Node, A);
            when Ada_Enum_Type_Def =>
               declare
                  Lits      : constant LAL.Enum_Literal_Decl_List :=
                    D.As_Enum_Type_Def.F_Enum_Literals;
                  Lit_Index : constant Positive :=
                    (case A is
                     when Range_First => Lits.First_Child_Index,
                     when Range_Last  => Lits.Last_Child_Index);
                  Char_Pos  : Natural;
               begin
                  if Is_Std_Char_Type (D.Parent.As_Base_Type_Decl) then
                     --  Due to how we define the Character type in our
                     --  artifical __standard unit (and its
                     --  Wide_Character and Wide_Wide_Character
                     --  variants), the 'First and 'Last attributes cannot
                     --  return an Enum_Literal_Decl since they are not
                     --  defined. In order to not fail the Eval_As_Int
                     --  function, we return the corresponding Integer
                     --  value instead.
                     Char_Pos :=
                       (case A is
                        when Range_First =>
                           Support.Text.Character_Type'Pos
                           (Support.Text.Character_Type'First),
                        when Range_Last  =>
                          (if D.P_Std_Char_Type
                              .As_Base_Type_Decl = D.Parent.As_Base_Type_Decl
                           then
                              Character'Pos (Character'Last)
                           elsif D.P_Std_Wide_Char_Type
                                 .As_Base_Type_Decl =
                                 D.Parent.As_Base_Type_Decl
                           then
                              Wide_Character'Pos (Wide_Character'Last)
                           else
                              Support.Text.Character_Type'Pos
                              (Support.Text.Character_Type'Last)));

                     return Create_Int_Result (D.Parent.As_Base_Type_Decl,
                                               Char_Pos);
                  else
                     return Eval_Decl (Lits.Child (Lit_Index).As_Basic_Decl);
                  end if;
               end;
            when Ada_Decimal_Fixed_Point_Def =>
               declare
                  Def : constant LAL.Decimal_Fixed_Point_Def :=
                     D.As_Decimal_Fixed_Point_Def;

                  Rng : constant LAL.Range_Spec := Def.F_Range;
               begin
                  --  If a range has been specified we simply recurse on it,
                  --  otherwise we need to manually compute its bounds using
                  --  the `digits` and `delta` values specified for this fixed
                  --  point type definition.
                  if Rng.Is_Null then
                     declare
                        Delta_Res : constant Eval_Result :=
                           Expr_Eval (Def.F_Delta);

                        Delta_Val : constant Double :=
                          (if Delta_Res.Kind in Real
                           then Delta_Res.Real_Result.To_Double
                           else raise Property_Error with
                              "delta must be real");

                        Digits_Res : constant Eval_Result :=
                           Expr_Eval (Def.F_Digits);

                        Digits_Val : constant Integer :=
                          (if Digits_Res.Kind in Int
                           then To_Integer (Digits_Res.Int_Result)
                           else raise Property_Error with
                              "digits must be an integer");

                        Bound : constant Double :=
                          (if Digits_Val > 0 and Delta_Val > 0.0
                           then (10.0 ** Digits_Val - 1.0) * Delta_Val
                           else raise Property_Error with
                              "delta and digits must be positive");
                     begin
                        return Result : Eval_Result :=
                          (Kind        => Real,
                           Expr_Type   => D.Parent.As_Base_Type_Decl,
                           Real_Result => <>)
                        do
                           Result.Real_Result.Set
                             (case A is
                              when Range_First => -Bound,
                              when Range_Last => Bound);
                        end return;
                     end;
                  else
                     return Eval_Range_Attr (Rng.F_Range.As_Ada_Node, A);
                  end if;
               end;
            when Ada_Ordinary_Fixed_Point_Def =>
               return Eval_Range_Attr
                 (D.As_Ordinary_Fixed_Point_Def.F_Range.F_Range.As_Ada_Node,
                  A);

            when others =>
               raise Property_Error with
                  "Cannot get " & A'Image & " attribute of type def "
                  & D.Kind'Image;
            end case;

         when Ada_Object_Decl =>
            declare
               Val    : constant Eval_Result := Eval_Decl (D.As_Basic_Decl);
               Typ    : constant LAL.Base_Type_Decl :=
                  D.As_Object_Decl.P_Type_Expression.P_Designated_Type_Decl;
               Result : Big_Integer;
            begin
               if Val.Kind /= String_Lit then
                  raise Property_Error with
                    "Cannot eval " & A'Image & " on " & Val.Kind'Image;
               end if;

               case A is
               when Range_First => Result.Set (GNATCOLL.GMP.Long (Val.First));
               when Range_Last => Result.Set (GNATCOLL.GMP.Long (Val.Last));
               end case;

               return Create_Int_Result (Typ, Result);
            end;

         when others =>
            raise Property_Error with
               "Cannot eval " & A'Image & " attribute of " & D.Kind'Image;
         end case;
      end Eval_Range_Attr;

      ------------------------
      -- Eval_Function_Attr --
      ------------------------

      function Eval_Function_Attr
        (AR : LAL.Attribute_Ref; Args : LAL.Assoc_List) return Eval_Result
      is
         Attr : constant LAL.Identifier := AR.F_Attribute;
         Name : constant Wide_Wide_String :=
            Canonicalize (Attr.Text).Symbol;
      begin
         if Name in "min" | "max" then
            if Args.Is_Null or else Args.Children_Count /= 2 then
               raise Property_Error with
                  "'Min/'Max require exactly two arguments";
            end if;

            declare
               Typ   : constant Base_Type_Decl :=
                 AR.F_Prefix.P_Name_Designated_Type;
               Val_1 : constant Eval_Result :=
                 Expr_Eval (Args.Child (1).As_Param_Assoc.F_R_Expr);
               Val_2 : constant Eval_Result :=
                 Expr_Eval (Args.Child (2).As_Param_Assoc.F_R_Expr);
            begin
               if Val_1.Kind /= Val_2.Kind then
                  raise Property_Error with
                     "Inconsistent inputs for 'Min/'Max";
               end if;

               case Val_1.Kind is
                  when Int =>
                     if Name = "min" then
                        return Create_Int_Result
                          (Typ,
                           Eval_Result'
                             (if Val_1.Int_Result < Val_2.Int_Result
                              then Val_1 else Val_2).Int_Result);
                     else
                        return Create_Int_Result
                          (Typ,
                           Eval_Result'
                             (if Val_1.Int_Result > Val_2.Int_Result
                              then Val_1 else Val_2).Int_Result);
                     end if;
                  when Real =>
                     if Name = "min" then
                        return Create_Real_Result
                          (Typ,
                           Eval_Result'
                             (if Val_1.Real_Result < Val_2.Real_Result
                              then Val_1 else Val_2).Real_Result);
                     else
                        return Create_Real_Result
                          (Typ,
                           Eval_Result'
                             (if Val_1.Real_Result > Val_2.Real_Result
                              then Val_1 else Val_2).Real_Result);
                     end if;
                  when others =>
                     raise Property_Error with
                        "'Min/'Max not applicable on enum types";
               end case;
            end;
         elsif Name in "succ" | "pred" then
            if Args.Is_Null or else Args.Children_Count /= 1 then
               raise Property_Error with
                  "'Pred/'Succ require exactly one argument";
            end if;

            declare
               Typ      : constant Base_Type_Decl :=
                 AR.F_Prefix.P_Name_Designated_Type;
               Val      : constant Eval_Result :=
                 Expr_Eval (Args.Child (1).As_Param_Assoc.F_R_Expr);
               Enum_Val : Enum_Literal_Decl;
            begin
               case Val.Kind is
               when Int =>
                  --  TODO??? Properly handle modular types
                  return Create_Int_Result
                    (Typ,
                     (if Name = "succ"
                      then Val.Int_Result + 1
                      else Val.Int_Result - 1));
               when Real =>
                  raise Property_Error with
                     "'Pred/'Succ not applicable to reals";
               when others =>
                  Enum_Val := Ada_Node'
                    (if Name = "succ"
                     then Val.Enum_Result.Next_Sibling
                     else Val.Enum_Result.Previous_Sibling)
                    .As_Enum_Literal_Decl;

                  if Enum_Val.Is_Null then
                     raise Property_Error with
                       "out of bounds 'Pred/'Succ on enum";
                  end if;
                  return Create_Enum_Result (Typ, Enum_Val);
               end case;
            end;
         elsif Name in "val" then
            if Args.Is_Null or Args.Children_Count /= 1 then
               raise Property_Error with
                  "'Val require exactly one argument";
            end if;

            declare
               Typ      : constant Base_Type_Decl :=
                 AR.F_Prefix.P_Name_Designated_Type;
               Val      : constant Eval_Result :=
                 Expr_Eval (Args.Child (1).As_Param_Assoc.F_R_Expr);
            begin
               if Val.Kind /= Int then
                  raise Property_Error with
                     "'Val expects an integer argument";
               end if;

               if Typ.P_Is_Int_Type then
                  return Create_Int_Result (Typ, Val.Int_Result);
               elsif Typ.P_Is_Enum_Type then
                  declare
                     Index : constant Integer :=
                        To_Integer (Val.Int_Result);

                     Enum_Val : Enum_Literal_Decl :=
                        No_Enum_Literal_Decl;

                     Root_Type : constant LAL.Base_Type_Decl :=
                        Typ.P_Root_Type;
                  begin
                     if Index > -1 then
                        if (Index <= Character'Pos (Character'Last)
                            and then Root_Type = Typ.P_Std_Char_Type)
                          or else (Index <= Wide_Character'Pos
                                            (Wide_Character'Last)
                                   and then Root_Type =
                                            Typ.P_Std_Wide_Char_Type)
                          or else Root_Type = Typ.P_Std_Wide_Wide_Char_Type
                          --  Do not need to check for Wide_Wide_Character'Last
                          --  here, a runtime exception will be raised if Index
                          --  is out of range.
                        then
                           --  Due to how we define the Character type in our
                           --  artifical __standard unit (and its
                           --  Wide_Character and Wide_Wide_Character
                           --  variants), the 'Val attribute cannot return an
                           --  Enum_Literal_Decl since they are not defined. In
                           --  order to not fail the Eval_As_Int function, we
                           --  return the corresponding Integer value instead.
                           return Create_Int_Result (Typ, Val.Int_Result);
                        end if;

                        Enum_Val := Child
                           (Root_Type.As_Type_Decl.F_Type_Def.As_Enum_Type_Def
                            .F_Enum_Literals, Index + 1).As_Enum_Literal_Decl;
                     end if;

                     if Enum_Val.Is_Null then
                        raise Property_Error with
                          "out of bounds 'Val on enum";
                     end if;

                     return Create_Enum_Result (Typ, Enum_Val);
                  end;
               else
                  raise Property_Error with
                     "'Val only applicable to scalar types";
               end if;
            end;
         elsif Name in "pos" then
            if Args.Is_Null or Args.Children_Count /= 1 then
               raise Property_Error with
                  "'Pos require exactly one argument";
            end if;

            declare
               Typ      : constant Base_Type_Decl :=
                  AR.F_Prefix.P_Name_Designated_Type;
               Ret_Typ  : constant Base_Type_Decl :=
                  AR.P_Universal_Int_Type.As_Base_Type_Decl;
               Val      : constant Eval_Result :=
                  Expr_Eval (Args.Child (1).As_Param_Assoc.F_R_Expr);
            begin
               if Typ.P_Is_Int_Type then
                  if Val.Kind /= Int then
                     raise Property_Error with
                        "'Pos expects an integer argument";
                  end if;

                  --  The evaluator doesn't check if Pos argument is in the
                  --  range of Typ, i.e., illegal code such as:
                  --    Positive'Pos (-2)
                  --  will return -2.

                  return Create_Int_Result (Ret_Typ, Val.Int_Result);
               elsif Typ.P_Is_Enum_Type then
                  case Val.Kind is
                  when Int =>
                     return Create_Int_Result (Ret_Typ, Val.Int_Result);
                     --  This case allows to support Character enum literals
                  when Enum_Lit =>
                     return Create_Int_Result
                       (Ret_Typ, Val.Enum_Result.P_Enum_Rep);
                  when others =>
                     raise Property_Error with
                        "'Pos expects an argument of a discrete type";
                  end case;
               else
                  raise Property_Error with
                     "'Pos only applicable to discrete types";
               end if;
            end;
         elsif Name in "length" then

            --  Current support of 'Length only works on Strings (Character
            --  arrays). TODO??? Add support for all array types, including
            --  multidimensional ones.

            if not Args.Is_Null then
               raise Property_Error with
                  "'Length require no argument";
            end if;
            --  Not true for multidimensional arrays. 'Length attribute can
            --  take one argument standing for the Nth dimension of the
            --  array length is requested.

            declare
               Typ    : constant Base_Type_Decl :=
                  AR.F_Prefix.P_Name_Designated_Type;
               Val    : constant Eval_Result :=
                  Expr_Eval
                    (AR.F_Prefix.P_Referenced_Decl
                     .As_Object_Decl.F_Default_Expr);
               Result : Big_Integer;
            begin
               if Val.Kind /= String_Lit then
                  raise Property_Error with
                     "'Length expects a string argument";
               end if;

               Result.Set (GNATCOLL.GMP.Long (Length (As_String (Val))));

               return Create_Int_Result (Typ, Result);
            end;
         else
            raise Property_Error
              with "Unhandled attribute ref: " & Image (Attr.Text);
         end if;
      end Eval_Function_Attr;

      ----------------------
      -- Eval_Array_Index --
      ----------------------

      function Eval_Array_Index
        (Call_Expr : LAL.Call_Expr; Index : LAL.Expr) return Eval_Result
      is
         Array_Val : constant Eval_Result :=
            Eval_Decl (Call_Expr.P_Referenced_Decl);
         Index_Val : constant Eval_Result := Expr_Eval (Index);

         use GNATCOLL.GMP.Integers.Misc;
      begin
         if Array_Val.Kind = String_Lit then
            declare
               Str   : constant Unbounded_Text_Type := As_String (Array_Val);
               Index : constant Integer :=
                  Integer (As_Signed_Long (Index_Val.Int_Result));
            begin
               return Create_Int_Result
                 (Call_Expr.P_Expression_Type.P_Comp_Type,
                  Wide_Wide_Character'Pos (Element (Str, Index)));
            end;
         else
            raise Property_Error with
               "Cannot eval array index of kind " & Array_Val.Kind'Image;
         end if;
      end Eval_Array_Index;

      ----------------------
      -- Eval_Array_Slice --
      ----------------------

      function Eval_Array_Slice
        (Call_Expr : LAL.Call_Expr; Bounds : LAL.Bin_Op) return Eval_Result
      is
         Array_Val : constant Eval_Result :=
            Eval_Decl (Call_Expr.P_Referenced_Decl);
         First_Val : constant Eval_Result := Expr_Eval (Bounds.F_Left);
         Last_Val  : constant Eval_Result := Expr_Eval (Bounds.F_Right);

         use GNATCOLL.GMP.Integers.Misc;
      begin
         if Array_Val.Kind = String_Lit then
            declare
               Str   : Unbounded_Text_Type := As_String (Array_Val);
               First : constant Integer :=
                  Integer (As_Signed_Long (First_Val.Int_Result));
               Last  : constant Integer :=
                  Integer (As_Signed_Long (Last_Val.Int_Result));
               Len   : constant Positive := Length (Str);

            begin
               if First < Last then
                  --  Adjust Str regarding to requested bounds
                  Delete (Str, Len - (Array_Val.Last - Last - 1), Len);
                  Delete (Str, 1, First - Array_Val.First);
               else
                  --  The empty string
                  Delete (Str, 1, Len);
               end if;
               return (String_Lit, Call_Expr.P_Expression_Type,
                       Str, First, Last);
            end;
         else
            raise Property_Error with
               "Cannot eval array slide of kind " & Array_Val.Kind'Image;
         end if;
      end Eval_Array_Slice;

      ---------------
      -- Expr_Eval --
      ---------------

      function Expr_Eval (E : LAL.Expr) return Eval_Result is
      begin
         return Expr_Eval_In_Env (E, Env);
      end Expr_Eval;

   begin
      --  Processings on invalid Ada sources may lead to calling Expr_Eval on a
      --  null node. In this case, regular Ada runtime checks in code below
      --  will trigger a Constraint_Error, while we want here to propagate
      --  Property_Error exceptions on invalid code. So do the check ourselves.

      if E.Is_Null then
         raise Property_Error with "attempt to evaluate a null node";
      end if;

      case E.Kind is
         when Ada_Identifier | Ada_Dotted_Name =>
            return Eval_Decl (E.As_Name.P_Referenced_Decl);

         when Ada_Char_Literal =>
            declare
               Char      : constant LAL.Char_Literal := E.As_Char_Literal;
               Node_Type : constant LAL.Base_Type_Decl :=
                  Char.P_Expression_Type.P_Root_Type;
            begin
               --  A character literal is an enum value like any other and so
               --  its value should be its position in the enum. However, due
               --  to how we define our artificial __standard unit, this
               --  assumption does not hold for the Character type and its
               --  variants (Wide_Character, etc.) as they are not defined in
               --  their exact shape. We must therefore implement a specific
               --  path to handle them here.
               if Is_Std_Char_Type (Node_Type) then
                  --  Note that Langkit_Support's Character_Type is a
                  --  Wide_Wide_Character which can therefore also be used to
                  --  handle the Character and Wide_Character types.
                  return Create_Int_Result
                    (Char.P_Expression_Type,
                     Support.Text.Character_Type'Pos
                       (Char.P_Denoted_Value));
               else
                  --  If it's not a standard character type, evaluate it just
                  --  as any other enum literal.
                  return Eval_Decl (Char.P_Referenced_Decl);
               end if;
            end;

         when Ada_Int_Literal =>
            return (Int,
                    E.P_Universal_Int_Type.As_Base_Type_Decl,
                    E.As_Int_Literal.P_Denoted_Value);

         when Ada_Real_Literal =>
            return Result : Eval_Result :=
              (Kind        => Real,
               Expr_Type   => E.P_Universal_Real_Type.As_Base_Type_Decl,
               Real_Result => <>)
            do
               Decode_Real_Literal (E.Text, Result.Real_Result);
            end return;

         when Ada_String_Literal =>
            declare
               Val : constant Unbounded_Text_Type :=
                  +E.As_String_Literal.P_Denoted_Value;
            begin
               return (String_Lit, E.P_Expression_Type, Val, 1, Length (Val));
            end;

         when Ada_Membership_Expr =>
            declare
               MB        : constant LAL.Membership_Expr :=
                  E.As_Membership_Expr;

               Result    : Boolean := False;
               Op        : constant LAL.Op := F_Op (MB);
               Alts      : constant LAL.Expr_Alternatives_List :=
                  F_Membership_Exprs (MB);

               Choice_Value : constant Big_Integer := As_Int
                 (Expr_Eval (F_Expr (MB)));
            begin
               for C of Alts.Children loop
                  Result := Result or else P_Choice_Match (C, Choice_Value);
               end loop;

               if Op.Kind = Ada_Op_Not_In then
                  Result := not Result;
               end if;

               return Create_Bool_Result (Result, E.As_Ada_Node);
            end;

         when Ada_Relation_Op =>
            declare
               function Bool (X : Boolean; N : LAL.Ada_Node'Class := E)
                  return Eval_Result renames Create_Bool_Result;

               BO : constant LAL.Bin_Op := E.As_Bin_Op;
               Op : constant LAL.Op := BO.F_Op;
               L  : constant Eval_Result := Expr_Eval (BO.F_Left);
               R  : constant Eval_Result := Expr_Eval (BO.F_Right);
            begin
               if L.Kind /= R.Kind then
                  raise Property_Error with "Unsupported type discrepancy";
               end if;

               case R.Kind is
               when Int =>
                  case Op.Kind is
                  when Ada_Op_Eq =>
                     return Bool (L.Int_Result = R.Int_Result);
                  when Ada_Op_Neq =>
                     return Bool (L.Int_Result /= R.Int_Result);
                  when Ada_Op_Lt =>
                     return Bool (L.Int_Result < R.Int_Result);
                  when Ada_Op_Lte =>
                     return Bool (L.Int_Result <= R.Int_Result);
                  when Ada_Op_Gt =>
                     return Bool (L.Int_Result > R.Int_Result);
                  when Ada_Op_Gte =>
                     return Bool (L.Int_Result >= R.Int_Result);
                  when others =>
                     raise Program_Error with "Impossible path";
                  end case;

               when Real =>
                  case Op.Kind is
                  when Ada_Op_Eq =>
                     return Bool (L.Real_Result = R.Real_Result);
                  when Ada_Op_Neq =>
                     return Bool (L.Real_Result /= R.Real_Result);
                  when Ada_Op_Lt =>
                     return Bool (L.Real_Result < R.Real_Result);
                  when Ada_Op_Lte =>
                     return Bool (L.Real_Result <= R.Real_Result);
                  when Ada_Op_Gt =>
                     return Bool (L.Real_Result > R.Real_Result);
                  when Ada_Op_Gte =>
                     return Bool (L.Real_Result >= R.Real_Result);
                  when others =>
                     raise Program_Error with "Impossible path";
                  end case;

               when Enum_Lit =>
                  case Op.Kind is
                  when Ada_Op_Eq =>
                     return Bool (L.Enum_Result = R.Enum_Result);
                  when Ada_Op_Neq =>
                     return Bool (L.Enum_Result /= R.Enum_Result);
                  when others =>
                     raise Property_Error with
                        "Unhandled relation operator on enum values: "
                        & Op.Kind'Image;
                  end case;

               when String_Lit =>
                  case Op.Kind is
                  when Ada_Op_Eq =>
                     return Bool
                       (Langkit_Support.Text."="
                         (L.String_Result, R.String_Result));
                  when Ada_Op_Neq =>
                     return Bool
                       (Langkit_Support.Text."/="
                         (L.String_Result, R.String_Result));
                  when others =>
                     raise Property_Error with
                        "Unhandled relation operator on string values: "
                        & Op.Kind'Image;
                  end case;
               end case;
            end;

         when Ada_Bin_Op =>
            declare
               BO : constant LAL.Bin_Op := E.As_Bin_Op;
               Op : constant LAL.Op := BO.F_Op;
               L  : constant Eval_Result := Expr_Eval (BO.F_Left);
               R  : constant Eval_Result := Expr_Eval (BO.F_Right);
            begin
               if L.Kind /= R.Kind then
                  if L.Kind = Int and then R.Kind = Real
                    and then Op.Kind = Ada_Op_Mult
                  then
                     declare
                        Result : Rational;
                        Left : Rational;
                     begin
                        Left.Set (L.Int_Result);
                        Result.Set (Left * R.Real_Result);
                        return Create_Real_Result (R.Expr_Type, Result);
                     end;
                  elsif L.Kind = Real and then R.Kind = Int
                    and then Op.Kind = Ada_Op_Mult
                  then
                     declare
                        Result : Rational;
                        Right : Rational;
                     begin
                        Right.Set (R.Int_Result);
                        Result.Set (L.Real_Result * Right);
                        return Create_Real_Result (L.Expr_Type, Result);
                     end;
                  elsif L.Kind = Real and then R.Kind = Int
                    and then Op.Kind = Ada_Op_Div
                  then
                     declare
                        Result : Rational;
                        Right : Rational;
                     begin
                        Right.Set (R.Int_Result);
                        Result.Set (L.Real_Result / Right);
                        return Create_Real_Result (L.Expr_Type, Result);
                     end;
                  elsif L.Kind = Real and then R.Kind = Int
                    and then Op.Kind = Ada_Op_Pow
                  then
                     declare
                        Result : Rational;
                     begin
                        Result.Set (L.Real_Result ** R.Int_Result);
                        return Create_Real_Result (L.Expr_Type, Result);
                     end;
                  else
                     raise Property_Error with "Unsupported type discrepancy";
                  end if;
               end if;

               case R.Kind is
               when Int =>
                  --  Handle arithmetic operators on Int values
                  declare
                     Result : Big_Integer;
                  begin
                     case Op.Kind is
                     when Ada_Op_Plus =>
                        Result.Set (L.Int_Result + R.Int_Result);
                     when Ada_Op_Minus =>
                        Result.Set (L.Int_Result - R.Int_Result);
                     when Ada_Op_Mult =>
                        Result.Set (L.Int_Result * R.Int_Result);
                     when Ada_Op_Div =>
                        if R.Int_Result = 0 then
                           raise Property_Error with "Division by zero";
                        end if;
                        Result.Set (L.Int_Result / R.Int_Result);
                     when Ada_Op_Pow =>
                        Raise_To_N (L.Int_Result, R.Int_Result, Result);
                     when others =>
                        raise Property_Error with
                           "Unhandled operator: " & Op.Kind'Image;
                     end case;

                     return Create_Int_Result (R.Expr_Type, Result);
                  end;

               when Real =>
                  --  Handle arithmetic operators on Real values
                  declare
                     Result : Rational;
                  begin
                     begin
                        case Op.Kind is
                        when Ada_Op_Plus =>
                           Result.Set (L.Real_Result + R.Real_Result);
                        when Ada_Op_Minus =>
                           Result.Set (L.Real_Result - R.Real_Result);
                        when Ada_Op_Mult =>
                           Result.Set (L.Real_Result * R.Real_Result);
                        when Ada_Op_Div =>
                           Result.Set (L.Real_Result / R.Real_Result);
                        when others =>
                           raise Property_Error with
                              "Unhandled operator: " & Op.Kind'Image;
                        end case;
                     exception
                        when Exc : Constraint_Error =>
                           raise Property_Error with
                              "Floating point computation error: "
                              & Ada.Exceptions.Exception_Message (Exc);
                     end;
                     return Create_Real_Result (R.Expr_Type, Result);
                  end;

               when Enum_Lit =>
                  --  Handle relational operators on boolean values
                  declare
                     LB        : constant Boolean := As_Bool (L);
                     RB        : constant Boolean := As_Bool (R);
                     Result    : Boolean;
                  begin
                     case Op.Kind is
                     when Ada_Op_And | Ada_Op_And_Then =>
                        Result := LB and then RB;
                     when Ada_Op_Or | Ada_Op_Or_Else =>
                        Result := LB or else RB;
                     when others =>
                        raise Property_Error with
                           "Wrong operator for boolean: " & Op.Kind'Image;
                     end case;

                     return Create_Bool_Result (Result, BO.As_Ada_Node);
                  end;

               when String_Lit =>
                  raise Property_Error with
                     "Wrong operator for string: " & Op.Kind'Image;
               end case;
            end;

         when Ada_Concat_Op =>
            declare
               CO            : constant LAL.Concat_Op := E.As_Concat_Op;
               Concat_Result : Unbounded_Text_Type;
               First         : Natural := 0;
            begin
               for I of CO.P_Operands loop
                  declare
                     ER : constant Eval_Result := Expr_Eval (I);
                  begin
                     if First = 0 then
                        First := ER.First;
                     end if;
                     Concat_Result := Concat_Result & ER.String_Result;
                  end;
               end loop;
               return
                 (String_Lit,
                  E.P_Expression_Type,
                  Concat_Result,
                  First,
                  First + Length (Concat_Result));
            end;

         when Ada_Un_Op =>
            declare
               UO           : constant LAL.Un_Op := E.As_Un_Op;
               Op           : constant LAL.Op := UO.F_Op;
               Operand_Val  : constant Eval_Result := Expr_Eval (UO.F_Expr);
               Operand_Type : LAL.Base_Type_Decl renames Operand_Val.Expr_Type;

               subtype Valid_Unop_Kind is Ada_Node_Kind_Type with
                  Static_Predicate => Valid_Unop_Kind in
                     Ada_Op_Minus | Ada_Op_Plus | Ada_Op_Abs | Ada_Op_Not;
               Op_Kind : constant Valid_Unop_Kind := Op.Kind;
               --  Parsers can only build unary operators with the above
               --  operations. Using a subtype here saves us from writing dead
               --  code.
            begin
               case Operand_Val.Kind is
               when Enum_Lit =>
                  --  Unary operators are not valid on enums. This is not a
                  --  legality check: since we process standard character types
                  --  as integers, this guard will not reject them, but at
                  --  least code below can assume we are dealing with integers
                  --  or reals.
                  raise Property_Error with
                     "Unary operator invalid on enumerations";

               when String_Lit =>
                  raise Property_Error with
                     "Unary operator invalid on strings";

               when Int =>
                  declare
                     Operand : Big_Integer renames Operand_Val.Int_Result;
                     Result  : Big_Integer;
                  begin
                     case Op_Kind is
                     when Ada_Op_Minus =>
                        Result.Set (-Operand);
                     when Ada_Op_Plus =>
                        Result.Set (Operand);
                     when Ada_Op_Abs =>
                        Result.Set (abs Operand);
                     when Ada_Op_Not =>
                        --  TODO??? Here, we need to check that the operand
                        --  type is a modular type, and flip bits according to
                        --  its size.
                        raise Property_Error with
                           """not"" not implemented yet";
                     end case;
                     return Create_Int_Result (Operand_Type, Result);
                  end;

               when Real =>
                  declare
                     Operand : Rational renames Operand_Val.Real_Result;
                     Result  : Rational;
                  begin
                     begin
                        case Op_Kind is
                        when Ada_Op_Minus =>
                           Result.Set (-Operand);
                        when Ada_Op_Plus =>
                           Result.Set (Operand);
                        when Ada_Op_Abs =>
                           Result.Set (abs Operand);
                        when Ada_Op_Not =>
                           raise Property_Error with
                              "Invalid ""not"" operator for floating point"
                              & " value";
                        end case;
                     exception
                        when Exc : Constraint_Error =>
                           raise Property_Error with
                              "Floating point computation error: "
                              & Ada.Exceptions.Exception_Message (Exc);
                     end;
                     return Create_Real_Result (Operand_Type, Result);
                  end;
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
                  return Eval_Function_Attr (AR, LAL.No_Assoc_List);
               end if;
            end;

         when Ada_Paren_Expr =>
            return Expr_Eval (E.As_Paren_Expr.F_Expr);

         when Ada_Call_Expr =>
            declare
               C               : constant Call_Expr := E.As_Call_Expr;
               S               : constant Ada_Node := C.F_Suffix;
               Arg             : Expr;
               Designated_Type : constant Base_Type_Decl :=
                  C.F_Name.P_Name_Designated_Type;
               C_Kind          : Call_Expr_Kind;
            begin
               --  Make sure that C's name designates a type and that C has
               --  exactly one argument.
               if C.F_Name.Kind in Ada_Attribute_Ref then
                  return Eval_Function_Attr
                    (C.F_Name.As_Attribute_Ref, S.As_Assoc_List);
               end if;

               --  Avoid displaying LAL's internal property errors on calls to
               --  P_Kind when evaluating invalid code.
               begin
                  C_Kind := C.P_Kind;
               exception
                  when Property_Error =>
                     raise Property_Error with
                        "Unhandled call expr: " & Image (E.Text);
               end;

               if C_Kind in Array_Index then
                  return Eval_Array_Index
                    (C, S.Child (1).As_Param_Assoc.F_R_Expr);
               elsif C_Kind in Array_Slice then
                  return Eval_Array_Slice
                    (C, S.As_Bin_Op);
               elsif Designated_Type.Is_Null
                  or else S.Is_Null
                  or else S.Children_Count /= 1
               then
                  raise Property_Error
                    with "Unhandled call expr: " & Image (E.Text);
               end if;

               Arg := S.Child (1).As_Param_Assoc.F_R_Expr;
               if Designated_Type.P_Is_Float_Type then
                  declare
                     Arg_Val : constant Eval_Result := Expr_Eval (Arg);
                     Result  : Rational;
                  begin
                     case Arg_Val.Kind is
                     when Int =>
                        Result.Set (Arg_Val.Int_Result);
                     when Real =>
                        Result.Set (Arg_Val.Real_Result);
                     when Enum_Lit =>
                        raise Property_Error with "Invalid enum argument";
                     when String_Lit =>
                        raise Property_Error with "Invalid string argument";
                     end case;
                     return Create_Real_Result (Designated_Type, Result);
                  end;

               elsif Designated_Type.P_Is_Int_Type then
                  declare
                     Arg_Val : constant Eval_Result := Expr_Eval (Arg);
                     Result  : Big_Integer;
                  begin
                     case Arg_Val.Kind is
                     when Int =>
                        Result.Set (Arg_Val.Int_Result);
                     when Real =>
                        Result.Set
                          (GNATCOLL.GMP.Long (Arg_Val.Real_Result.To_Double));
                     when Enum_Lit =>
                        raise Property_Error with "Invalid enum argument";
                     when String_Lit =>
                        raise Property_Error with "Invalid string argument";
                     end case;
                     return Create_Int_Result (Designated_Type, Result);
                  end;

               elsif Designated_Type.P_Is_Enum_Type then
                  declare
                     Arg_Val : constant Eval_Result := Expr_Eval (Arg);
                  begin
                     case Arg_Val.Kind is
                     when Int =>
                        raise Property_Error with "Invalid integer argument";
                     when Real =>
                        raise Property_Error with "Invalid real argument";
                     when Enum_Lit =>
                        --  Convert an enum to another enum: return Arg_Val
                        --  with its new type.
                        return Create_Enum_Result
                           (Designated_Type, Arg_Val.Enum_Result);
                     when String_Lit =>
                        raise Property_Error with "Invalid string argument";
                     end case;
                  end;

               else
                  raise Property_Error
                    with "Unhandled type conversion: " & Image (E.Text);
               end if;
            end;

         when others =>
            raise Property_Error with "Unhandled node: " & E.Kind'Img;
      end case;
   end Expr_Eval_In_Env;

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
            when String_Lit =>
               raise Property_Error;
         end case;
      end return;
   end As_Int;

   -------------
   -- As_Bool --
   -------------

   function As_Bool (Self : Eval_Result) return Boolean is
      Bool_Type   : LAL.Base_Type_Decl;
   begin
      case Self.Kind is
         when Enum_Lit =>
            Bool_Type :=
              Self.Enum_Result.P_Std_Entity (+"Boolean").As_Base_Type_Decl;
            if Self.Expr_Type /= Bool_Type then
               raise Property_Error with "Wrong type for enum for As_Bool";
            end if;
            return Self.Enum_Result.Text = "True";
         when others =>
            raise Property_Error with "Wrong value kind for As_Bool";
      end case;
   end As_Bool;

   ---------------
   -- As_String --
   ---------------

   function As_String (Self : Eval_Result) return Unbounded_Text_Type is
   begin
      case Self.Kind is
         when Int =>
            raise Property_Error;
         when Real =>
            raise Property_Error;
         when Enum_Lit =>
            raise Property_Error;
         when String_Lit =>
            return Self.String_Result;
      end case;
   end As_String;

   -----------
   -- Image --
   -----------

   function Image (Self : Eval_Result) return String is
   begin
      return "<Eval_Result "
        & Self.Kind'Image & " "
        & (case Self.Kind is
           when Int => Self.Int_Result.Image,
           when Real => Self.Real_Result.Image,
           when Enum_Lit => Self.Enum_Result.Image,
           when String_Lit => Encode (To_Text (Self.String_Result), "UTF-8"))
        & ">";
   end Image;

end Libadalang.Expr_Eval;
