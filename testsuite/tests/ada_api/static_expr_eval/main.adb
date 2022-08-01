with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx  : constant Analysis_Context := Create_Context (Charset => "utf-8");
   Unit : constant Analysis_Unit := Get_From_File (Ctx, "test.adb");

   Float_Precision : constant := 2;
   --  Controls the number of decimals displayed for real values

   function Is_Object_Decl (N : Ada_Node) return Boolean
   is (Kind (N) in Ada_Object_Decl
         and then not N.As_Object_Decl.F_Default_Expr.Is_Null);

   function Eval_Result_To_String (X : Eval_Result) return String;
   --  Return a representation of the given Eval_Result as a String.
   --  It uses a custom routine for rendering reals that limit the
   --  amount of decimals to display, so as to avoid potential
   --  precision indeterminism across platforms.

   function Eval_Call (E : Call_Expr) return Eval_Result;
   --  Evaluate the given function call expression. This uses the
   --  Eval_Expr_In_Env subprogram in order to substitute the formals
   --  by the actuals.

   ---------------------------
   -- Eval_Result_To_String --
   ---------------------------

   function Eval_Result_To_String (X : Eval_Result) return String is
   begin
      case X.Kind is
         when Int =>
            return "Int " & X.Int_Result.Image;
         when Real =>
            return "Real " & X.Real_Result.Image;
         when Enum_Lit =>
            return "Enum_Lit " & X.Enum_Result.Image;
         when String_Lit =>
            return ("String_Lit " & """"
                    & Encode (To_Text (X.String_Result), "UTF-8") & """");
      end case;
   end Eval_Result_To_String;

   ---------------
   -- Eval_Call --
   ---------------

   function Eval_Call (E : Call_Expr) return Eval_Result is
      Called_Subp : constant Basic_Decl := E.F_Name.P_Referenced_Decl;
      Fun_Expr    : constant Expr := Called_Subp.As_Expr_Function.F_Expr;
      Assocs      : constant Assoc_List := E.F_Suffix.As_Assoc_List;

      Bindings : constant Param_Actual_Array := Assocs.P_Zip_With_Params;
      Env : Substitution_Array (Bindings'Range);
   begin
      for I in Env'Range loop
         declare
            Binding : constant Param_Actual := Bindings (I);

            Formal     : constant Basic_Decl :=
               Param (Binding).As_Defining_Name.P_Basic_Decl;

            Value      : constant Eval_Result :=
               Expr_Eval (Actual (Binding).As_Expr);

            Value_Type : constant Base_Type_Decl :=
               Formal.P_Type_Expression.P_Designated_Type_Decl;
         begin
            Env (I) := Create_Substitution
               (From_Decl  => Formal,
                To_Value   => As_Int (Value),
                Value_Type => Value_Type);
         end;
      end loop;
      return Expr_Eval_In_Env (Fun_Expr, Env);
   end Eval_Call;

begin
   if Unit.Has_Diagnostics then
      for D of Unit.Diagnostics loop
         Put_Line (Unit.Format_GNU_Diagnostic (D));
      end loop;
      return;
   end if;

   for E of Find (Root (Unit), Is_Object_Decl'Access).Consume loop
      begin
         declare
            Default_Expr : constant Expr := E.As_Object_Decl.F_Default_Expr;
            Is_User_Call : constant Boolean :=
               (Default_Expr.Kind = Ada_Call_Expr
                and then Default_Expr.As_Call_Expr.P_Is_Call
                and then Default_Expr.As_Call_Expr.F_Name.Kind
                   not in Ada_Attribute_Ref);

            Res : constant Eval_Result :=
               (if Is_User_Call then Eval_Call (Default_Expr.As_Call_Expr)
                else Expr_Eval (Default_Expr));
         begin
            Put_Line
               ("Expr " & E.Image & " evaluated to "
                & Eval_Result_To_String (Res));
            if Res.Kind in Int | Enum_Lit then
               Put_Line ("   Int value is " & As_Int (Res).Image);
            end if;
         end;
      exception
         when Error : Property_Error =>
            Put_Line ("Property_Error: " & Exception_Message (Error));
      end;
      New_Line;
   end loop;

   Put_Line ("Done.");
end Main;
