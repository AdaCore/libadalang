with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx  : constant Analysis_Context := Create_Context;
   Unit : constant Analysis_Unit := Get_From_File (Ctx, "test.adb");

   Float_Precision : constant := 2;

   function Is_Object_Decl (N : Ada_Node) return Boolean
   is (Kind (N) in Ada_Object_Decl
         and then not N.As_Object_Decl.F_Default_Expr.Is_Null);

   function Eval_Result_To_String (X : Eval_Result) return String is
   begin
      if X.Kind = Real then
         declare
            Img     : String  := X.Real_Result'Image;

            -- Get the integer value of the exponent. The exponent
            -- is made of the last two characters of the image.
            Exp     : Integer :=
               Integer'Value (Img (Img'Last - 1 .. Img'Last));

            -- Is the exponent positive?
            Exp_Pos : Boolean := (Img (Img'Last - 2) = '+');

            -- Compute the number of digits to keep in the final
            -- representation so as to have exactly `Float_Precision` digits
            -- of precision.
            Length  : Integer := Float_Precision
               + (if Exp_Pos then Exp else 0);
         begin
            -- Build the final string as the concatenation of:
            --  * The first `3 + Length` characters of the original image
            --    (the sign, the first digit and the dot + the computed
            --    precision).
            --  * The exponent of the original image.
            return Img (1 .. 3 + Length) & Img (Img'Last - 3 .. Img'Last);
         end;
      else
         return Image (X);
      end if;
   end Eval_Result_To_String;

begin
   for E of Find (Root (Unit), Is_Object_Decl'Access).Consume loop
      begin
         declare
            Res : Eval_Result := Expr_Eval (E.As_Object_Decl.F_Default_Expr);
            Img : String := Image (Res);
         begin
            Put_Line
               ("Expr " & Short_Image (E) & " evaluated to "
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
