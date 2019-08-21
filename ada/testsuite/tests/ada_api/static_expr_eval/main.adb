with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx  : constant Analysis_Context := Create_Context (Charset => "utf8");
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

   ---------------------------
   -- Eval_Result_To_String --
   ---------------------------

   function Eval_Result_To_String (X : Eval_Result) return String is
   begin
      case X.Kind is
         when Int =>
            return "Int " & X.Int_Result.Image;
         when Real =>
            declare
               Img         : constant String  := X.Real_Result'Image;

               --  We expect a float image matching the following pattern::
               --
               --     [mantissa]E[sign][exponent]
               --
               --  Where `mantissa` is a read number such as `1.00`, `sign` is
               --  either `+` or `-`, and `exponent` is a two-digit integer.
               --
               --  The following declarations break up the image into these
               --  various components.

               pragma Assert (Img'Length > 5);

               Sign          : Character renames Img (Img'First);
               Mantissa      : String renames
                  Img (Img'First + 1 .. Img'Last - 4);
               Exponent_Sign : Character renames Img (Img'Last - 2);
               Exponent      : String renames Img (Img'Last - 1 .. Img'Last);

               pragma Assert (Sign in ' ' | '-');
               pragma Assert (Exponent_Sign in '+' | '-');
               pragma Assert (Img (Img'Last - 3) = 'E');

               --  We can now decode the various parts

               Exponent_Value    : constant Integer := Integer'Value (Exponent);
               Positive_Exponent : constant Boolean := Sign = '+';

               Mantissa_Sign : constant String :=
                  (if Sign = ' ' then "" else "-");

               Mantissa_Slice : String renames Mantissa
                 (Mantissa'First
                  .. Integer'Min (Mantissa'First + 1 + Float_Precision,
                                  Mantissa'Last));
               --  Take only the slice of Mantissa that we are interested in to
               --  keep the expected precision::
               --
               --    * the first 2 characters of the original image (the first
               --      digit and the dot);
               --
               --    * additional digits for the expected precision.
               --
               --  ... but don't go past the mantissa we have (Mantissa'Last).
            begin
               --  Rebuild the image from the mantissa slice and the exponent
               return ("Real " & Mantissa_Sign & Mantissa_Slice
                       & 'E' & Exponent_Sign & Exponent);
            end;
         when Enum_Lit =>
            return "Enum_Lit " & X.Enum_Result.Short_Image;
      end case;
   end Eval_Result_To_String;

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
            Res : constant Eval_Result :=
               Expr_Eval (E.As_Object_Decl.F_Default_Expr);
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
