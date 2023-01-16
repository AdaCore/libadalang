with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Opt_Parse;
with GNATCOLL.Strings; use GNATCOLL.Strings;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Libadalang.Analysis;           use Libadalang.Analysis;
with Libadalang.Common;             use Libadalang.Common;
with Libadalang.Data_Decomposition; use Libadalang.Data_Decomposition;
with Libadalang.Expr_Eval;          use Libadalang.Expr_Eval;
with Libadalang.Helpers;            use Libadalang.Helpers;

with Put_Title;

procedure LAL_DDA is

   Collection : Repinfo_Collection;

   procedure Process_Type_Decl (Decl : Base_Type_Decl);
   --  Display all representation information that is available in
   --  ``Collection`` for this type declaration.

   procedure Process_Variants
     (Variants : Variant_Representation_Array; Prefix : String);
   --  Display all representation information for the given record variants.
   --  ``Prefix`` is used as a prefix for all printed lines.

   procedure Process_Components
     (Components : Component_Representation_Array; Prefix : String);
   --  Display all representation information for the given components.
   --  ``Prefix`` is used as a prefix for all printed lines.

   procedure Put_Number (Expr : Numerical_Expression);
   --  If ``Expr`` requires discriminants, print "<dynamic>".  Otherwise,
   --  evaluate it and print its result.

   package Expr_Vectors is new Ada.Containers.Vectors (Positive, Expr);
   use type Expr_Vectors.Vector;
   package Expr_Vector_Vectors is new Ada.Containers.Vectors
     (Positive, Expr_Vectors.Vector);

   function Test_Discriminants
     (Decl : Base_Type_Decl) return Expr_Vector_Vectors.Vector;
   --  Fetch the vector of discriminants to use for testing from nearby Test
   --  pragmas.

   procedure Error (Node : Ada_Node'Class; Message : String) with No_Return;
   --  Abort the App with the given error ``Message``, contextualized using
   --  ``Node`` 's source location.

   procedure Put_Error (Message : String; Exc : Exception_Occurrence);
   --  Print information about ``Exc``

   ---------
   -- App --
   ---------

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array);
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);
   package App is new Libadalang.Helpers.App
     (Name         => "lal_dda",
      Description  =>
        "Exercize Libadalang's Data_Decomposition API on type declarations",
      App_Setup    => App_Setup,
      Process_Unit => Process_Unit);

   package Args is
      use GNATCOLL.Opt_Parse;

      package Rep_Info_Files is new Parse_Option_List
        (App.Args.Parser, "-i", "--rep-info-file",
         Arg_Type   => Unbounded_String,
         Accumulate => True,
         Help       => "Output for the compiler's -gnatR4j option");

   end Args;

   ---------------
   -- App_Setup --
   ---------------

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array) is
      pragma Unreferenced (Context, Jobs);
   begin
      Collection := Load (Filename_Array (Args.Rep_Info_Files.Get));
   exception
      when Exc : Loading_Error =>
         Put_Line
           ("Loading_Error raised while loading representation information:");
         Put_Line (Exception_Message (Exc));
         New_Line;
   end App_Setup;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);

      function Process (Node : Ada_Node'Class) return Visit_Status;
      --  Implement the element processing.
      --
      --  * If Node is a type declaration, call ``Process_Type_Decl`` on it.
      --
      --  * If it is a ``Test_Object_Type`` pragma, fetch the object
      --    declaration before it and call ``Process_Type_Decl`` on the type
      --    declaration designated by the object type expression.

      -------------
      -- Process --
      -------------

      function Process (Node : Ada_Node'Class) return Visit_Status is
      begin
         case Node.Kind is
            when Ada_Base_Type_Decl =>
               Process_Type_Decl (Node.As_Base_Type_Decl);

            when Ada_Pragma_Node =>
               declare
                  PN   : constant Pragma_Node := Node.As_Pragma_Node;
                  Name : constant Text_Type := To_Lower (PN.F_Id.Text);
                  Decl : Ada_Node;
               begin
                  if Name = "test_object_type" then
                     if PN.F_Args.Children_Count > 0 then
                        Error (Node, "no argument expected for this pragma");
                     end if;
                     Decl := PN.Previous_Sibling;
                     if Decl.Kind /= Ada_Object_Decl then
                        Error
                          (Node,
                           "previous declaration must be an object"
                           & " declaration");
                     end if;
                     Process_Type_Decl
                       (Decl.As_Object_Decl
                        .F_Type_Expr
                        .P_Designated_Type_Decl);
                  end if;
               end;

            when others =>
               null;
         end case;
         return Into;
      end Process;

   begin
      Put_Title
        ('#', "Analyzing " & Ada.Directories.Simple_Name (Unit.Get_Filename));
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         New_Line;
         return;

      elsif not Unit.Root.Is_Null then
         Unit.Root.Traverse (Process'Access);
      end if;
   end Process_Unit;

   -----------------------
   -- Process_Type_Decl --
   -----------------------

   procedure Process_Type_Decl (Decl : Base_Type_Decl) is
      R             : Type_Representation;
      Exprs_Vectors : constant Expr_Vector_Vectors.Vector :=
        Test_Discriminants (Decl);
   begin
      begin
         R := Collection.Lookup (Decl);
      exception
         when Exc : Type_Mismatch_Error =>
            Put_Error
              ("Cannot fetch representation information for " & Decl.Image,
               Exc);
            New_Line;
         return;
      end;

      if Is_Null (R) then
         Put_Line ("No representation information for " & Decl.Image);
         New_Line;
         return;
      end if;

      --  Look for extra test directives. Use the "unresolved" API first...

      Put_Line ("Representation information for " & Decl.Image & ":");
      Put_Line ("Kind: " & Kind (R)'Image);

      Put_Line
        ("Alignment:" & Libadalang.Data_Decomposition.Alignment (R)'Image);

      Put ("Object_Size | Value_Size: ");
      begin
         Put_Number (Object_Size (R));
      exception
         when Unsupported_Expression =>
            Put ("<unsupported>");
      end;
      Put (" | ");
      begin
         Put_Number (Value_Size (R));
      exception
         when Unsupported_Expression =>
            Put ("<unsupported>");
      end;
      New_Line;

      case Kind (R) is
         when Record_Type =>
            Put_Line
              ("Bit_Order | Scalar_Storage_Order: "
               & Bit_Order (R)'Image
               & " | "
               & Scalar_Storage_Order (R)'Image);

            begin
               Process_Components (Components (R), "");
            exception
               when Exc : Type_Mismatch_Error =>
                  Put_Error ("Cannot get components", Exc);
            end;

            if Has_Variant_Part (R) then
               begin
                  Process_Variants (Variants (R), "");
               exception
                  when Exc : Type_Mismatch_Error =>
                     Put_Error ("Cannot get variants", Exc);
               end;
            end if;

         when Array_Type =>
            Put_Line ("Scalar_Storage_Order: "
                      & Scalar_Storage_Order (R)'Image);
            Put ("Component_Size: ");
            begin
               Put_Number (Component_Size (R));
            exception
               when Unsupported_Expression =>
                  Put ("<unsupported>");
            end;
            New_Line;

         when Fixed_Type =>
            Put_Line ("Small: " & Small (R).Image);
            Put_Line ("Range: "
                      & Range_First (R).Image
                      & " .. "
                      & Range_Last (R).Image);

         when others =>
            null;
      end case;

      --  ... then use the resolved record API if lists of discriminants are
      --  given.

      for Exprs of Exprs_Vectors loop

         --  First, evaluate the discriminants to use for record resolution

         declare
            DVs : Libadalang.Data_Decomposition.Discriminant_Values
                    (1 .. Natural (Exprs.Length));
         begin
            New_Line;
            Put ("Resolved record for discriminants (");
            for Cur in Exprs.Iterate loop
               declare
                  I  : constant Positive := Expr_Vectors.To_Index (Cur);
                  DV : constant GMP_Int.Big_Integer :=
                    Discriminant_Value
                      (Expr_Eval (Expr_Vectors.Element (Cur)));
               begin
                  DVs (I).Set (DV);
                  if I > 1 then
                     Put (", ");
                  end if;
                  Put (DV.Image);
               end;
            end loop;
            Put_Line ("):");

            --  Then resolve the record

            begin
               declare
                  RR : constant Resolved_Record_Type :=
                    Resolved_Record (R, DVs);
               begin
                  Put_Line ("  Alignment:" & RR.Alignment'Image);
                  Put_Line ("  Object_Size:" & RR.Object_Size'Image);
                  Put_Line ("  Value_Size:" & RR.Value_Size'Image);
                  Put_Line ("  Bit_Order: " & RR.Bit_Order'Image);
                  Put_Line ("  Scalar_Storage_Order: "
                            & RR.Scalar_Storage_Order'Image);
                  for C of RR.Components loop
                     Put ("  * ");
                     if C.Declaration.Is_Null then
                        Put
                          ("Artificial component "
                           & Image (To_Text (C.Artificial_Name),
                                    With_Quotes => True));
                     else
                        Put (C.Declaration.Image);
                     end if;
                     Put (" at" & C.Position'Image);

                     if C.First_Bit /= 0 then
                        Put (", bit" & C.First_Bit'Image);
                     end if;
                     Put_Line (", size:" & C.Size'Image);
                  end loop;
               end;
            exception
               when Resolution_Error =>
                  Put_Line ("  <Resolution_Error>");
            end;
         end;
      end loop;

      New_Line;
   end Process_Type_Decl;

   ------------------------
   -- Test_Discriminants --
   ------------------------

   function Test_Discriminants
     (Decl : Base_Type_Decl) return Expr_Vector_Vectors.Vector
   is
      Result  : Expr_Vector_Vectors.Vector;
      Node    : Ada_Node := Decl.Next_Sibling;
      Prag    : Pragma_Node;
   begin
      --  Look for all Test pragmas that directly follow ``Decl``

      while not Node.Is_Null and then Node.Kind = Ada_Pragma_Node loop
         Prag := Node.As_Pragma_Node;
         if To_Lower (Prag.F_Id.Text) = "test" then

            --  We expect test pragmas to have a list of arguments without name
            --  associations. Each argument must be a static expression that
            --  resolves to a discriminant value, and the list of arguments
            --  must correspond to the list of type discriminants:
            --
            --    type R (D1 : Natural; D2 : Enum_Type) is record ...
            --    end record;
            --    pragma Test (1, Enum_Value);

            declare
               Args  : constant Ada_Node_Array := Prag.F_Args.Children;
               A     : Pragma_Argument_Assoc;
               E     : Expr;
               Exprs : Expr_Vectors.Vector;
            begin
               for Node of Args loop
                  A := Node.As_Pragma_Argument_Assoc;
                  if not A.F_Name.Is_Null then
                     Error (A, "named arguments not allowed");
                  end if;
                  E := A.F_Expr;
                  if not E.P_Is_Static_Expr then
                     Error (E, "static expression expected");
                  end if;
                  Exprs.Append (E);
               end loop;
               Result.Append (Exprs);
            end;

         end if;

         Node := Node.Next_Sibling;
      end loop;
      return Result;
   end Test_Discriminants;

   ----------------------
   -- Process_Variants --
   ----------------------

   procedure Process_Variants
     (Variants : Variant_Representation_Array; Prefix : String)
   is
      First : Boolean := True;
   begin
      if Variants'Length = 0 then
         return;
      end if;

      Put_Line (Prefix & "* Variant part");
      for V of Variants loop
         if First then
            First := False;
         else
            Put_Line (To_XString (Prefix).Trim (Right).To_String);
         end if;
         Process_Components (Components (V), Prefix & "  | ");
         if Has_Subvariant_Part (V) then
            Process_Variants (Subvariants (V), Prefix & "  | ");
         end if;
      end loop;
   end Process_Variants;

   ------------------------
   -- Process_Components --
   ------------------------

   procedure Process_Components
     (Components : Component_Representation_Array; Prefix : String)
   is
      Decl : Defining_Name;
   begin
      for Comp of Components loop
         Put (Prefix & "* ");
         Decl := Declaration (Comp);
         if Decl.Is_Null then
            Put ("Artificial component "
                 & Image (Component_Name (Comp), With_Quotes => True));
         else
            Put (Declaration (Comp).Image);
         end if;
         if Discriminant_Number (Comp) /= 0 then
            Put
              (" (discriminant" & Discriminant_Number (Comp)'Image & ")");
         end if;

         Put (" at ");
         begin
            Put_Number (Position (Comp));
         exception
            when Unsupported_Expression =>
               Put ("<unsupported>");
         end;
         declare
            B : constant Natural := First_Bit (Comp);
         begin
            if B /= 0 then
               Put (", bit" & B'Image);
            end if;
         end;
         Put (", size: ");
         begin
            Put_Number (Size (Comp));
         exception
            when Unsupported_Expression =>
               Put ("<unsupported>");
         end;
         New_Line;
      end loop;
   end Process_Components;

   ----------------
   -- Put_Number --
   ----------------

   procedure Put_Number (Expr : Numerical_Expression) is
   begin
      if Discriminant_Count (Expr) > 0 then
         Put ("<dynamic>");
      else
         Put (Evaluate (Expr, No_Discriminant_Value).Image);
      end if;
   end Put_Number;

   -----------
   -- Error --
   -----------

   procedure Error (Node : Ada_Node'Class; Message : String) is
   begin
      Abort_App (Image (Start_Sloc (Node.Sloc_Range)) & ": " & Message);
   end Error;

   ---------------
   -- Put_Error --
   ---------------

   procedure Put_Error (Message : String; Exc : Exception_Occurrence) is
   begin
      Put_Line (Message & ":");
      Put_Line ("  " & Exception_Name (Exc));
      Put_Line ("  " & Exception_Message (Exc));
   end Put_Error;

begin
   App.Run;
   Put_Line ("Done.");
end LAL_DDA;
