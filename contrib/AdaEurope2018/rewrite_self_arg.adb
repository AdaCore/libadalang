--  Rename controlling parameters of primitive subprograms to "Self". Write the
--  result in ".new" suffixed files.

with Ada.Text_IO;

with Langkit_Support.Text;
with Libadalang.Analysis;
with Libadalang.Rewriting;

with Helpers;

procedure Rewrite_Self_Arg is

   package TIO renames Ada.Text_IO;
   package LAL renames Libadalang.Analysis;
   use type LAL.Ada_Node_Kind_Type;
   package LALR renames Libadalang.Rewriting;

   type Analysis_Unit_Array is array (Positive range <>) of LAL.Analysis_Unit;

   function Modified_Units
     (Handle : LALR.Rewriting_Handle) return Analysis_Unit_Array;
   --  Return the list of analysis units that will be modified by the Handle
   --  rewritting session.

   procedure Initialize (Context : LAL.Analysis_Context);
   procedure Process_Unit (Unit : LAL.Analysis_Unit);
   function Process_Node (Node : LAL.Ada_Node'Class) return LAL.Visit_Status;
   procedure Summarize (Context : LAL.Analysis_Context);

   Handle : LALR.Rewriting_Handle;

   --------------------
   -- Modified_Units --
   --------------------

   function Modified_Units
     (Handle : LALR.Rewriting_Handle) return Analysis_Unit_Array
   is
      Units : constant LALR.Unit_Rewriting_Handle_Array :=
        LALR.Unit_Handles (Handle);
      Result : Analysis_Unit_Array (Units'Range);
   begin
      for I in Units'Range loop
         Result (I) := LALR.Unit (Units (I));
      end loop;
      return Result;
   end Modified_Units;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Context : LAL.Analysis_Context) is
   begin
      Handle := LALR.Start_Rewriting (Context);
   end Initialize;

   ------------------
   -- Process_Node --
   ------------------

   function Process_Node (Node : LAL.Ada_Node'Class) return LAL.Visit_Status is
      use type LALR.Node_Rewriting_Handle;

      Subp_Spec : LAL.Subp_Spec;
      --  Subprogram specification to rewrite

      Primitive_Of : LAL.Base_Type_Decl;
      --  Subp_Spec is a primitive of this

      Id : LALR.Node_Rewriting_Handle := LALR.No_Node_Rewriting_Handle;
      --  Formal identifier to rewrite into Self
   begin
      --  Only process Subp_Spec nodes that are primitives and that have at
      --  least one paramater.
      if Node.Kind /= LAL.Ada_Subp_Spec then
         return LAL.Into;
      end if;

      Subp_Spec := Node.As_Subp_Spec;
      Primitive_Of := Subp_Spec.P_Primitive_Subp_Of;
      if Primitive_Of.Is_Null or else Subp_Spec.F_Subp_Params.Is_Null
      then
         return LAL.Into;
      end if;

      --  Look for a parameter whose type is the same as Primitive_Of. Abort
      --  rewriting if there are more than one such parameter.
      Primitive_Of := Primitive_Of.P_Canonical_Type;
      for Param_Spec of Subp_Spec.F_Subp_Params.F_Params.Children loop
         declare
            use type LAL.Base_Type_Decl;

            PS         : constant LAL.Param_Spec := Param_Spec.As_Param_Spec;
            Param_Type : constant LAL.Base_Type_Decl :=
              PS.F_Type_Expr.P_Designated_Type_Decl.P_Canonical_Type;
         begin
            if Param_Type = Primitive_Of then
               if Id /= LALR.No_Node_Rewriting_Handle
                 or else PS.F_Ids.Children_Count > 1
               then
                  return LAL.Into;
               end if;
               Id := LALR.Handle (PS.F_Ids.Child (1).As_Defining_Name.F_Name);
            end if;
         end;
      end loop;

      --  Rename the formal!
      if Id /= LALR.No_Node_Rewriting_Handle then
         LALR.Set_Text (Id, "Self");
      end if;

      return LAL.Into;
   end Process_Node;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Unit : LAL.Analysis_Unit) is
   begin
      LAL.Root (Unit).Traverse (Process_Node'Access);
   end Process_Unit;

   ---------------
   -- Summarize --
   ---------------

   procedure Summarize (Context : LAL.Analysis_Context) is
      pragma Unreferenced (Context);

      Units : constant Analysis_Unit_Array := Modified_Units (Handle);
      --  Remember which analysis units are to be rewritten

      Result : constant LALR.Apply_Result := LALR.Apply (Handle);
   begin
      case Result.Success is
         when True => null;
         when False => TIO.Put_Line ("Error during rewriting...");
      end case;

      --  Go through all rewritten units and generate a ".new" source file to
      --  contain the rewritten sources.
      for U of Units loop
         declare
            Filename : constant String := LAL.Get_Filename (U) & ".new";
            Charset  : constant String := LAL.Get_Charset (U);

            --  Retreive rewritten text, and encode it using the same encoding
            --  as in the original file.
            Content_Text  : constant Langkit_Support.Text.Text_Type :=
              LAL.Text (U);
            Content_Bytes : constant String :=
              Langkit_Support.Text.Transcode (Content_Text, Charset);

            Output_File : TIO.File_Type;
         begin
            TIO.Create
              (Output_File, TIO.Out_File, Filename);
            TIO.Put (Output_File, Content_Bytes);
            TIO.Close (Output_File);
         end;
      end loop;
   end Summarize;

begin
   Helpers.Iterate_Units
     (Initialize'Access, Process_Unit'Access, Summarize'Access);
end Rewrite_Self_Arg;
