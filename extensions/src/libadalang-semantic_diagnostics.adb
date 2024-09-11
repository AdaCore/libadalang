--
--  Copyright (C) 2014-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Common; use Libadalang.Common;
with Libadalang.Implementation;
with Libadalang.Public_Converters;

package body Libadalang.Semantic_Diagnostics is

   procedure Trace_Diagnostics
     (Diags       : Solver_Diagnostic_Array;
      Render_Node : Node_Renderer);
   --  Output the given diagnostic in the ``LIBADALANG.SEM_DIAGS`` trace

   function Render_Diagnostic
     (Diag        : Solver_Diagnostic;
      Render_Node : Node_Renderer;
      Refined_Loc : Ada_Node := No_Ada_Node)
      return Located_Message;
   --  Substitutes the holes in the given raw diagnostic's template by the
   --  diagnostic's argument nodes after rendering them with the given node
   --  renderer. If ``Refined_Loc`` is non null, use its location instead of
   --  that of the original diagnostic.

   ----------------------
   -- To_Pretty_String --
   ----------------------

   function To_Pretty_String (Msg : Located_Message) return String is
      Sloc        : constant Source_Location :=
        Start_Sloc (Msg.Location.Sloc_Range);
      Sloc_Prefix : constant String :=
        (if Sloc = No_Source_Location
         then ""
         else Image (Sloc) & ": ");
   begin
      return Sloc_Prefix & Image (To_Text (Msg.Message));
   end To_Pretty_String;

   -----------------------
   -- Render_Diagnostic --
   -----------------------

   function Render_Diagnostic
     (Diag        : Solver_Diagnostic;
      Render_Node : Node_Renderer;
      Refined_Loc : Ada_Node := No_Ada_Node)
      return Located_Message
   is
      Last_Index : Natural := 1;
      Result     : Unbounded_Wide_Wide_String :=
        To_Unbounded_Text (Message_Template (Diag));
      Actual_Loc : constant Ada_Node :=
        (if Refined_Loc.Is_Null
         then Location (Diag).As_Ada_Node
         else Refined_Loc);
   begin
      for Arg of Libadalang.Analysis.Args (Diag) loop
         Last_Index := Index (Result, "{}", Last_Index);
         Replace_Slice
           (Result, Last_Index, Last_Index + 1, Render_Node (Arg));
      end loop;
      return (Actual_Loc, Result);
   end Render_Diagnostic;

   ---------------------------------
   -- Build_Contextual_Diagnostic --
   ---------------------------------

   function Build_Contextual_Diagnostics
     (Node        : Ada_Node;
      Sem_Diags   : Solver_Diagnostic_Array;
      Aggregate   : Diagnostic_Aggregator;
      Render_Node : Node_Renderer) return Contextual_Diagnostic
   is
      Result : Contextual_Diagnostic;
   begin
      if Sem_Diags'Length = 0 then
         Result.Error :=
           (Node, To_Unbounded_Text (To_Text ("resolution failed")));
         return Result;
      end if;
      Result := Aggregate (Sem_Diags, Render_Node);
      return Result;
   end Build_Contextual_Diagnostics;

   -----------------------
   -- Trace_Diagnostics --
   -----------------------

   procedure Trace_Diagnostics
     (Diags       : Solver_Diagnostic_Array;
      Render_Node : Node_Renderer)
   is
   begin
      if Diags_Trace.Is_Active then
         Diags_Trace.Trace ("Raw semantic diagnostics:");
         Diags_Trace.Increase_Indent;
         for Diag of Diags loop
            Diags_Trace.Trace
              (To_Pretty_String (Render_Diagnostic (Diag, Render_Node)));
            Diags_Trace.Trace (" (round" & Round (Diag)'Image & ")");
            Diags_Trace.Increase_Indent;
            for Ctx of Contexts (Diag) loop
               Diags_Trace.Trace
                 ("with " & Image (Ref_Node (Ctx).As_Ada_Node) &
                  " => " & Image (Decl_Node (Ctx).As_Ada_Node));
            end loop;
            Diags_Trace.Decrease_Indent;
         end loop;
         Diags_Trace.Decrease_Indent;
      end if;
   end Trace_Diagnostics;

   ----------------------
   -- Basic_Aggregator --
   ----------------------

   function Basic_Aggregator
     (Sem_Diags   : Solver_Diagnostic_Array;
      Render_Node : Node_Renderer) return Contextual_Diagnostic
   is
      use Ada.Containers;

      Max_Alt_Count : constant := 3;
      --  The maximal number of attempted candidates to show

      Res : Contextual_Diagnostic;

      package Solver_Diagnostic_Vectors is new Ada.Containers.Vectors
        (Positive, Solver_Diagnostic);

      package Ada_Node_Vectors is new Ada.Containers.Vectors
        (Positive, Ada_Node);

      Kept    : Solver_Diagnostic_Vectors.Vector;
      Targets : Ada_Node_Vectors.Vector;
   begin
      Trace_Diagnostics (Sem_Diags, Render_Node);

      for Diag of Sem_Diags loop
         for Ctx of Contexts (Diag) loop
            if not Targets.Contains (Ref_Node (Ctx).As_Ada_Node) then
               Kept.Clear;
               Targets.Clear;
               for Ctx of Contexts (Diag) loop
                  Targets.Append (Ref_Node (Ctx).As_Ada_Node);
               end loop;
               exit;
            end if;
         end loop;
         if Contexts (Diag)'Length = Targets.Length then
            Kept.Append (Diag);
         end if;
      end loop;

      if Kept.Length = 1 or else Targets.Length = 0 then
         Res.Error := Render_Diagnostic (Kept.First_Element, Render_Node);
         Res.Contexts.Clear;
      elsif (for some T of Targets => T.Kind in Ada_Op) then
         Res.Error :=
           (Targets.First_Element,
            To_Unbounded_Text ("no matching operator found"));
      else
         if Kept.Length > Max_Alt_Count then
            Res.Error :=
              (Targets.First_Element,
               To_Unbounded_Text
                 ("no matching alternative (showing"
                  & Max_Alt_Count'Wide_Wide_Image
                  & " out of"
                  & Kept.Length'Wide_Wide_Image
                  & " candidates)"));
         else
            Res.Error :=
              (Targets.First_Element,
               To_Unbounded_Text
                 ("no matching alternative (of"
                  & Kept.Length'Wide_Wide_Image
                  & " candidates)"));
         end if;

         for Diag of Kept loop
            declare
               use Libadalang.Implementation;
               use Libadalang.Public_Converters;

               Ctxs         : constant Logic_Context_Array := Contexts (Diag);
               Last_Ctx     : constant Logic_Context := Ctxs (Ctxs'Last);
               Internal_Ctx : constant Internal_Logic_Context :=
                 (Unwrap_Entity (Ref_Node (Last_Ctx)),
                  Unwrap_Entity (Decl_Node (Last_Ctx)));

               Base_Loc       : constant Ada_Node'Class := Location (Diag);
               Contextual_Loc : constant Ada_Node := Wrap_Node
                 (Ada_Node_P_Call_Context
                   (Unwrap_Node (Base_Loc),
                    Internal_Ctx));
            begin
               Res.Contexts.Append (Render_Diagnostic
                 (Diag,
                  Render_Node,
                  Contextual_Loc));
            end;
            exit when Res.Contexts.Length >= Max_Alt_Count;
         end loop;
      end if;

      return Res;
   end Basic_Aggregator;

   -------------------------
   -- Basic_Node_Renderer --
   -------------------------

   function Basic_Node_Renderer (Node : Ada_Node) return Text_Type is
      function Render_Instantiations
        (Instances : Generic_Instantiation_Array) return Text_Type;
      --  Render the given array of generic instantiations in the following
      --  format: "instance at line X, instance at line Y".

      function Render_Basic_Decl (Decl : Basic_Decl) return Text_Type;
      --  Render the given basic decl by returning its name together with
      --  the current generic context, if any.

      function Sloc_Image (Node : Ada_Node) return Text_Type;
      --  Return the output of ``Node.Full_Sloc_Image`` but without the
      --  trailing ": " characters.

      ---------------------------
      -- Render_Instantiations --
      ---------------------------

      function Render_Instantiations
        (Instances : Generic_Instantiation_Array) return Text_Type
      is
         First : constant Generic_Instantiation := Instances (Instances'First);
         Line  : constant Text_Type :=
            First.Sloc_Range.Start_Line'Wide_Wide_Image;
         Text  : constant Text_Type := "instance at line" & Line;
      begin
         if Instances'Length > 1 then
            return Text & ", " & Render_Instantiations
              (Instances (Instances'First + 1 .. Instances'Last));
         else
            return Text;
         end if;
      end Render_Instantiations;

      -----------------------
      -- Render_Basic_Decl --
      -----------------------

      function Render_Basic_Decl (Decl : Basic_Decl) return Text_Type is
         Name      : constant Text_Type := Decl.P_Defining_Name.Text;
         Instances : constant Generic_Instantiation_Array :=
            Decl.P_Generic_Instantiations;
      begin
         if Instances'Length > 0 then
            return Name & " [" & Render_Instantiations (Instances) & "]";
         else
            return Name;
         end if;
      end Render_Basic_Decl;

      ----------------
      -- Sloc_Image --
      ----------------

      function Sloc_Image (Node : Ada_Node) return Text_Type is
         Img : constant Text_Type := Node.Full_Sloc_Image;
      begin
         return Img (Img'First .. Img'Last - 2);
      end Sloc_Image;
   begin
      if Node.Is_Null then
         return "<null>";
      elsif Node.Kind not in Ada_Basic_Decl then
         return "<unknown>";
      elsif Node.Kind in Ada_Anonymous_Type_Decl_Range then
         return (case Node.As_Anonymous_Type_Decl.F_Type_Def.Kind is
           when Ada_Base_Type_Access_Def =>
             "anonymous access on " & Basic_Node_Renderer
               (Node.As_Base_Type_Decl.P_Accessed_Type.As_Ada_Node),
           when Ada_Array_Type_Def =>
             "anonymous array type defined at " & Sloc_Image (Node),
           when others =>
             "anonymous type defined at " & Sloc_Image (Node));
      elsif Node.Kind in Ada_Classwide_Type_Decl then
         return Basic_Node_Renderer (Node.Parent) & "'Class";
      elsif Node = Node.P_Universal_Int_Type then
         return "universal integer";
      elsif Node = Node.P_Universal_Real_Type then
         return "universal real";
      else
         return Render_Basic_Decl (Node.As_Basic_Decl);
      end if;
   end Basic_Node_Renderer;
end Libadalang.Semantic_Diagnostics;
