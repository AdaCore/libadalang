--  Check that Load_From_Project works as expected

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with GNATCOLL.Traces;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Libadalang.Analysis;           use Libadalang.Analysis;
with Libadalang.Common;             use Libadalang.Common;
with Libadalang.Data_Decomposition; use Libadalang.Data_Decomposition;
with Libadalang.Iterators;          use Libadalang.Iterators;
with Libadalang.Project_Provider;   use Libadalang.Project_Provider;

procedure Main is

   Repinfo : Repinfo_Collection;

   function "+"
     (S : String) return Unbounded_String renames To_Unbounded_String;

   procedure Load_Project
     (Tree       : in out GPR2.Project.Tree.Object;
      View       : out GPR2.Project.View.Object;
      Filename   : String;
      Var_Name   : String := "";
      Var_Value  : String := "";
      Subdirs    : String := "";
      Subproject : String := "");
   --  Load in ``Tree`` the project file ``Filename` with the given settings
   --  (``Var_Name``/``Var_Value`` for an external variable if ``Var_Name`` is
   --  not empty, ``Subdirs`` for the homonym GPR2 argument.
   --
   --  Set ``View`` to ``Undefined`` if ``Subproject`` is the empty string, and
   --  to the view with the corresponding name otherwise.

   procedure Check
     (Tree    : GPR2.Project.Tree.Object;
      View    : GPR2.Project.View.Object;
      Sources : Filename_Array);
   --  Load all Ada sources files denoted by the filenames in ``Sources`` and
   --  lookup type information for the (only expected) type declaration they
   --  contain.

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Tree       : in out GPR2.Project.Tree.Object;
      View       : out GPR2.Project.View.Object;
      Filename   : String;
      Var_Name   : String := "";
      Var_Value  : String := "";
      Subdirs    : String := "";
      Subproject : String := "")
   is
      Options : GPR2.Options.Object;
   begin
      Options.Add_Switch (GPR2.Options.P, Filename);
      if Subdirs /= "" then
         Options.Add_Switch (GPR2.Options.Subdirs, Subdirs);
      end if;
      if Var_Name'Length > 0 then
         Options.Add_Switch (GPR2.Options.X, Var_Name & "=" & Var_Value);
      end if;

      if not Tree.Load
               (Options,
                With_Runtime         => True,
                Artifacts_Info_Level => GPR2.Sources_Units,
                Absent_Dir_Error     => GPR2.No_Error)
      then
         raise Program_Error;
      end if;

      View := GPR2.Project.View.Undefined;
      if Subproject'Length > 0 then
         for V of Tree loop
            if To_Lower (String (V.Name)) = To_Lower (Subproject) then
               View := V;
            end if;
         end loop;
         if not View.Is_Defined then
            raise Program_Error with
              "cannot find " & Subproject & " in " & Filename;
         end if;
      end if;
   end Load_Project;

   -----------
   -- Check --
   -----------

   procedure Check
     (Tree    : GPR2.Project.Tree.Object;
      View    : GPR2.Project.View.Object;
      Sources : Filename_Array)
   is
      Ctx    : constant Analysis_Context := Create_Context
        (Unit_Provider => Create_Project_Unit_Provider (Tree, View));
      U      : Analysis_Unit;
      T      : Base_Type_Decl;
      T_Info : Type_Representation;
   begin
      for Filename of Sources loop
         declare
            S : constant String := To_String (Filename);
         begin
            U := Ctx.Get_From_File (S);
            T := Find_First
              (U.Root, Kind_Is (Ada_Concrete_Type_Decl)).As_Base_Type_Decl;

            if U.Has_Diagnostics then
               for D of U.Diagnostics loop
                  Put_Line (U.Format_GNU_Diagnostic (D));
               end loop;
               raise Program_Error;
            end if;

            T_Info := Repinfo.Lookup (T);
            if Is_Null (T_Info) then
               Put_Line ("Cannot find type representation of " & T.Image);
            else
               Put_Line (T.Image & " is a " & Kind (T_Info)'Image);
            end if;
         end;
      end loop;
   end Check;

begin
   GNATCOLL.Traces.Parse_Config_File;

   --  Test Load_From_Project on the simplest project

   Put_Line ("== Simple ==");
   New_Line;
   declare
      Tree : GPR2.Project.Tree.Object;
      View : GPR2.Project.View.Object;
   begin
      Load_Project (Tree, View, "simple.gpr");
      Repinfo := Load_From_Project (Tree, View);
      Check (Tree, View, (1 => +"src/simple/simple.ads"));
   end;
   New_Line;

   --  Make sure Load_From_Project honors its View formal: it is not supposed
   --  to load representation information for the Tree_Root project/unit.

   Put_Line ("== Tree ==");
   New_Line;
   declare
      Tree : GPR2.Project.Tree.Object;
      View : GPR2.Project.View.Object;
   begin
      Load_Project (Tree, View, "tree_root.gpr", Subproject => "tree_child");
      Repinfo := Load_From_Project (Tree, View);
      Check
        (Tree,
         View,
         (+"src/tree_root/tree_root.ads",
          +"src/tree_child/tree_child.ads"));
   end;
   New_Line;

   --  Make sure Load_From_Project correctly procesess a project loaded with
   --  some Subdirs argument, and that it can nevertheless use another Subdirs
   --  for its processings.

   Put_Line ("== Subdirs ==");
   New_Line;
   declare
      Tree : GPR2.Project.Tree.Object;
      View : GPR2.Project.View.Object;
   begin
      Load_Project (Tree, View, "simple.gpr", Subdirs => "somesubdirs");
      Repinfo := Load_From_Project (Tree, View, Subdirs => "othersubdirs");
      Check (Tree, View, (1 => +"src/simple/simple.ads"));
   end;
   New_Line;

   --  Make sure Load_From_Project passes expected external variables to
   --  gprbuild: without setting MY_VAR to a non-empty value, the project would
   --  have no source, and calling gprbuild would produce no JSON file.

   Put_Line ("== With vars ==");
   New_Line;
   declare
      Tree : GPR2.Project.Tree.Object;
      View : GPR2.Project.View.Object;
   begin
      Load_Project
        (Tree, View, "with_var.gpr", Var_Name => "MY_VAR", Var_Value => "foo");
      Repinfo := Load_From_Project (Tree, View);
      Check (Tree, View, (1 => +"src/with_var/with_var.ads"));
   end;
   New_Line;

   Put_Line ("Done.");
end Main;
