--  Check that Load_From_Project works as expected

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with GNATCOLL.Traces;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Libadalang.Analysis;           use Libadalang.Analysis;
with Libadalang.Common;             use Libadalang.Common;
with Libadalang.Data_Decomposition; use Libadalang.Data_Decomposition;
with Libadalang.Iterators;          use Libadalang.Iterators;
with Libadalang.Project_Provider;   use Libadalang.Project_Provider;

procedure Main is

   Ctx     : Analysis_Context;
   Tree    : GPR2.Project.Tree.Object;
   View    : GPR2.Project.View.Object;
   Repinfo : Repinfo_Collection;

   function "+"
     (S : String) return Unbounded_String renames To_Unbounded_String;

   procedure Load_Project
     (Filename   : String;
      Var_Name   : GPR2.Optional_Name_Type := "";
      Var_Value  : GPR2.Value_Type := "";
      Subdirs    : String := "";
      Subproject : String := "");
   --  Load in ``Tree`` the project file ``Filename` with the given settings
   --  (``Var_Name``/``Var_Value`` for an external variable if ``Var_Name`` is
   --  not empty, ``Subdirs`` for the homonym GPR2 argument.
   --
   --  Set ``View`` to ``Undefined`` if ``Subproject`` is the empty string, and
   --  to the view with the corresponding name otherwise.

   procedure Check (Sources : Filename_Array);
   --  Load all Ada sources files denoted by the filenames in ``Sources`` and
   --  lookup type information for the (only expected) type declaration they
   --  contain.

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Filename   : String;
      Var_Name   : GPR2.Optional_Name_Type := "";
      Var_Value  : GPR2.Value_Type := "";
      Subdirs    : String := "";
      Subproject : String := "")
   is
      Context : GPR2.Context.Object;
   begin
      if Var_Name'Length > 0 then
         Context.Insert (Var_Name, Var_Value);
      end if;

      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
                       (Name      => GPR2.Filename_Type (Filename),
                        Directory => GPR2.Path_Name.No_Resolution),
         Context  => Context,
         Subdirs  => GPR2.Optional_Name_Type (Subdirs));

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

      Ctx := Create_Context
        (Unit_Provider => Create_Project_Unit_Provider (Tree, View));
   end Load_Project;

   -----------
   -- Check --
   -----------

   procedure Check (Sources : Filename_Array) is
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
   Load_Project ("simple.gpr");
   Repinfo := Load_From_Project (Tree, View);
   Check ((1 => +"src/simple/simple.ads"));
   New_Line;

   --  Make sure Load_From_Project honors its View formal: it is not supposed
   --  to load representation information for the Tree_Root project/unit.

   Put_Line ("== Tree ==");
   New_Line;
   Load_Project ("tree_root.gpr", Subproject => "tree_child");
   Repinfo := Load_From_Project (Tree, View);
   Check
     ((+"src/tree_root/tree_root.ads",
       +"src/tree_child/tree_child.ads"));
   New_Line;

   --  Make sure Load_From_Project correctly procesess a project loaded with
   --  some Subdirs argument, and that it can nevertheless use another Subdirs
   --  for its processings.

   Put_Line ("== Subdirs ==");
   New_Line;
   Load_Project ("simple.gpr", Subdirs => "somesubdirs");
   Repinfo := Load_From_Project (Tree, View, Subdirs => "othersubdirs");
   Check ((1 => +"src/simple/simple.ads"));
   New_Line;

   --  Make sure Load_From_Project passes expected external variables to
   --  gprbuild: without setting MY_VAR to a non-empty value, the project would
   --  have no source, and calling gprbuild would produce no JSON file.

   Put_Line ("== With vars ==");
   New_Line;
   Load_Project ("with_var.gpr", Var_Name => "MY_VAR", Var_Value => "foo");
   Repinfo := Load_From_Project (Tree, View);
   Check ((1 => +"src/with_var/with_var.ads"));
   New_Line;

   Put_Line ("Done.");
end Main;
