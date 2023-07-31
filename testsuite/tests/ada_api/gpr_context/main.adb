with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

with Support; use Support;

procedure Main is

   procedure Check
     (Label         : String;
      Root_Project  : String;
      Project       : String := "";
      Event_Handler : Event_Handler_Reference := No_Event_Handler_Ref;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := 8);
   --  Load the requested Root_Project/Project, create an analysis context from
   --  it and the given additional arguments and perform various requests on
   --  that context.

   -----------
   -- Check --
   -----------

   procedure Check
     (Label         : String;
      Root_Project  : String;
      Project       : String := "";
      Event_Handler : Event_Handler_Reference := No_Event_Handler_Ref;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := 8)
   is
      Tree : Project_Tree_Access;
      Env  : Project_Environment_Access;
      Prj  : Project_Type := No_Project;
      Ctx  : Analysis_Context;
      U    : Analysis_Unit;
      N    : Basic_Decl;
   begin
      Put_Line ("== " & Label & " ==");
      New_Line;

      --  Load the requested tree and fetch the requested project (if any)

      Tree := new Project_Tree;
      Initialize (Env);
      Tree.Load (Create (+Root_Project), Env);
      if Project /= "" then
         Prj := Tree.Project_From_Name (Project);
      end if;

      --  Create the analysis context from that project. This may fail if the
      --  project is an aggregate, so protect against specific exceptions.

      begin
         Ctx := Create_Context_From_Project
           (Tree             => Tree,
            Project          => Prj,
            Env              => Env,
            Is_Project_Owner => True,
            Event_Handler    => Event_Handler,
            With_Trivia      => With_Trivia,
            Tab_Stop         => Tab_Stop);
      exception
         when Exc : Unsupported_View_Error =>
            Put_Line ("Unsupported_View_Error: " & Exception_Message (Exc));
            New_Line;

            Tree.Unload;
            Free (Tree);
            Free (Env);
            return;
      end;

      U := Ctx.Get_From_Provider ("pkg", Unit_Body);
      if U.Has_Diagnostics then
         for D of U.Diagnostics loop
            Put_Line (U.Format_GNU_Diagnostic (D));
         end loop;
         raise Program_Error;
      end if;

      --  To show that With_Trivia / Tab_Stop are properly forwarded to the
      --  analysis context constructor and that the default charset is
      --  correctly determined, show the first token (or trivia).

      Put_Line ("pkg%b first token/trivia: " & Image (U.First_Token));
      Put_Line ("pkg%b root node: " & U.Root.Image);

      --  To show that the unit provider works as expected, resolve the Pkg
      --  package spec from its body.

      N :=
        U.Root
        .As_Compilation_Unit.F_Body
        .As_Library_Item.F_Item
        .As_Package_Body.P_Previous_Part;
      Put_Line ("pkg%b previous part: " & N.Image);

      --  To show that configuration pragmas are properly detected from the
      --  project, print their list.

      for P of U.Root.As_Compilation_Unit.P_All_Config_Pragmas loop
         Put_Line ("Config pragma: " & P.Image);
      end loop;

      New_Line;
   end Check;

begin
   Check
     (Label        => "Simple: defaults",
      Root_Project => "simple/p.gpr");
   Check
     (Label        => "Simple: without trivia",
      Root_Project => "simple/p.gpr",
      With_Trivia  => False);
   Check
     (Label        => "Simple: tab stop = 4",
      Root_Project => "simple/p.gpr",
      Tab_Stop     => 4);

   Check
     (Label        => "UTF-8",
      Root_Project => "utf-8/p.gpr");

   Check
     (Label        => "Aggregate project (no specific view)",
      Root_Project => "aggregate/agg.gpr");
   Check
     (Label        => "Aggregate project (specific view: p2)",
      Root_Project => "aggregate/agg.gpr",
      Project      => "p2");

   Check
     (Label         => "Simple: event handler",
      Root_Project  => "simple/p.gpr",
      Event_Handler => Create_EH);

   Check
     (Label         => "Preprocessing (p1)",
      Root_Project  => "preprocessing/p1.gpr");
   Check
     (Label         => "Preprocessing (p2)",
      Root_Project  => "preprocessing/p2.gpr");

   Check
     (Label         => "Config pragmas (p1)",
      Root_Project  => "config_pragmas/p1.gpr");
   Check
     (Label         => "Config pragmas (p2)",
      Root_Project  => "config_pragmas/p2.gpr");

   Put_Line ("Done.");
end Main;
