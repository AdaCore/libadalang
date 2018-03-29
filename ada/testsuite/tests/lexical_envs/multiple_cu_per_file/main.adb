with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;            use Libadalang.Analysis;
with Libadalang.Iterators;           use Libadalang.Iterators;
with Libadalang.Unit_Files.Projects; use Libadalang.Unit_Files.Projects;

procedure Main is

   function Load_Unit_Provider return Unit_Provider_Access;
   function Is_Fact_Id (N : Ada_Node) return Boolean;
   procedure Resolve;

   ------------------------
   -- Load_Unit_Provider --
   ------------------------

   function Load_Unit_Provider return Unit_Provider_Access is
      Env     : Project_Environment_Access;
      Project : constant Project_Tree_Access := new Project_Tree;
   begin
      Initialize (Env);
      Project.Load (Create (+"p.gpr"), Env);
      return new Project_Unit_Provider_Type'(Create (Project, Env, True));
   end Load_Unit_Provider;

   ----------------
   -- Is_Fact_Id --
   ----------------

   function Is_Fact_Id (N : Ada_Node) return Boolean is
   begin
      return N.Kind = Ada_Identifier and then Text_Type'(N.Text) = "Fact";
   end Is_Fact_Id;

   UP   : Unit_Provider_Access := Load_Unit_Provider;
   Ctx  : Analysis_Context := Create
     (Unit_Provider => Unit_Provider_Access_Cst (UP));

   Foo_Unit     : constant Analysis_Unit := Get_From_File (Ctx, "foo.adb");
   Specs_1_Unit : constant Analysis_Unit := Get_From_File (Ctx, "specs-1.ada");
   Specs_2_Unit : constant Analysis_Unit := Get_From_File (Ctx, "specs-2.ada");

   -------------
   -- Resolve --
   -------------

   procedure Resolve is
      Call_Id : constant Ada_Node :=
         Find_First (Root (Foo_Unit), Is_Fact_Id'Access);
   begin
      Put_Line ("Fact resolves to: "
                & Image (Call_Id.P_Referenced_Decl.Short_Image));
      New_Line;
   end Resolve;

   Dummy : Analysis_Unit;

begin
   Discard_Errors_In_Populate_Lexical_Env (Ctx, False);
   Populate_Lexical_Env (Specs_1_Unit);
   Populate_Lexical_Env (Specs_2_Unit);

   Put_Line ("From scratch:");
   Resolve;

   Put_Line ("Reloading specs-1.ada:");
   Dummy := Get_From_File (Ctx, "specs-1.ada", Reparse => True);
   Resolve;

   Put_Line ("Reloading specs-2.ada:");
   Dummy := Get_From_File (Ctx, "specs-2.ada", Reparse => True);
   Resolve;

   Destroy (Ctx);
   Destroy (UP);
   Put_Line ("Done.");
end Main;
