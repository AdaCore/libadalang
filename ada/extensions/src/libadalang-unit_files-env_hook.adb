with Libadalang.AST.Types; use Libadalang.AST.Types;

package body Libadalang.Unit_Files.Env_Hook is

   procedure Handle_With_Decl (Ctx : Analysis_Context; Names : Name_List);
   --  Helper for the environment hook to handle WithDecl nodes

   procedure Handle_Unit_Body
     (Ctx         : Analysis_Context;
      Node        : Body_Node;
      Initial_Env : in out Lexical_Env);
   --  Helper for the environment hook to handle unit body nodes

   function Get_Main_Env (Unit : Analysis_Unit) return Lexical_Env;
   --  Return the main lexical environment for some analysis unit. For a
   --  library-level package declaration for instance, this is the environment
   --  of the package itself.

   --------------
   -- Env_Hook --
   --------------

   procedure Env_Hook
     (Unit        : Analysis_Unit;
      Node        : Ada_Node;
      Initial_Env : in out Lexical_Env)
   is
      Ctx : constant Analysis_Context := Get_Context (Unit);
   begin
      if Node.all in With_Clause_Type'Class then
         Handle_With_Decl (Ctx, With_Clause (Node).F_Packages);
      elsif Node.all in Body_Node_Type'Class then
         Handle_Unit_Body (Ctx, Body_Node (Node), Initial_Env);
      end if;
   end Env_Hook;

   ----------------------
   -- Handle_With_Decl --
   ----------------------

   procedure Handle_With_Decl (Ctx : Analysis_Context; Names : Name_List) is
   begin
      for N of Names.all loop
         declare
            UFP : constant Unit_File_Provider_Access_Cst :=
               Unit_File_Provider (Ctx);
            Unit_File_Name : constant String :=
               UFP.Get_File (N, Unit_Specification);
            Unit           : constant Analysis_Unit :=
               Get_From_File (Ctx, Unit_File_Name);
         begin
            --  TODO??? Should we do something special if the Unit has parsing
            --  errors?
            if Root (Unit) /= null then
               Populate_Lexical_Env (Unit);
               Reference_Unit (From => Get_Unit (Names), Referenced => Unit);
            end if;
         end;
      end loop;
   end Handle_With_Decl;

   ----------------------
   -- Handle_Unit_Body --
   ----------------------

   procedure Handle_Unit_Body
     (Ctx         : Analysis_Context;
      Node        : Body_Node;
      Initial_Env : in out Lexical_Env)
   is
      Names     : Name_Array_Access;
   begin
      --  If this unit is not a body we are interested in, there is no spec to
      --  process.
      if Node.all not in Subp_Body_Type'Class
         and then Node.all not in Package_Body_Type'Class
      then
         return;
      end if;

      Names := Node.P_Defining_Names;

      declare
         UFP : constant Unit_File_Provider_Access_Cst :=
            Unit_File_Provider (Ctx);
         N    : constant Ada_Node := Ada_Node (Names.Items (1));
         Name : constant String := UFP.Get_File (N, Unit_Specification);

         --  TODO??? Maybe we should just return when the file does not exist:
         --  after all it is legal.
         Unit : Analysis_Unit := Get_From_File (Ctx, Name);
      begin
         Dec_Ref (Names);
         if Root (Unit) /= null then
            Populate_Lexical_Env (Unit);
            Initial_Env := Get_Main_Env (Unit);
            Reference_Unit (From => Get_Unit (Node), Referenced => Unit);
         end if;
      exception
         when Property_Error =>
            Dec_Ref (Names);
            raise;
      end;
   end Handle_Unit_Body;

   ------------------
   -- Get_Main_Env --
   ------------------

   function Get_Main_Env (Unit : Analysis_Unit) return Lexical_Env is
      N : constant Ada_Node := Root (Unit);
      C : Compilation_Unit;
      B : Basic_Decl;
   begin
      if N = null or else N.all not in Compilation_Unit_Type'Class then
         return AST_Envs.Empty_Env;
      end if;

      C := Compilation_Unit (N);
      if C.F_Body = null or else C.F_Body.all not in Library_Item_Type'Class
      then
         return AST_Envs.Empty_Env;
      end if;

      B := Library_Item (C.F_Body).F_Item;
      return (if B = null
              then AST_Envs.Empty_Env
              else B.Children_Env);
   end Get_Main_Env;

end Libadalang.Unit_Files.Env_Hook;
