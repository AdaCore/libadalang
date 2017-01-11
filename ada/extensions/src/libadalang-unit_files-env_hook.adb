package body Libadalang.Unit_Files.Env_Hook is

   procedure Handle_With_Decl (Ctx : Analysis_Context; Names : Name_List);
   --  Helper for the environment hook to handle WithDecl nodes

   procedure Handle_Unit_Decl (Ctx : Analysis_Context; Node : Basic_Decl);
   --  Helper for the environment hook to handle library-level unit decl nodes

   procedure Handle_Unit_Body (Ctx : Analysis_Context; Node : Body_Node);
   --  Helper for the environment hook to handle library-level unit body nodes

   procedure Ignore (Unit : Analysis_Unit) is null;
   --  Helper to discard analysis units from Fetch_Unit

   --------------
   -- Env_Hook --
   --------------

   procedure Env_Hook (Unit : Analysis_Unit; Node : Ada_Node) is
      Ctx : constant Analysis_Context := Get_Context (Unit);
   begin
      if Node.all in With_Clause_Type'Class then
         Handle_With_Decl (Ctx, With_Clause (Node).F_Packages);

      elsif Node.Parent.all in Library_Item_Type'Class then
         if Node.all in Body_Node_Type'Class then
            Handle_Unit_Body (Ctx, Body_Node (Node));
         elsif Node.all in Basic_Decl_Type'Class then
            Handle_Unit_Decl (Ctx, Basic_Decl (Node));
         end if;
      end if;
   end Env_Hook;

   ----------------------
   -- Handle_With_Decl --
   ----------------------

   procedure Handle_With_Decl (Ctx : Analysis_Context; Names : Name_List) is
   begin
      for N of Names.Children loop
         Ignore (Fetch_Unit (Ctx, N, Unit_Specification));
      end loop;
   end Handle_With_Decl;

   ----------------------
   -- Handle_Unit_Decl --
   ----------------------

   procedure Handle_Unit_Decl (Ctx : Analysis_Context; Node : Basic_Decl) is
      Names : Name_Array_Access;
   begin
      --  If this not a library-level subprogram/package decl, there is no spec
      --  to process.
      if Node.all not in Package_Decl_Type'Class
         and then Node.all not in Basic_Subp_Decl_Type'Class
         and then Node.all not in Generic_Package_Decl_Type'Class
         and then Node.all not in Generic_Subp_Decl_Type'Class
      then
         return;
      end if;

      Names := Node.P_Defining_Names;
      pragma Assert (Names.N = 1);

      declare
         N : constant Ada_Node := Ada_Node (Names.Items (1));
      begin
         Dec_Ref (Names);
         if N.all in Dotted_Name_Type'Class then
            Ignore (Fetch_Unit
              (Ctx,
               Ada_Node (Dotted_Name (N).F_Prefix),
               Unit_Specification));
         end if;
      end;
   end Handle_Unit_Decl;

   ----------------------
   -- Handle_Unit_Body --
   ----------------------

   procedure Handle_Unit_Body (Ctx : Analysis_Context; Node : Body_Node) is
      Names : Name_Array_Access;
   begin
      --  If this not a library-level subprogram/package body, there is no spec
      --  to process.
      if Node.all not in Package_Body_Type'Class
         and then Node.all not in Subp_Body_Type'Class
      then
         return;
      end if;

      Names := Node.P_Defining_Names;
      pragma Assert (Names.N = 1);

      declare
         N : constant Ada_Node := Ada_Node (Names.Items (1));
      begin
         Dec_Ref (Names);
         Ignore (Fetch_Unit (Ctx, N, Unit_Specification));
      end;
   end Handle_Unit_Body;

end Libadalang.Unit_Files.Env_Hook;
