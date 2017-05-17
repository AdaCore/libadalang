package body Libadalang.Unit_Files is

   ----------------
   -- Fetch_Unit --
   ----------------

   function Fetch_Unit
     (Ctx  : Analysis_Context;
      Name : Ada_Node;
      Kind : Unit_Kind) return Analysis_Unit
   is
      procedure Prepare_Semres (Unit : Analysis_Unit);
      --  Prepare semantic analysis and reference Unit from the current unit

      --------------------
      -- Prepare_Semres --
      --------------------

      procedure Prepare_Semres (Unit : Analysis_Unit) is
      begin
         if Root (Unit) /= null then
            Populate_Lexical_Env (Unit);
            Reference_Unit (From => Get_Unit (Name), Referenced => Unit);
         end if;
      end Prepare_Semres;

      UFP              : constant Unit_Provider_Access_Cst :=
         Unit_Provider (Ctx);

      Unit, First_Unit : Analysis_Unit;
      Current_Name     : Ada_Node := Name;

   begin
      --  In Ada, "with A.B" gives visibility to A and A.B. To process all
      --  "mentionned" units, the following loop iterates on ["A.B", "A"].

      while Current_Name /= null loop
         --  TODO??? Find a proper way to handle file not found, parsing error,
         --  etc.
         Unit := UFP.Get_Unit (Ctx, Current_Name, Kind);
         Prepare_Semres (Unit);

         --  The first iteration gives the unit we are required to return
         if First_Unit = No_Analysis_Unit then
            First_Unit := Unit;
         end if;

         --  Fetch the next mention name to process
         if Current_Name.all in Base_Id_Type'Class then
            Current_Name := null;
         elsif Current_Name.all in Dotted_Name_Type'Class then
            Current_Name := Ada_Node (Dotted_Name (Current_Name).F_Prefix);
         else
            raise Property_Error;
         end if;
      end loop;

      return First_Unit;
   end Fetch_Unit;

end Libadalang.Unit_Files;
