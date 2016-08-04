with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

with Interfaces; use Interfaces;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.AST.Types; use Libadalang.AST.Types;

package body Libadalang.Unit_Files is

   function Get_Unit_Name (N : Name) return Text_Type;
   --  Return a Name as a string. For instance: "Foo.Bar". Raise a
   --  Property_Error if N is not a valid unit name.

   function Get_Unit_File_Name (Name : Text_Type) return String;
   --  Return the file name corresponding to a unit name. Raise a
   --  Property_Error if N is not a valid unit name.
   --
   --  TODO??? Right now, this handles only pure ASCII unit names as it's not
   --  clear how we should handle Unicode characters for file names.

   function Get_Unit_File_Name (N : Name) return String is
     (Get_Unit_File_Name (Get_Unit_Name (N)));

   procedure Handle_With_Decl (Ctx : Analysis_Context; Names : List_Name);
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
      if Node.all in With_Decl_Type'Class then
         Handle_With_Decl (Ctx, With_Decl (Node).F_Packages);
      elsif Node.all in Body_Node_Type'Class then
         Handle_Unit_Body (Ctx, Body_Node (Node), Initial_Env);
      end if;
   end Env_Hook;

   ----------------------
   -- Handle_With_Decl --
   ----------------------

   procedure Handle_With_Decl (Ctx : Analysis_Context; Names : List_Name) is
   begin
      for N of Names.all loop
         declare
            Unit_File_Name : constant String :=
               Get_Unit_File_Name (Name (N)) & ".ads";
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
      if Node.all not in Subprogram_Body_Type'Class
         and then Node.all not in Package_Body_Type'Class
      then
         return;
      end if;

      Names := Node.P_Defining_Names;

      declare
         Name : constant String :=
           Get_Unit_File_Name (Names.Items (1)) & ".ads";

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

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name (N : Name) return Text_Type is
   begin
      if N.all in Identifier_Type'Class then
         return Data (Identifier (N).F_Tok).Text.all;

      elsif N.all in Dotted_Name_Type'Class then
         declare
            DN : constant Dotted_Name := Dotted_Name (N);
         begin
            if DN.F_Prefix.all in Name_Type'Class
               and then DN.F_Suffix.all in Identifier_Type'Class
            then
               return (Get_Unit_Name (Name (DN.F_Prefix))
                       & "."
                       & Get_Unit_Name (Name (DN.F_Suffix)));
            end if;
         end;
      end if;

      raise Property_Error with "invalid AST node for unit name";
   end Get_Unit_Name;

   ------------------------
   -- Get_Unit_File_Name --
   ------------------------

   function Get_Unit_File_Name (Name : Text_Type) return String is
      Result : String (1 .. Name'Length);
      I      : Positive := 1;
   begin
      --  Make Name lower case and replace dots with dashes. Only allow ASCII.
      for C of Name loop
         declare
            CN : constant Unsigned_32 := Wide_Wide_Character'Pos (C);
         begin
            if C = '.' then
               Result (I) := '-';
            elsif CN in 16#20# .. 16#7f# then
               Result (I) := Ada.Strings.Maps.Value
                 (Ada.Strings.Maps.Constants.Lower_Case_Map,
                  Character'Val (CN));
            else
               raise Property_Error with "unhandled unit name";
            end if;
         end;
         I := I + 1;
      end loop;
      return Result;
   end Get_Unit_File_Name;

end Libadalang.Unit_Files;
