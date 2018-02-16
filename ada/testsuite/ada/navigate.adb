with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.VFS;
with Langkit_Support.Text;
with Libadalang.Analysis;
with Libadalang.Iterators;

with Put_Title;

procedure Navigate is

   package CMD renames Ada.Command_Line;
   package LAL renames Libadalang.Analysis;
   package LALIT renames Libadalang.Iterators;

   function Short_Image (Node : LAL.Ada_Node'Class) return String
   is (Langkit_Support.Text.Image (Node.Short_Image));

   function To_Lower (S : String) return String
      renames Ada.Characters.Handling.To_Lower;

   Fatal_Error   : exception;
   Ctx           : LAL.Analysis_Context;
   Enabled_Kinds : array (LAL.Ada_Node_Kind_Type) of Boolean :=
     (others => False);

   function Is_Navigation_Disabled (N : LAL.Ada_Node) return Boolean;

   function Node_Filter (N : LAL.Ada_Node) return Boolean
   is (Enabled_Kinds (N.Kind) and then not Is_Navigation_Disabled (N));

   procedure Stop_With_Error
     (Message    : String;
      With_Usage : Boolean := False);
   procedure Print_Usage;
   procedure Process_File (Unit : LAL.Analysis_Unit);
   procedure Print_Navigation
     (Part_Name : String; Orig, Dest : LAL.Ada_Node'Class);
   procedure Decode_Kinds (List : String);

   function Basename (Filename : String) return String;
   --  Return the base name of the Filename path

   --------------
   -- Basename --
   --------------

   function Basename (Filename : String) return String is
      use GNATCOLL.VFS;
   begin
      return +Create (+Filename).Base_Name;
   end Basename;

   ---------------------
   -- Stop_With_Error --
   ---------------------

   procedure Stop_With_Error
     (Message    : String;
      With_Usage : Boolean := False) is
   begin
      Put_Line (CMD.Command_Name & ": " & Message);
      if With_Usage then
         New_Line;
         Print_Usage;
      end if;
      raise Fatal_Error;
   end Stop_With_Error;

   -----------------
   -- Print_Usage --
   -----------------

   procedure Print_Usage is
   begin
      Put_Line
        ("Usage: " & CMD.Command_Name
         & " [KINDS] [SOURCE-FILES]"
         & ASCII.LF & ASCII.LF
         & "Exercise navigation between AST nodes (spec/body/...)."
         & ASCII.LF & ASCII.LF
         & "KINDS must be a comma-separated list of AST node kind names, like"
         & " Ada_Subp_Body,Ada_Package_Decl. This will filter the nodes on"
         & " which we exercise navigation properties."
         & ASCII.LF & ASCII.LF
         & "SOURCE-FILES must be a list of source files to process.");
   end Print_Usage;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File (Unit : LAL.Analysis_Unit) is
   begin
      if LAL.Has_Diagnostics (Unit) then
         for D of LAL.Diagnostics (Unit) loop
            Put_Line (LAL.Format_GNU_Diagnostic (Unit, D));
         end loop;
         New_Line;
         return;
      end if;

      declare
         It            : LALIT.Local_Find_Iterator :=
            LALIT.Find (LAL.Root (Unit), Node_Filter'Access);
         Node          : LAL.Ada_Node;
         At_Least_Once : Boolean := False;
      begin
         while It.Next (Node) loop
            declare
               Processed_Something : Boolean := True;
            begin
               case Node.Kind is

                  --  Packages

                  when LAL.Ada_Base_Package_Decl =>

                     Print_Navigation
                       ("Body", Node,
                        Node.As_Base_Package_Decl.P_Body_Part);
                  when LAL.Ada_Package_Body =>
                     Print_Navigation
                       ("Decl", Node, Node.As_Package_Body.P_Decl_Part);

                  when LAL.Ada_Generic_Package_Decl =>
                     Print_Navigation
                       ("Body", Node,
                        Node.As_Generic_Package_Decl.P_Body_Part);

                  --  Subprograms

                  when LAL.Ada_Subp_Decl =>
                     Print_Navigation
                       ("Body", Node, Node.As_Subp_Decl.P_Body_Part);
                  when LAL.Ada_Subp_Body =>
                     Print_Navigation
                       ("Decl", Node, Node.As_Subp_Body.P_Decl_Part);

                  when LAL.Ada_Generic_Subp_Decl =>
                     Print_Navigation
                       ("Body", Node,
                        Node.As_Generic_Subp_Decl.P_Body_Part);

                  when others =>
                     Processed_Something := False;

               end case;
               At_Least_Once := At_Least_Once or else Processed_Something;
            exception
               when LAL.Property_Error =>
                  Put_Line ("Error when processing " & Short_Image (Node));
                  At_Least_Once := True;
            end;
         end loop;
         if not At_Least_Once then
            Put_Line ("<no node to process>");
         end if;
            New_Line;
      end;
   end Process_File;

   ----------------------
   -- Print_Navigation --
   ----------------------

   procedure Print_Navigation
     (Part_Name : String; Orig, Dest : LAL.Ada_Node'Class) is
   begin
      if Dest.Is_Null then
         Put_Line
           (Short_Image (Orig) & " has no " & To_Lower (Part_Name));
      else
         Put_Line
           (Part_Name & " of " & Short_Image (Orig) & " is "
            & Short_Image (Dest)
            & " [" & Basename (LAL.Get_Filename (Dest.Unit)) & "]");
      end if;
   end Print_Navigation;

   ------------------
   -- Decode_Kinds --
   ------------------

   procedure Decode_Kinds (List : String) is
      Start : Positive := List'First;

      procedure Process_Name (Name : String);

      ------------------
      -- Process_Name --
      ------------------

      procedure Process_Name (Name : String) is
      begin
         if Name'Length /= 0 then
            begin
               declare
                  Kind : constant LAL.Ada_Node_Kind_Type :=
                     LAL.Ada_Node_Kind_Type'Value (Name);
               begin
                  Enabled_Kinds (Kind) := True;
               end;
            exception
               when Constraint_Error =>
                  Stop_With_Error ("invalid kind name: " & Name);
            end;
         end if;
      end Process_Name;

   begin
      for Cur in Start .. List'Last loop
         if List (Cur) = ',' then
            Process_Name (List (Start .. Cur - 1));
            Start := Cur + 1;
         elsif Cur = List'Last then
            Process_Name (List (Start .. Cur));
         end if;
      end loop;
   end Decode_Kinds;

   ----------------------------
   -- Is_Navigation_Disabled --
   ----------------------------

   function Is_Navigation_Disabled (N : LAL.Ada_Node) return Boolean is

      function Lowercase_Name (Id : LAL.Identifier) return String is
        (To_Lower (Langkit_Support.Text.Image (Id.Text)));

      function Has_Disable_Navigation
        (Aspects : LAL.Aspect_Spec) return Boolean;

      ----------------------------
      -- Has_Disable_Navigation --
      ----------------------------

      function Has_Disable_Navigation
        (Aspects : LAL.Aspect_Spec) return Boolean
      is
         use type LAL.Ada_Node_Kind_Type;
      begin
         if Aspects.Is_Null then
            return False;
         end if;
         for Child of LAL.Ada_Node_Array'(Aspects.F_Aspect_Assocs.Children)
         loop
            declare
               Assoc : constant LAL.Aspect_Assoc := Child.As_Aspect_Assoc;
            begin
               if Assoc.F_Id.Kind = LAL.Ada_Identifier then
                  declare
                     Id : constant LAL.Identifier := Assoc.F_Id.As_Identifier;
                  begin
                     return Lowercase_Name (Id) = "disable_navigation";
                  end;
               end if;
            end;
         end loop;
         return False;
      end Has_Disable_Navigation;

   begin
      case N.Kind is
         when LAL.Ada_Base_Package_Decl =>
            return Has_Disable_Navigation (N.As_Base_Package_Decl.F_Aspects);
         when others =>
            return False;
      end case;
   end Is_Navigation_Disabled;

begin
   if CMD.Argument_Count < 2 then
      Stop_With_Error ("not enough arguments", True);
   end if;

   Decode_Kinds (CMD.Argument (1));
   Ctx := LAL.Create;

   for I in 2 .. CMD.Argument_Count loop
      declare
         Arg  : constant String := CMD.Argument (I);
         Unit : constant LAL.Analysis_Unit := LAL.Get_From_File (Ctx, Arg);
      begin
         Put_Title ('#', Arg);
         Process_File (Unit);
      end;
   end loop;

   LAL.Destroy (Ctx);
   Put_Line ("Done.");

exception
   when Fatal_Error =>
      CMD.Set_Exit_Status (CMD.Failure);
end Navigate;
