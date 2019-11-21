with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;
with GNATCOLL.VFS;

with Langkit_Support.Text;
with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Helpers;
with Libadalang.Iterators;

with Put_Title;

procedure Navigate is

   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   package LALIT renames Libadalang.Iterators;

   package X renames GNATCOLL.Strings;

   type Enabled_Kinds_Type is array (LALCO.Ada_Node_Kind_Type) of Boolean;
   All_Kinds : constant Enabled_Kinds_Type := (others => True);

   function String_To_Kinds (List : String) return Enabled_Kinds_Type;

   procedure Process_File
     (Context : Libadalang.Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);

   package App is new Libadalang.Helpers.App
     (Description     => "Navigate between AST nodes (spec/body/...).",
      Process_Unit    => Process_File);

   package Args is
      use GNATCOLL.Opt_Parse;

      package Kinds is new Parse_Option
        (App.Args.Parser, "-k", "--kinds",
         "Comma-separated list of AST node kind names, like"
         & " ""Ada_Subp_Body,Ada_Package_Decl"". This will filter the"
         & " nodes on which we navigate.",
         Enabled_Kinds_Type,
         Default_Val => All_Kinds,
         Convert     => String_To_Kinds);
   end Args;

   function Short_Image (Node : LAL.Ada_Node'Class) return String
   is (Langkit_Support.Text.Image (Node.Short_Text_Image));

   function To_Lower (S : String) return String
      renames Ada.Characters.Handling.To_Lower;

   function Is_Navigation_Disabled (N : LAL.Ada_Node) return Boolean;

   procedure Print_Navigation
     (Part_Name : String; Orig, Dest : LAL.Ada_Node'Class);

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

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File
     (Context : Libadalang.Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      pragma Unreferenced (Context);
      use GNATCOLL.VFS;

      At_Least_Once : Boolean := False;

      function Filter (N : LAL.Ada_Node) return Boolean is
        (Args.Kinds.Get (N.Kind) and then not Is_Navigation_Disabled (N));
   begin
      Put_Title ('#', +Create (+Unit.Get_Filename).Base_Name);

      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         New_Line;
         return;
      end if;

      for Node of LALIT.Find (Unit.Root, Filter'Access).Consume loop
         declare
            Processed_Something : Boolean := True;
         begin
            case Node.Kind is

               --  Bodies

               when LALCO.Ada_Body_Node =>
                  Print_Navigation
                    ("Body previous part", Node,
                     Node.As_Body_Node.P_Previous_Part);

                  Print_Navigation
                    ("Decl", Node,
                     Node.As_Body_Node.P_Decl_Part);

                  case Node.Kind is
                     when LALCO.Ada_Package_Body_Stub =>
                        Print_Navigation
                          ("Body", Node,
                           Node.As_Package_Body_Stub.P_Body_Part_For_Decl);

                     when others => null;
                  end case;

               --  Packages

               when LALCO.Ada_Base_Type_Decl =>
                  Print_Navigation
                    ("Type previous part", Node,
                     Node.As_Base_Type_Decl.P_Previous_Part);

                  case Node.Kind is
                     when LALCO.Ada_Protected_Type_Decl =>
                        Print_Navigation
                          ("Protected decl next part", Node,
                           Node.As_Basic_Decl.P_Next_Part_For_Decl);

                        Print_Navigation
                          ("Protected decl body part", Node,
                           Node.As_Basic_Decl.P_Body_Part_For_Decl);

                     when others =>
                        --  Protected type decls don't have a type next part
                        Print_Navigation
                          ("Type next part", Node,
                           Node.As_Base_Type_Decl.P_Next_Part);
                  end case;

               when LALCO.Ada_Base_Package_Decl =>

                  Print_Navigation
                    ("Body", Node,
                     Node.As_Base_Package_Decl.P_Body_Part);

               when LALCO.Ada_Generic_Package_Decl =>
                  Print_Navigation
                    ("Body", Node,
                     Node.As_Generic_Package_Decl.P_Body_Part);

               --  Subprograms

               when LALCO.Ada_Basic_Subp_Decl =>
                  Print_Navigation
                    ("Body", Node, Node.As_Basic_Subp_Decl.P_Body_Part);

               when LALCO.Ada_Generic_Subp_Decl =>
                  Print_Navigation
                    ("Body", Node,
                     Node.As_Generic_Subp_Decl.P_Body_Part);

               when others =>
                  Processed_Something := False;

            end case;
            At_Least_Once := At_Least_Once or else Processed_Something;
         exception
            when LALCO.Property_Error =>
               Put_Line ("Error when processing " & Short_Image (Node));
               At_Least_Once := True;
         end;
      end loop;

      if not At_Least_Once then
         Put_Line ("<no node to process>");
      end if;

      New_Line;
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
            & " [" & Basename (Dest.Unit.Get_Filename) & "]");
      end if;
   end Print_Navigation;

   ------------------
   -- Decode_Kinds --
   ------------------

   function String_To_Kinds (List : String) return Enabled_Kinds_Type is
      Enabled_Kinds : Enabled_Kinds_Type := (others => False);

      Names : constant X.XString_Array := X.To_XString (List).Split (",");
   begin
      for Name of Names loop
         if Name.Length /= 0 then
            begin
               declare
                  Kind : constant LALCO.Ada_Node_Kind_Type :=
                     LALCO.Ada_Node_Kind_Type'Value (X.To_String (Name));
               begin
                  Enabled_Kinds (Kind) := True;
               end;
            exception
               when Constraint_Error =>
                  raise GNATCOLL.Opt_Parse.Opt_Parse_Error
                  with "invalid kind name: " & X.To_String (Name);
            end;
         end if;
      end loop;
      return Enabled_Kinds;
   end String_To_Kinds;

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
         use type LALCO.Ada_Node_Kind_Type;
      begin
         if Aspects.Is_Null then
            return False;
         end if;
         for Child of LAL.Ada_Node_Array'(Aspects.F_Aspect_Assocs.Children)
         loop
            declare
               Assoc : constant LAL.Aspect_Assoc := Child.As_Aspect_Assoc;
            begin
               if Assoc.F_Id.Kind = LALCO.Ada_Identifier then
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
         when LALCO.Ada_Base_Package_Decl =>
            return Has_Disable_Navigation (N.As_Base_Package_Decl.F_Aspects);
         when others =>
            return False;
      end case;
   end Is_Navigation_Disabled;

begin
   App.Run;
   Put_Line ("Done.");
end Navigate;
