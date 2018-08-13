with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Characters.Handling;

with Libadalang.Common;                 use Libadalang.Common;
with Libadalang.Unit_Files.Default;

package body Libadalang.Auto_Provider is

   procedure Add_Entry
     (Provider : in out Auto_Unit_Provider;
      Filename : Virtual_File;
      CU       : Compilation_Unit);
   --  Add a CU -> Filename entry to Provider.Mapping

   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
     (Provider : in out Auto_Unit_Provider;
      Filename : Virtual_File;
      CU       : Compilation_Unit)
   is
      use Ada.Strings.Wide_Wide_Unbounded;

      FQN  : constant Symbol_Type_Array := CU.P_Fully_Qualified_Name;
      Kind : constant Unit_Kind := CU.P_Unit_Kind;
      Name : Unbounded_Wide_Wide_String;
   begin
      for I in FQN'Range loop
         if I > FQN'First then
            Append (Name, '.');
         end if;
         Append (Name, Image (FQN (I)));
      end loop;

      declare
         Key      : constant Symbol_Type := As_Key (To_Wide_Wide_String (Name),
                                                    Kind, Provider);
         Dummy_Cur : CU_To_File_Maps.Cursor;
         Inserted  : Boolean;
      begin
         Provider.Mapping.Insert (Key, Filename, Dummy_Cur, Inserted);

         --  TODO??? Somehow report duplicate entries
         pragma Unreferenced (Inserted);
      end;
   end Add_Entry;

   ----------------
   -- Find_Files --
   ----------------

   function Find_Files
     (Name_Pattern : GNAT.Regpat.Pattern_Matcher :=
        Default_Source_Filename_Pattern;
      Directories  : GNATCOLL.VFS.File_Array)
      return GNATCOLL.VFS.File_Array_Access
   is
      package File_Vectors is new Ada.Containers.Vectors
        (Positive, Virtual_File);
      use File_Vectors;

      Result : Vector;
   begin
      for D of Directories loop
         declare
            Files : File_Array_Access := Read_Dir_Recursive
               (D, Filter => Files_Only);
         begin
            for F of Files.all loop
               if GNAT.Regpat.Match (Name_Pattern, +F.Base_Name) then
                  Result.Append (F);
               end if;
            end loop;
            Unchecked_Free (Files);
         end;
      end loop;

      return R : File_Array_Access :=
         new File_Array (1 .. Natural (Result.Length))
      do
         for Cur in Result.Iterate loop
            R (To_Index (Cur)) := Element (Cur);
         end loop;
      end return;
   end Find_Files;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Auto_Unit_Provider;
      Name     : Text_Type;
      Kind     : Unit_Kind) return String
   is
      use CU_To_File_Maps;

      Cur : constant Cursor := Provider.Mapping.Find
        (As_Key (Name, Kind, Provider));
   begin
      if Cur = No_Element then
         return "";
      else
         return +Full_Name (Element (Cur));
      end if;
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Auto_Unit_Provider;
      Context     : Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit'Class
   is
      Filename : constant String := Provider.Get_Unit_Filename (Name, Kind);
   begin
      if Filename /= "" then
         return Get_From_File (Context, Filename, Charset, Reparse);
      else
         declare
            Str_Name : constant String :=
               Libadalang.Unit_Files.Default.Unit_String_Name (Name);
            Dummy_File : constant String :=
               Libadalang.Unit_Files.Default.File_From_Unit (Str_Name, Kind);
            Kind_Name  : constant String :=
              (case Kind is
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant String :=
               "Could not find source file for " & Str_Name & " (" & Kind_Name
               & ")";
         begin
            return Get_With_Error (Context, Dummy_File, Error, Charset);
         end;
      end if;
   end Get_Unit;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Provider : in out Auto_Unit_Provider) is
   begin
      Provider.Mapping.Clear;
      Destroy (Provider.Keys);
   end Release;

   --------------------------
   -- Create_Auto_Provider --
   --------------------------

   procedure Create_Auto_Provider
     (Provider    : out Auto_Unit_Provider;
      Input_Files : GNATCOLL.VFS.File_Array;
      Charset     : String := Default_Charset)
   is
      Context : constant Analysis_Context := Create_Context (Charset);
   begin
      --  Go through every input file and try to parse them
      for File of Input_Files loop
         declare
            F    : constant String := +File.Full_Name;
            Unit : constant Analysis_Unit :=
               Get_From_File (Context, F, Reparse => True);
            R    : constant Ada_Node := Root (Unit);
         begin
            if not Has_Diagnostics (Unit) then
               --  If parsing went fine, add the compilation units File
               --  contains to our internal mapping.
               --
               --  TODO??? Somehow report parsing errors.
               case R.Kind is
                  when Ada_Compilation_Unit =>
                     Add_Entry (Provider, File, R.As_Compilation_Unit);
                  when Ada_Compilation_Unit_List =>
                     for CU of R.Children loop
                        Add_Entry (Provider, File, CU.As_Compilation_Unit);
                     end loop;
                  when others =>
                     --  Ignore anything else: can can't determine what
                     --  compilation unit this file corresponds to if there is
                     --  no Compilation_Unit node.
                     null;
               end case;
            end if;
         end;

      end loop;
   end Create_Auto_Provider;

   --------------------------
   -- Create_Auto_Provider --
   --------------------------

   function Create_Auto_Provider
     (Input_Files : GNATCOLL.VFS.File_Array;
      Charset     : String := Default_Charset) return Auto_Unit_Provider is
   begin
      return Provider : Auto_Unit_Provider do
         Provider.Keys := Create;
         Create_Auto_Provider (Provider, Input_Files, Charset);
      end return;
   end Create_Auto_Provider;

   ------------
   -- As_Key --
   ------------

   function As_Key
     (Name     : Text_Type;
      Kind     : Unit_Kind;
      Provider : Auto_Unit_Provider) return Symbol_Type
   is
      Canon_Name  : constant Text_Type :=
         Ada.Wide_Wide_Characters.Handling.To_Lower (Name);
      Kind_Letter : constant Wide_Wide_Character :=
        (case Kind is
         when Unit_Specification => 's',
         when Unit_Body          => 'b');
   begin
      return Find (Provider.Keys, Kind_Letter & ':' & Canon_Name);
   end As_Key;

end Libadalang.Auto_Provider;
