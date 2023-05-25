--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Characters.Handling;

with Libadalang.Unit_Files;

package body Libadalang.Auto_Provider is

   ----------------
   -- Find_Files --
   ----------------

   function Find_Files
     (Filter       : not null access function (Name : String) return Boolean;
      Directories  : GNATCOLL.VFS.File_Array)
      return GNATCOLL.VFS.File_Array_Access;
   --  Common implementation for the two public ``Find_Files`` functions:
   --  return the list of files in ``Directories`` for which ``Filter`` (when
   --  called on the file base name) returns True.

   procedure Add_Entry
     (Provider       : in out Auto_Unit_Provider;
      Filename       : Unbounded_String;
      CU             : Compilation_Unit;
      PLE_Root_Index : Positive);
   --  Add a CU -> Filename entry to Provider.Mapping

   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
     (Provider       : in out Auto_Unit_Provider;
      Filename       : Unbounded_String;
      CU             : Compilation_Unit;
      PLE_Root_Index : Positive)
   is
      use Ada.Strings.Wide_Wide_Unbounded;

      FQN  : constant Unbounded_Text_Type_Array :=
        CU.P_Syntactic_Fully_Qualified_Name;
      Kind : constant Analysis_Unit_Kind := CU.P_Unit_Kind;
      Name : Unbounded_Wide_Wide_String;
   begin
      for I in FQN'Range loop
         if I > FQN'First then
            Append (Name, '.');
         end if;
         Append (Name, To_Text (FQN (I)));
      end loop;

      declare
         Key       : constant Symbol_Type :=
           As_Key (To_Wide_Wide_String (Name), Kind, Provider);
         Value     : constant Filename_And_PLE_Root :=
           (Filename, PLE_Root_Index);
         Dummy_Cur : Unit_Maps.Cursor;
         Inserted  : Boolean;
      begin
         Provider.Mapping.Insert (Key, Value, Dummy_Cur, Inserted);

         --  TODO??? Somehow report duplicate entries
         pragma Unreferenced (Inserted);
      end;
   end Add_Entry;

   ----------------
   -- Find_Files --
   ----------------

   function Find_Files
     (Filter       : not null access function (Name : String) return Boolean;
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
               if Filter.all (+F.Base_Name) then
                  Result.Append (F);
               end if;
            end loop;
            Unchecked_Free (Files);
         end;
      end loop;

      return R : constant File_Array_Access :=
         new File_Array (1 .. Natural (Result.Length))
      do
         for Cur in Result.Iterate loop
            R (To_Index (Cur)) := Element (Cur);
         end loop;
      end return;
   end Find_Files;

   ----------------
   -- Find_Files --
   ----------------

   function Find_Files
     (Name_Pattern : GNAT.Regpat.Pattern_Matcher :=
        Default_Source_Filename_Pattern;
      Directories  : GNATCOLL.VFS.File_Array)
      return GNATCOLL.VFS.File_Array_Access
   is
      function Filter (Name : String) return Boolean
      is (GNAT.Regpat.Match (Name_Pattern, Name));
   begin
      return Find_Files (Filter'Access, Directories);
   end Find_Files;

   -----------------------
   -- Find_Files_Regexp --
   -----------------------

   function Find_Files_Regexp
     (Name_Pattern : GNAT.Regexp.Regexp := Default_Source_Filename_Regexp;
      Directories  : GNATCOLL.VFS.File_Array)
      return GNATCOLL.VFS.File_Array_Access
   is
      function Filter (Name : String) return Boolean
      is (GNAT.Regexp.Match (Name, Name_Pattern));
   begin
      return Find_Files (Filter'Access, Directories);
   end Find_Files_Regexp;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Auto_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is
   begin
      --  Get_Unit_Location is supposed to handle all cases, so this should be
      --  dead code.

      pragma Unreferenced (Provider, Name, Kind);
      return (raise Program_Error);
   end Get_Unit_Filename;

   -----------------------
   -- Get_Unit_Location --
   -----------------------

   overriding procedure Get_Unit_Location
     (Provider       : Auto_Unit_Provider;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : in out Unbounded_String;
      PLE_Root_Index : in out Natural)
   is
      use Unit_Maps;

      Mapping : Map renames Provider.Mapping;
      Cur     : constant Cursor :=
        Mapping.Find (As_Key (Name, Kind, Provider));
   begin
      if Has_Element (Cur) then
         declare
            Value : Filename_And_PLE_Root renames
              Mapping.Constant_Reference (Cur);
         begin
            Filename := Value.Filename;
            PLE_Root_Index := Value.PLE_Root_Index;
         end;
      else
         Filename := Null_Unbounded_String;
         PLE_Root_Index := 1;
      end if;
   end Get_Unit_Location;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Auto_Unit_Provider;
      Context     : Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit'Class
   is
      --  Get_Unit_And_PLE_Root is supposed to handle all cases, so this should
      --  be dead code.

      pragma Unreferenced (Provider, Context, Name, Kind, Charset, Reparse);
   begin
      return (raise Program_Error);
   end Get_Unit;

   ---------------------------
   -- Get_Unit_And_PLE_Root --
   ---------------------------

   overriding procedure Get_Unit_And_PLE_Root
     (Provider       : Auto_Unit_Provider;
      Context        : Analysis_Context'Class;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Charset        : String := "";
      Reparse        : Boolean := False;
      Unit           : in out Analysis_Unit'Class;
      PLE_Root_Index : in out Natural)
   is
      Filename : Unbounded_String;
   begin
      Provider.Get_Unit_Location (Name, Kind, Filename, PLE_Root_Index);
      pragma Assert (PLE_Root_Index > 0);

      if Length (Filename) > 0 then
         Unit :=
           Analysis_Unit'Class
             (Context.Get_From_File (To_String (Filename), Charset, Reparse));
      else
         declare
            Dummy_File : constant String :=
               Libadalang.Unit_Files.File_From_Unit (Name, Kind);
            Kind_Name  : constant Text_Type :=
              (case Kind is
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant Text_Type :=
               "Could not find source file for " & Name & " (" & Kind_Name
               & ")";
         begin
            Unit := Analysis_Unit'Class
              (Context.Get_With_Error (Dummy_File, Error, Charset));
         end;
      end if;
   end Get_Unit_And_PLE_Root;

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

      for Filename_VFS of Input_Files loop
         declare
            Filename_String : constant String := +Filename_VFS.Full_Name;
            Filename_Unb    : constant Unbounded_String :=
              To_Unbounded_String (Filename_String);

            Unit : constant Analysis_Unit :=
              Get_From_File (Context, Filename_String, Reparse => True);
            R    : constant Ada_Node := Root (Unit);
         begin
            if not Has_Diagnostics (Unit) then

               --  If parsing went fine, add the compilation units File
               --  contains to our internal mapping.
               --
               --  TODO??? Somehow report parsing errors.

               case Unit_Files.Root_Nodes (R.Kind) is
                  when Ada_Compilation_Unit =>
                     Add_Entry
                       (Provider, Filename_Unb, R.As_Compilation_Unit, 1);
                  when Ada_Compilation_Unit_List =>
                     for I in 1 .. R.Children_Count loop
                        Add_Entry
                          (Provider,
                           Filename_Unb,
                           R.Child (I).As_Compilation_Unit,
                           I);
                     end loop;

                  when Ada_Pragma_Node_List =>

                     --  This could be a configuration pragma file, or a body
                     --  that contains just "pragma No_Body;". In any case,
                     --  there is no entry to register here.

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
         Provider.Keys := Create_Symbol_Table;
         Create_Auto_Provider (Provider, Input_Files, Charset);
      end return;
   end Create_Auto_Provider;

   ------------
   -- As_Key --
   ------------

   function As_Key
     (Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Provider : Auto_Unit_Provider) return Symbol_Type
   is
      Canon_Name  : constant Text_Type :=
         Ada.Wide_Wide_Characters.Handling.To_Lower (Name);
      Kind_Letter : constant Wide_Wide_Character :=
        (case Kind is
         when Unit_Specification => 's',
         when Unit_Body          => 'b');
   begin
      return Get_Symbol
        (Provider.Keys, Find (Provider.Keys, Kind_Letter & ':' & Canon_Name));
   end As_Key;

end Libadalang.Auto_Provider;
