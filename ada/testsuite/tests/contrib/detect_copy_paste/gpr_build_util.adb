------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Case_Util;             use GNAT.Case_Util;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.HTable;
with GNAT.Regexp;                use GNAT.Regexp;

with GPR.Opt;     use GPR.Opt;
with GPR.Osint;   use GPR.Osint;
with GPR.Com;
with GPR.Debug;
with GPR.Err;     use GPR.Err;
with GPR.Erroutc; use GPR.Erroutc;
with GPR.Ext;
with GPR.Names;   use GPR.Names;
with GPR.Output;  use GPR.Output;
with GPR.Tempdir;
with GPR.Util;    use GPR.Util;

package body Gpr_Build_Util is

   use Stamps;

   use ALI;

   function File_Not_A_Source_Of
     (Project_Tree : Project_Tree_Ref;
      Uname        : Name_Id;
      Sfile        : File_Name_Type) return Boolean;
   --  Check that file name Sfile is one of the source of unit Uname. Returns
   --  True if the unit is in one of the project file, but the file name is not
   --  one of its source. Returns False otherwise.

   procedure Verbose_Msg
     (N1                : Name_Id;
      S1                : String;
      N2                : Name_Id := No_Name;
      S2                : String  := "";
      Prefix            : String  := "  -> ";
      Minimum_Verbosity : Opt.Verbosity_Level_Type := Opt.Low);
   --  If the verbose flag (Verbose_Mode) is set and the verbosity level is at
   --  least equal to Minimum_Verbosity, then print Prefix to standard output
   --  followed by N1 and S1. If N2 /= No_Name then N2 is printed after S1. S2
   --  is printed last. Both N1 and N2 are printed in quotation marks. The two
   --  forms differ only in taking Name_Id or File_name_Type arguments.

   ---------
   -- Add --
   ---------

   procedure Add
     (Option : String_Access;
      To     : in out String_List_Access;
      Last   : in out Natural)
   is
   begin
      if Last = To'Last then
         declare
            New_Options : constant String_List_Access :=
                            new String_List (1 .. To'Last * 2);

         begin
            New_Options (To'Range) := To.all;

            --  Set all elements of the original options to null to avoid
            --  deallocation of copies.

            To.all := (others => null);

            Free (To);
            To := New_Options;
         end;
      end if;

      Last := Last + 1;
      To (Last) := Option;
   end Add;

   procedure Add
     (Option : String;
      To     : in out String_List_Access;
      Last   : in out Natural)
   is
   begin
      Add (Option => new String'(Option), To => To, Last => Last);
   end Add;

   ----------------------------
   -- Aggregate_Libraries_In --
   ----------------------------

   function Aggregate_Libraries_In (Tree : Project_Tree_Ref) return Boolean is
      List : Project_List;

   begin
      List := Tree.Projects;
      while List /= null loop
         if List.Project.Qualifier = Aggregate_Library then
            return True;
         end if;

         List := List.Next;
      end loop;

      return False;
   end Aggregate_Libraries_In;

   -------------------------
   -- Base_Name_Index_For --
   -------------------------

   function Base_Name_Index_For
     (Main            : String;
      Main_Index      : Int;
      Index_Separator : Character) return File_Name_Type
   is
      Result : File_Name_Type;

   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Base_Name (Main));

      --  Remove the extension, if any, that is the last part of the base name
      --  starting with a dot and following some characters.

      for J in reverse 2 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Name_Len := J - 1;
            exit;
         end if;
      end loop;

      --  Add the index info, if index is different from 0

      if Main_Index > 0 then
         Add_Char_To_Name_Buffer (Index_Separator);

         declare
            Img : constant String := Main_Index'Img;
         begin
            Add_Str_To_Name_Buffer (Img (2 .. Img'Last));
         end;
      end if;

      Result := Name_Find;
      return Result;
   end Base_Name_Index_For;

   ------------------------------
   -- Check_Source_Info_In_ALI --
   ------------------------------

   function Check_Source_Info_In_ALI
     (The_ALI : ALI.ALI_Id;
      Tree    : Project_Tree_Ref) return Name_Id
   is
      Result    : Name_Id := No_Name;
      Unit_Name : Name_Id;

   begin
      --  Loop through units

      for U in ALIs.Table (The_ALI).First_Unit ..
               ALIs.Table (The_ALI).Last_Unit
      loop
         --  Check if the file name is one of the source of the unit

         Get_Name_String (Units.Table (U).Uname);
         Name_Len  := Name_Len - 2;
         Unit_Name := Name_Find;

         if File_Not_A_Source_Of (Tree, Unit_Name, Units.Table (U).Sfile) then
            return No_Name;
         end if;

         if Result = No_Name then
            Result := Unit_Name;
         end if;

         --  Loop to do same check for each of the withed units

         for W in Units.Table (U).First_With .. Units.Table (U).Last_With loop
            declare
               WR : ALI.With_Record renames Withs.Table (W);

            begin
               if WR.Sfile /= No_File then
                  Get_Name_String (WR.Uname);
                  Name_Len  := Name_Len - 2;
                  Unit_Name := Name_Find;

                  if File_Not_A_Source_Of (Tree, Unit_Name, WR.Sfile) then
                     return No_Name;
                  end if;
               end if;
            end;
         end loop;
      end loop;

      --  Loop to check subunits and replaced sources

      for D in ALIs.Table (The_ALI).First_Sdep ..
               ALIs.Table (The_ALI).Last_Sdep
      loop
         declare
            SD : Sdep_Record renames Sdep.Table (D);

         begin
            Unit_Name := SD.Subunit_Name;

            if Unit_Name = No_Name then

               --  Check if this source file has been replaced by a source with
               --  a different file name.

               if Tree /= null and then Tree.Replaced_Source_Number > 0 then
                  declare
                     Replacement : constant File_Name_Type :=
                       Replaced_Source_HTable.Get
                         (Tree.Replaced_Sources, SD.Sfile);

                  begin
                     if Replacement /= No_File then
                        if Opt.Verbosity_Level > Opt.Low
                        then
                           Put_Line
                             ("source file"
                              & Get_Name_String (SD.Sfile)
                              & " has been replaced by "
                              & Get_Name_String (Replacement));
                        end if;

                        return No_Name;
                     end if;
                  end;
               end if;

               --  Check that a dependent source for a unit that is from a
               --  project is indeed a source of this unit.

               Unit_Name := SD.Unit_Name;

               if Unit_Name /= No_Name
                 --  and then not Fname.Is_Internal_File_Name (SD.Sfile)
                 and then File_Not_A_Source_Of (Tree, Unit_Name, SD.Sfile)
               then
                  return No_Name;
               end if;

            else
               --  For separates, the file is no longer associated with the
               --  unit ("proc-sep.adb" is not associated with unit "proc.sep")
               --  so we need to check whether the source file still exists in
               --  the source tree: it will if it matches the naming scheme
               --  (and then will be for the same unit).

               if GPR.Find_Source
                    (In_Tree   => Tree,
                     Project   => No_Project,
                     Base_Name => SD.Sfile) = No_Source
               then
                  Get_Name_String (SD.Sfile);

                  if Name_Len < 3
                    or else Name_Buffer (2) /= '-'
                    or else
                      (Name_Buffer (1) /= 'a'
                       and then
                       Name_Buffer (1) /= 'g'
                       and then
                       Name_Buffer (1) /= 'i'
                       and then
                       Name_Buffer (1) /= 's')
                  then
                     if Opt.Verbosity_Level > Opt.Low
                     then
                        Put_Line
                          ("While parsing ALI file, file "
                           & Get_Name_String (SD.Sfile)
                           & " is indicated as containing subunit "
                           & Get_Name_String (Unit_Name)
                           & " but this does not match what was found while"
                           & " parsing the project. Will recompile");
                     end if;

                     return No_Name;
                  end if;
               end if;
            end if;
         end;
      end loop;

      return Result;
   end Check_Source_Info_In_ALI;

   --------------------------------
   -- Create_Binder_Mapping_File --
   --------------------------------

   function Create_Binder_Mapping_File
     (Project_Tree : Project_Tree_Ref) return Path_Name_Type
   is
      Mapping_Path : Path_Name_Type := No_Path;

      Mapping_FD : File_Descriptor := Invalid_FD;
      --  A File Descriptor for an eventual mapping file

      ALI_Unit : Unit_Name_Type := No_Unit_Name;
      --  The unit name of an ALI file

      ALI_Name : File_Name_Type := No_File;
      --  The file name of the ALI file

      ALI_Project : Project_Id := No_Project;
      --  The project of the ALI file

      Bytes : Integer;
      OK    : Boolean := False;
      Unit  : Unit_Index;

      Status : Boolean;
      --  For call to Close

      Iter : Source_Iterator := For_Each_Source
                                  (In_Tree           => Project_Tree,
                                   Language          => Name_Ada,
                                   Encapsulated_Libs => False,
                                   Locally_Removed   => False);

      Source : GPR.Source_Id;

   begin
      Tempdir.Create_Temp_File (Mapping_FD, Mapping_Path);
      Record_Temp_File (Project_Tree.Shared, Mapping_Path);

      if Mapping_FD /= Invalid_FD then
         OK := True;

         loop
            Source := Element (Iter);
            exit when Source = No_Source;

            Unit := Source.Unit;

            if Source.Replaced_By /= No_Source
              or else Unit = No_Unit_Index
              or else Unit.Name = No_Name
            then
               ALI_Name := No_File;

            --  If this is a body, put it in the mapping

            elsif Source.Kind = Impl
              and then Unit.File_Names (Impl) /= No_Source
              and then Unit.File_Names (Impl).Project /= No_Project
            then
               Get_Name_String (Unit.Name);
               Add_Str_To_Name_Buffer ("%b");
               ALI_Unit := Name_Find;
               ALI_Name :=
                 Lib_File_Name (Unit.File_Names (Impl).Display_File);
               ALI_Project := Unit.File_Names (Impl).Project;

            --  Otherwise, if this is a spec and there is no body, put it in
            --  the mapping.

            elsif Source.Kind = Spec
              and then Unit.File_Names (Impl) = No_Source
              and then Unit.File_Names (Spec) /= No_Source
              and then Unit.File_Names (Spec).Project /= No_Project
            then
               Get_Name_String (Unit.Name);
               Add_Str_To_Name_Buffer ("%s");
               ALI_Unit := Name_Find;
               ALI_Name :=
                 Lib_File_Name (Unit.File_Names (Spec).Display_File);
               ALI_Project := Unit.File_Names (Spec).Project;

            else
               ALI_Name := No_File;
            end if;

            --  If we have something to put in the mapping then do it now. If
            --  the project is extended, look for the ALI file in the project,
            --  then in the extending projects in order, and use the last one
            --  found.

            if ALI_Name /= No_File then

               --  Look in the project and the projects that are extending it
               --  to find the real ALI file.

               declare
                  ALI      : constant String := Get_Name_String (ALI_Name);
                  ALI_Path : Name_Id         := No_Name;

               begin
                  loop
                     --  For library projects, use the library ALI directory,
                     --  for other projects, use the object directory.

                     if ALI_Project.Library then
                        Get_Name_String
                          (ALI_Project.Library_ALI_Dir.Display_Name);
                     else
                        Get_Name_String
                          (ALI_Project.Object_Directory.Display_Name);
                     end if;

                     Add_Str_To_Name_Buffer (ALI);

                     if Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
                        ALI_Path := Name_Find;
                     end if;

                     ALI_Project := ALI_Project.Extended_By;
                     exit when ALI_Project = No_Project;
                  end loop;

                  if ALI_Path /= No_Name then

                     --  First line is the unit name

                     Get_Name_String (ALI_Unit);
                     Add_Char_To_Name_Buffer (ASCII.LF);
                     Bytes :=
                       Write
                         (Mapping_FD,
                          Name_Buffer (1)'Address,
                          Name_Len);
                     OK := Bytes = Name_Len;

                     exit when not OK;

                     --  Second line is the ALI file name

                     Get_Name_String (ALI_Name);
                     Add_Char_To_Name_Buffer (ASCII.LF);
                     Bytes :=
                       Write
                         (Mapping_FD,
                          Name_Buffer (1)'Address,
                          Name_Len);
                     OK := (Bytes = Name_Len);

                     exit when not OK;

                     --  Third line is the ALI path name

                     Get_Name_String (ALI_Path);
                     Add_Char_To_Name_Buffer (ASCII.LF);
                     Bytes :=
                       Write
                         (Mapping_FD,
                          Name_Buffer (1)'Address,
                          Name_Len);
                     OK := (Bytes = Name_Len);

                     --  If OK is False, it means we were unable to write a
                     --  line. No point in continuing with the other units.

                     exit when not OK;
                  end if;
               end;
            end if;

            Next (Iter);
         end loop;

         Close (Mapping_FD, Status);

         OK := OK and Status;
      end if;

      --  If the creation of the mapping file was successful, we add the switch
      --  to the arguments of gnatbind.

      if OK then
         return Mapping_Path;

      else
         return No_Path;
      end if;
   end Create_Binder_Mapping_File;

   --------------------------
   -- File_Not_A_Source_Of --
   --------------------------

   function File_Not_A_Source_Of
     (Project_Tree : Project_Tree_Ref;
      Uname        : Name_Id;
      Sfile        : File_Name_Type) return Boolean
   is
      Unit : constant Unit_Index :=
               Units_Htable.Get (Project_Tree.Units_HT, Uname);

      At_Least_One_File : Boolean := False;

   begin
      if Unit /= No_Unit_Index then
         for F in Unit.File_Names'Range loop
            if Unit.File_Names (F) /= null then
               At_Least_One_File := True;
               if Unit.File_Names (F).File = Sfile then
                  return False;
               end if;
            end if;
         end loop;

         if not At_Least_One_File then

            --  The unit was probably created initially for a separate unit
            --  (which are initially created as IMPL when both suffixes are the
            --  same). Later on, Override_Kind changed the type of the file,
            --  and the unit is no longer valid in fact.

            return False;
         end if;

         Verbose_Msg (Uname, "sources do not include ", Name_Id (Sfile));
         return True;
      end if;

      return False;
   end File_Not_A_Source_Of;

   ---------------------
   -- Get_Directories --
   ---------------------

   procedure Get_Directories
     (Project_Tree : Project_Tree_Ref;
      For_Project  : Project_Id;
      Activity     : Activity_Type;
      Languages    : Name_Ids)
   is

      procedure Recursive_Add
        (Project  : Project_Id;
         Tree     : Project_Tree_Ref;
         Extended : in out Boolean);
      --  Add all the source directories of a project to the path only if
      --  this project has not been visited. Calls itself recursively for
      --  projects being extended, and imported projects.

      procedure Add_Dir (Value : Path_Name_Type);
      --  Add directory Value in table Directories, if it is defined and not
      --  already there.

      -------------
      -- Add_Dir --
      -------------

      procedure Add_Dir (Value : Path_Name_Type) is
         Add_It : Boolean := True;

      begin
         if Value /= No_Path
           and then Is_Directory (Get_Name_String (Value))
         then
            for Index in 1 .. Directories.Last loop
               if Directories.Table (Index) = Value then
                  Add_It := False;
                  exit;
               end if;
            end loop;

            if Add_It then
               Directories.Increment_Last;
               Directories.Table (Directories.Last) := Value;
            end if;
         end if;
      end Add_Dir;

      -------------------
      -- Recursive_Add --
      -------------------

      procedure Recursive_Add
        (Project  : Project_Id;
         Tree     : Project_Tree_Ref;
         Extended : in out Boolean)
      is
         Current   : String_List_Id;
         Dir       : String_Element;
         OK        : Boolean := False;
         Lang_Proc : Language_Ptr := Project.Languages;

      begin
         --  Add to path all directories of this project

         if Activity = Compilation then
            Lang_Loop :
            while Lang_Proc /= No_Language_Index loop
               for J in Languages'Range loop
                  OK := Lang_Proc.Name = Languages (J);
                  exit Lang_Loop when OK;
               end loop;

               Lang_Proc := Lang_Proc.Next;
            end loop Lang_Loop;

            if OK then
               Current := Project.Source_Dirs;

               while Current /= Nil_String loop
                  Dir := Tree.Shared.String_Elements.Table (Current);
                  Add_Dir (Path_Name_Type (Dir.Value));
                  Current := Dir.Next;
               end loop;
            end if;

         elsif Project.Library then
            if Activity = SAL_Binding and then Extended then
               Add_Dir (Project.Object_Directory.Display_Name);

            else
               Add_Dir (Project.Library_ALI_Dir.Display_Name);
            end if;

         else
            Add_Dir (Project.Object_Directory.Display_Name);
         end if;

         if Project.Extends = No_Project then
            Extended := False;
         end if;
      end Recursive_Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Recursive_Add);

      Extended : Boolean := True;

      --  Start of processing for Get_Directories

   begin
      Directories.Init;
      For_All_Projects (For_Project, Project_Tree, Extended);
   end Get_Directories;

   ------------
   -- Inform --
   ------------

   procedure Inform (N : File_Name_Type; Msg : String) is
   begin
      Inform (Name_Id (N), Msg);
   end Inform;

   procedure Inform (N : Name_Id := No_Name; Msg : String) is
   begin
      Write_Program_Name;

      if N /= No_Name then
         Write_Char ('"');

         declare
            Name : constant String := Get_Name_String (N);
         begin
            if Debug.Debug_Flag_F and then Is_Absolute_Path (Name) then
               Write_Str (File_Name (Name));
            else
               Write_Str (Name);
            end if;
         end;

         Write_Str (""" ");
      end if;

      Write_Line (Msg);
   end Inform;

   ----------------------------
   -- Is_External_Assignment --
   ----------------------------

   function Is_External_Assignment
     (Env  : GPR.Tree.Environment;
      Argv : String) return Boolean
   is
      Start  : Positive := 3;
      Finish : Natural := Argv'Last;

      pragma Assert (Argv'First = 1);
      pragma Assert (Argv (1 .. 2) = "-X");

   begin
      if Argv'Last < 5 then
         return False;

      elsif Argv (3) = '"' then
         if Argv (Argv'Last) /= '"' or else Argv'Last < 7 then
            return False;
         else
            Start := 4;
            Finish := Argv'Last - 1;
         end if;
      end if;

      return GPR.Ext.Check
        (Self        => Env.External,
         Declaration => Argv (Start .. Finish));
   end Is_External_Assignment;

   -------------------
   -- Lib_File_Name --
   -------------------

   function Lib_File_Name
     (Source_File : File_Name_Type;
      Munit_Index : Nat := 0) return File_Name_Type
   is
   begin
      Get_Name_String (Source_File);

      for J in reverse 2 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Name_Len := J - 1;
            exit;
         end if;
      end loop;

      if Munit_Index /= 0 then
         Add_Char_To_Name_Buffer (Multi_Unit_Index_Character);
         Add_Nat_To_Name_Buffer (Munit_Index);
      end if;

      Add_Str_To_Name_Buffer (".ali");
      return Name_Find;
   end Lib_File_Name;

   -----------
   -- Mains --
   -----------

   package body Mains is

      package Names is new GNAT.Table
        (Table_Component_Type => Main_Info,
         Table_Index_Type     => Integer,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100);
      --  The table that stores the mains

      Current : Natural := 0;
      --  The index of the last main retrieved from the table

      Count_Of_Mains_With_No_Tree : Natural := 0;
      --  Number of main units for which we do not know the project tree

      --------------
      -- Add_Main --
      --------------

      procedure Add_Main
        (Name     : String;
         Index    : Int := 0;
         Location : Source_Ptr := No_Location;
         Project  : Project_Id := No_Project;
         Tree     : Project_Tree_Ref := null)
      is
         Canonical_Name : File_Name_Type;

      begin
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Name);
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
         Canonical_Name := Name_Find;

         --  Check if this main is already in table Names. If it is, do not
         --  put it again, to avoid binding and linking the same main several
         --  times in parallel when -jnn is used, as this does not work on all
         --  platforms.

         for J in 1 .. Names.Last loop
            if Canonical_Name = Names.Table (J).File
              and then Index = Names.Table (J).Index
              and then Project = Names.Table (J).Project
            then
               return;
            end if;
         end loop;

         if Current_Verbosity = High then
            Debug_Output ("Add_Main """ & Name & """ " & Index'Img
                          & " with_tree? "
                          & Boolean'Image (Tree /= null));
         end if;

         Name_Len := 0;
         Add_Str_To_Name_Buffer (Name);
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

         Names.Increment_Last;
         Names.Table (Names.Last) :=
           (Name_Find, Index, Location, No_Source, Project, Tree);

         if Tree /= null then
            Builder_Data (Tree).Number_Of_Mains :=
              Builder_Data (Tree).Number_Of_Mains + 1;

         else
            Mains.Count_Of_Mains_With_No_Tree :=
              Mains.Count_Of_Mains_With_No_Tree + 1;
         end if;
      end Add_Main;

      --------------------
      -- Complete_Mains --
      --------------------

      procedure Complete_Mains
        (Flags          : Processing_Flags;
         Root_Project   : Project_Id;
         Project_Tree   : Project_Tree_Ref;
         Unique_Compile : Boolean := False)
      is
         procedure Do_Complete (Project : Project_Id; Tree : Project_Tree_Ref);
         --  Check the mains for this specific project

         procedure Complete_All is new For_Project_And_Aggregated
           (Do_Complete);

         procedure Add_Multi_Unit_Sources
           (Tree   : Project_Tree_Ref;
            Source : GPR.Source_Id);
         --  Add all units from the same file as the multi-unit Source

         function Find_File_Add_Extension
           (Tree      : Project_Tree_Ref;
            Base_Main : String) return GPR.Source_Id;
         --  Search for Main in the project, adding body or spec extensions

         ----------------------------
         -- Add_Multi_Unit_Sources --
         ----------------------------

         procedure Add_Multi_Unit_Sources
           (Tree   : Project_Tree_Ref;
            Source : GPR.Source_Id)
         is
            Iter : Source_Iterator;
            Src  : GPR.Source_Id;

         begin
            Debug_Output
              ("found multi-unit source file in project", Source.Project.Name);

            Iter := For_Each_Source
              (In_Tree => Tree, Project => Source.Project);

            while Element (Iter) /= No_Source loop
               Src := Element (Iter);

               if Src.File = Source.File
                 and then Src.Index /= Source.Index
               then
                  if Src.File = Source.File then
                     Debug_Output
                       ("add main in project, index=" & Src.Index'Img);
                  end if;

                  Names.Increment_Last;
                  Names.Table (Names.Last) :=
                    (File     => Src.File,
                     Index    => Src.Index,
                     Location => No_Location,
                     Source   => Src,
                     Project  => Src.Project,
                     Tree     => Tree);

                  Builder_Data (Tree).Number_Of_Mains :=
                    Builder_Data (Tree).Number_Of_Mains + 1;
               end if;

               Next (Iter);
            end loop;
         end Add_Multi_Unit_Sources;

         -----------------------------
         -- Find_File_Add_Extension --
         -----------------------------

         function Find_File_Add_Extension
           (Tree      : Project_Tree_Ref;
            Base_Main : String) return GPR.Source_Id
         is
            Spec_Source : GPR.Source_Id := No_Source;
            Source      : GPR.Source_Id;
            Iter        : Source_Iterator;
            Suffix      : File_Name_Type;

         begin
            Source := No_Source;
            Iter := For_Each_Source (Tree);  --  In all projects
            loop
               Source := GPR.Element (Iter);
               exit when Source = No_Source;

               if Source.Kind = Impl then
                  Get_Name_String (Source.File);

                  if Name_Len > Base_Main'Length
                    and then Name_Buffer (1 .. Base_Main'Length) = Base_Main
                  then
                     Suffix :=
                       Source.Language.Config.Naming_Data.Body_Suffix;

                     if Suffix /= No_File then
                        declare
                           Suffix_Str : String := Get_Name_String (Suffix);
                        begin
                           Canonical_Case_File_Name (Suffix_Str);
                           exit when
                             Name_Buffer (Base_Main'Length + 1 .. Name_Len) =
                             Suffix_Str;
                        end;
                     end if;
                  end if;

               elsif Source.Kind = Spec
                 and then Source.Language.Config.Kind = Unit_Based
               then
                  --  An Ada spec needs to be taken into account unless there
                  --  is also a body. So we delay the decision for them.

                  Get_Name_String (Source.File);

                  if Name_Len > Base_Main'Length
                    and then Name_Buffer (1 .. Base_Main'Length) = Base_Main
                  then
                     Suffix := Source.Language.Config.Naming_Data.Spec_Suffix;

                     if Suffix /= No_File then
                        declare
                           Suffix_Str : String := Get_Name_String (Suffix);

                        begin
                           Canonical_Case_File_Name (Suffix_Str);

                           if Name_Buffer (Base_Main'Length + 1 .. Name_Len) =
                             Suffix_Str
                           then
                              Spec_Source := Source;
                           end if;
                        end;
                     end if;
                  end if;
               end if;

               Next (Iter);
            end loop;

            if Source = No_Source then
               Source := Spec_Source;
            end if;

            return Source;
         end Find_File_Add_Extension;

         -----------------
         -- Do_Complete --
         -----------------

         procedure Do_Complete
           (Project : Project_Id; Tree : Project_Tree_Ref)
         is
            J : Integer;

         begin
            if Mains.Number_Of_Mains (Tree) > 0
              or else Mains.Count_Of_Mains_With_No_Tree > 0
            then
               --  Traverse in reverse order, since in the case of multi-unit
               --  files we will be adding extra files at the end, and there's
               --  no need to process them in turn.

               J := Names.Last;
               Main_Loop : loop
                  declare
                     File        : Main_Info       := Names.Table (J);
                     Main_Id     : File_Name_Type  := File.File;
                     Main        : constant String :=
                                     Get_Name_String (Main_Id);
                     Base        : constant String := Base_Name (Main);
                     Source      : GPR.Source_Id   := No_Source;
                     Is_Absolute : Boolean         := False;

                  begin
                     if Base /= Main then
                        Is_Absolute := True;

                        if Is_Absolute_Path (Main) then
                           Main_Id := Create_Name (Base);

                        --  Not an absolute path

                        else
                           --  Always resolve links here, so that users can be
                           --  specify any name on the command line. If the
                           --  project itself uses links, the user will be
                           --  using -eL anyway, and thus files are also stored
                           --  with resolved names.

                           declare
                              Absolute : constant String :=
                                           Normalize_Pathname
                                             (Name           => Main,
                                              Directory      => "",
                                              Resolve_Links  => True,
                                              Case_Sensitive => False);
                           begin
                              File.File := Create_Name (Absolute);
                              Main_Id := Create_Name (Base);
                           end;
                        end if;
                     end if;

                     --  If no project or tree was specified for the main, it
                     --  came from the command line.
                     --  Note that the assignments below will not modify inside
                     --  the table itself.

                     if File.Project = null then
                        File.Project := Project;
                     end if;

                     if File.Tree = null then
                        File.Tree := Tree;
                     end if;

                     if File.Source = null then
                        if Current_Verbosity = High then
                           Debug_Output
                             ("search for main """ & Main
                              & '"' & File.Index'Img & " in "
                              & Get_Name_String (Debug_Name (File.Tree))
                              & ", project", Project.Name);
                        end if;

                        --  First, look for the main as specified. We need to
                        --  search for the base name though, and if needed
                        --  check later that we found the correct file.

                        declare
                           Sources : constant Source_Ids :=
                                       Find_All_Sources
                                         (In_Tree          => File.Tree,
                                          Project          => File.Project,
                                          Base_Name        => Main_Id,
                                          Index            => File.Index,
                                          In_Imported_Only => True);

                        begin
                           if Is_Absolute then
                              for J in Sources'Range loop
                                 if File_Name_Type (Sources (J).Path.Name) =
                                                                    File.File
                                 then
                                    Source := Sources (J);
                                    exit;
                                 end if;
                              end loop;

                           elsif Sources'Length > 1 then

                              --  This is only allowed if the units are from
                              --  the same multi-unit source file.

                              Source := Sources (1);

                              for J in 2 .. Sources'Last loop
                                 if Sources (J).Path /= Source.Path
                                   or else Sources (J).Index = Source.Index
                                 then
                                    Error_Msg_File_1 := Main_Id;
                                    GPR.Err.Error_Msg
                                      (Flags, "several main sources {",
                                       No_Location, File.Project);
                                    exit Main_Loop;
                                 end if;
                              end loop;

                           elsif Sources'Length = 1 then
                              Source := Sources (Sources'First);
                           end if;
                        end;

                        if Source = No_Source then
                           Source := Find_File_Add_Extension
                                       (File.Tree, Get_Name_String (Main_Id));
                        end if;

                        if Is_Absolute
                          and then Source /= No_Source
                          and then
                            File_Name_Type (Source.Path.Name) /= File.File
                        then
                           Debug_Output
                             ("Found a non-matching file",
                              Name_Id (Source.Path.Display_Name));
                           Source := No_Source;
                        end if;

                        if Source /= No_Source then
                           if not Is_Allowed_Language
                                    (Source.Language.Name)
                           then
                              --  Remove any main that is not in the list of
                              --  restricted languages.

                              Names.Table (J .. Names.Last - 1) :=
                                Names.Table (J + 1 .. Names.Last);
                              Names.Set_Last (Names.Last - 1);

                           else
                              --  If we have found a multi-unit source file but
                              --  did not specify an index initially, we'll
                              --  need to compile all the units from the same
                              --  source file.

                              if Source.Index /= 0 and then File.Index = 0 then
                                 Add_Multi_Unit_Sources (File.Tree, Source);
                              end if;

                              --  A main cannot be a source of a library
                              --  project.

                              if (not Opt.Compile_Only or else Opt.Bind_Only)
                                and then not Unique_Compile
                                and then Source.Project.Library
                              then
                                 Error_Msg_File_1 := Main_Id;
                                 GPR.Err.Error_Msg
                                   (Flags,
                                    "main cannot be a source" &
                                      " of a library project: {",
                                    No_Location, File.Project);

                              else
                                 --  Now update the original Main, otherwise it
                                 --  will be reported as not found.

                                 Debug_Output
                                   ("found main in project",
                                    Source.Project.Name);
                                 Names.Table (J).File    := Source.File;
                                 Names.Table (J).Project := Source.Project;

                                 if Names.Table (J).Tree = null then
                                    Names.Table (J).Tree := File.Tree;

                                    Builder_Data (File.Tree).Number_Of_Mains :=
                                      Builder_Data (File.Tree).Number_Of_Mains
                                      + 1;
                                    Mains.Count_Of_Mains_With_No_Tree :=
                                      Mains.Count_Of_Mains_With_No_Tree - 1;
                                 end if;

                                 Names.Table (J).Source  := Source;
                                 Names.Table (J).Index   := Source.Index;
                              end if;
                           end if;

                        elsif File.Location /= No_Location then

                           --  If the main is declared in package Builder of
                           --  the main project, report an error. If the main
                           --  is on the command line, it may be a main from
                           --  another project, so do nothing: if the main does
                           --  not exist in another project, an error will be
                           --  reported later.

                           Error_Msg_File_1 := Main_Id;
                           Error_Msg_Name_1 := File.Project.Name;
                           GPR.Err.Error_Msg
                             (Flags, "{ is not a source of project %%",
                              File.Location, File.Project);
                        end if;
                     end if;
                  end;

                  J := J - 1;
                  exit Main_Loop when J < Names.First;
               end loop Main_Loop;
            end if;

            if Total_Errors_Detected > 0 then
               Fail_Program (Tree, "problems with main sources");
            end if;
         end Do_Complete;

      --  Start of processing for Complete_Mains

      begin
         Complete_All (Root_Project, Project_Tree);

         if Mains.Count_Of_Mains_With_No_Tree > 0 then
            for J in Names.First .. Names.Last loop
               if Names.Table (J).Source = No_Source then
                  Fail_Program
                    (Project_Tree, '"' & Get_Name_String (Names.Table (J).File)
                     & """ is not a source of any project");
               end if;
            end loop;
         end if;
      end Complete_Mains;

      ------------
      -- Delete --
      ------------

      procedure Delete is
      begin
         Names.Set_Last (0);
         Mains.Reset;
      end Delete;

      -----------------------
      -- Fill_From_Project --
      -----------------------

      procedure Fill_From_Project
        (Root_Project : Project_Id;
         Project_Tree : Project_Tree_Ref)
      is
         procedure Add_Mains_From_Project
           (Project : Project_Id;
            Tree    : Project_Tree_Ref);
         --  Add the main units from this project into Mains.
         --  This takes into account the aggregated projects

         ----------------------------
         -- Add_Mains_From_Project --
         ----------------------------

         procedure Add_Mains_From_Project
           (Project : Project_Id;
            Tree    : Project_Tree_Ref)
         is
            List    : String_List_Id;
            Element : String_Element;

         begin
            if Number_Of_Mains (Tree) = 0
              and then Mains.Count_Of_Mains_With_No_Tree = 0
            then
               Debug_Output ("Add_Mains_From_Project", Project.Name);
               List := Project.Mains;

               if List /= GPR.Nil_String then

                  --  The attribute Main is not an empty list. Get the mains in
                  --  the list.

                  while List /= GPR.Nil_String loop
                     Element := Tree.Shared.String_Elements.Table (List);
                     Debug_Output ("Add_Main", Element.Value);

                     if Project.Library then
                        Fail_Program
                          (Tree,
                           "cannot specify a main program "
                           & "for a library project file");
                     end if;

                     Add_Main (Name     => Get_Name_String (Element.Value),
                               Index    => Element.Index,
                               Location => Element.Location,
                               Project  => Project,
                               Tree     => Tree);
                     List := Element.Next;
                  end loop;
               end if;
            end if;

            if Total_Errors_Detected > 0 then
               Fail_Program (Tree, "problems with main sources");
            end if;
         end Add_Mains_From_Project;

         procedure Fill_All is new For_Project_And_Aggregated
           (Add_Mains_From_Project);

      --  Start of processing for Fill_From_Project

      begin
         Fill_All (Root_Project, Project_Tree);
      end Fill_From_Project;

      ---------------
      -- Next_Main --
      ---------------

      function Next_Main return String is
         Info : constant Main_Info := Next_Main;
      begin
         if Info = No_Main_Info then
            return "";
         else
            return Get_Name_String (Info.File);
         end if;
      end Next_Main;

      function Next_Main return Main_Info is
      begin
         if Current >= Names.Last then
            return No_Main_Info;
         else
            Current := Current + 1;
            return Names.Table (Current);
         end if;
      end Next_Main;

      ---------------------
      -- Number_Of_Mains --
      ---------------------

      function Number_Of_Mains (Tree : Project_Tree_Ref) return Natural is
      begin
         if Tree = null then
            return Names.Last;
         else
            return Builder_Data (Tree).Number_Of_Mains;
         end if;
      end Number_Of_Mains;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Current := 0;
      end Reset;

      --------------------------
      -- Set_Multi_Unit_Index --
      --------------------------

      procedure Set_Multi_Unit_Index
        (Project_Tree : Project_Tree_Ref := null;
         Index        : Int := 0)
      is
      begin
         if Index /= 0 then
            if Names.Last = 0 then
               Fail_Program
                 (Project_Tree,
                  "cannot specify a multi-unit index but no main "
                  & "on the command line");

            elsif Names.Last > 1 then
               Fail_Program
                 (Project_Tree,
                  "cannot specify several mains with a multi-unit index");

            else
               Names.Table (Names.Last).Index := Index;
            end if;
         end if;
      end Set_Multi_Unit_Index;

   end Mains;

   -----------------------
   -- Path_Or_File_Name --
   -----------------------

   function Path_Or_File_Name (Path : Path_Name_Type) return String is
      Path_Name : constant String := Get_Name_String (Path);
   begin
      if Debug.Debug_Flag_F then
         return File_Name (Path_Name);
      else
         return Path_Name;
      end if;
   end Path_Or_File_Name;

   -----------------
   -- Verbose_Msg --
   -----------------

   procedure Verbose_Msg
     (N1                : Name_Id;
      S1                : String;
      N2                : Name_Id := No_Name;
      S2                : String  := "";
      Prefix            : String := "  -> ";
      Minimum_Verbosity : Opt.Verbosity_Level_Type := Opt.Low)
   is
   begin
      if not Opt.Verbose_Mode
        or else Minimum_Verbosity > Opt.Verbosity_Level
      then
         return;
      end if;

      Put (Prefix);
      Put ("""");
      Put (Get_Name_String (N1));
      Put (""" ");
      Put (S1);

      if N2 /= No_Name then
         Put (" """);
         Put (Get_Name_String (N2));
         Put (""" ");
      end if;

      Put (S2);
      New_Line;
   end Verbose_Msg;

   -----------
   -- Queue --
   -----------

   package body Queue is

      type Q_Record is record
         Info      : Source_Info;
         Processed : Boolean;
      end record;

      package Q is new GNAT.Table
        (Table_Component_Type => Q_Record,
         Table_Index_Type     => Natural,
         Table_Low_Bound      => 1,
         Table_Initial        => 1000,
         Table_Increment      => 100);
      --  This is the actual Queue

      package Busy_Obj_Dirs is new GNAT.HTable.Simple_HTable
        (Header_Num => GPR.Header_Num,
         Element    => Boolean,
         No_Element => False,
         Key        => Path_Name_Type,
         Hash       => Hash,
         Equal      => "=");

      Q_Processed   : Natural := 0;
      Q_Initialized : Boolean := False;

      Q_First : Natural := 1;
      --  Points to the first valid element in the queue

      One_Queue_Per_Obj_Dir : Boolean := False;
      --  See parameter to Initialize

      function Available_Obj_Dir (S : Source_Info) return Boolean;
      --  Whether the object directory for S is available for a build

      procedure Debug_Display (S : Source_Info);
      --  A debug display for S

      function Was_Processed (S : Source_Info) return Boolean;
      --  Whether S has already been processed. This marks the source as
      --  processed, if it hasn't already been processed.

      function Insert_No_Roots (Source  : Source_Info) return Boolean;
      --  Insert Source, but do not look for its roots (see doc for Insert)

      -------------------
      -- Was_Processed --
      -------------------

      function Was_Processed (S : Source_Info) return Boolean is
      begin
         return S.Id.In_The_Queue;
      end Was_Processed;

      -----------------------
      -- Available_Obj_Dir --
      -----------------------

      function Available_Obj_Dir (S : Source_Info) return Boolean is
      begin
         return not Busy_Obj_Dirs.Get
           (S.Id.Project.Object_Directory.Name);
      end Available_Obj_Dir;

      -------------------
      -- Debug_Display --
      -------------------

      procedure Debug_Display (S : Source_Info) is
      begin
         Put (Get_Name_String (S.Id.File));

         if S.Id.Index /= 0 then
            Put (",");
            Put (S.Id.Index'Img);
         end if;
      end Debug_Display;

      -------------
      -- Extract --
      -------------

      procedure Extract
        (Found  : out Boolean;
         Source : out Source_Info)
      is
      begin
         Found := False;

         if One_Queue_Per_Obj_Dir then
            for J in Q_First .. Q.Last loop
               if not Q.Table (J).Processed
                 and then Available_Obj_Dir (Q.Table (J).Info)
               then
                  Found := True;
                  Source := Q.Table (J).Info;
                  Q.Table (J).Processed := True;

                  if J = Q_First then
                     while Q_First <= Q.Last
                       and then Q.Table (Q_First).Processed
                     loop
                        Q_First := Q_First + 1;
                     end loop;
                  end if;

                  exit;
               end if;
            end loop;

         elsif Q_First <= Q.Last then
            Source := Q.Table (Q_First).Info;
            Q.Table (Q_First).Processed := True;
            Q_First := Q_First + 1;
            Found := True;
         end if;

         if Found then
            Q_Processed := Q_Processed + 1;
         end if;

         if Found and then Debug.Debug_Flag_Q then
            Ada.Text_IO.Put ("   Q := Q - [ ");
            Debug_Display (Source);
            Ada.Text_IO.Put (" ]");
            New_Line;

            Ada.Text_IO.Put ("   Q_First =");
            Ada.Text_IO.Put (Q_First'Img);
            New_Line;

            Ada.Text_IO.Put ("   Q.Last =");
            Ada.Text_IO.Put (Q.Last'Img);
            New_Line;
         end if;
      end Extract;

      ---------------
      -- Processed --
      ---------------

      function Processed return Natural is
      begin
         return Q_Processed;
      end Processed;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Queue_Per_Obj_Dir : Boolean;
         Force             : Boolean := False)
      is
      begin
         if Force or else not Q_Initialized then
            Q_Initialized := True;

            for J in 1 .. Q.Last loop
               Q.Table (J).Info.Id.In_The_Queue := False;
            end loop;

            Q.Init;
            Q_Processed := 0;
            Q_First     := 1;
            One_Queue_Per_Obj_Dir := Queue_Per_Obj_Dir;
         end if;
      end Initialize;

      ---------------------
      -- Insert_No_Roots --
      ---------------------

      function Insert_No_Roots (Source  : Source_Info) return Boolean is
      begin
         pragma Assert (Source.Id /= No_Source);

         --  Only insert in the Q if it is not already done, to avoid
         --  simultaneous compilations if -jnnn is used.

         if Was_Processed (Source) then
            return False;
         end if;

         --  Check if a source has already been inserted in the queue from the
         --  same project in a different project tree.

         for J in 1 .. Q.Last loop
            if Source.Id.Path.Name = Q.Table (J).Info.Id.Path.Name
              and then Source.Id.Index = Q.Table (J).Info.Id.Index
              and then
                Ultimate_Extending_Project_Of (Source.Id.Project).Path.Name
              =
              Ultimate_Extending_Project_Of (Q.Table (J).Info.Id.Project).
                Path.Name
            then
               --  No need to insert this source in the queue, but still
               --  return True as we may need to insert its roots.

               return True;
            end if;
         end loop;

         if Current_Verbosity = High then
            Put ("Adding """);
            Debug_Display (Source);
            Put_Line (""" to the queue");
         end if;

         Q.Append (New_Val => (Info => Source, Processed => False));
         Source.Id.In_The_Queue := True;

         if Debug.Debug_Flag_Q then
            Ada.Text_IO.Put ("   Q := Q + [ ");
            Debug_Display (Source);
            Ada.Text_IO.Put (" ] ");
            New_Line;

            Ada.Text_IO.Put ("   Q_First =");
            Ada.Text_IO.Put (Q_First'Img);
            New_Line;

            Ada.Text_IO.Put ("   Q.Last =");
            Ada.Text_IO.Put (Q.Last'Img);
            New_Line;
         end if;

         return True;
      end Insert_No_Roots;

      ------------
      -- Insert --
      ------------

      function Insert
        (Source     : Source_Info;
         With_Roots : Boolean := False) return Boolean
      is
         Root_Arr     : Array_Element_Id;
         Roots        : Variable_Value;
         List         : String_List_Id;
         Elem         : String_Element;
         Unit_Name    : Name_Id;
         Pat_Root     : Boolean;
         Root_Pattern : Regexp;
         Root_Found   : Boolean;
         Roots_Found  : Boolean;
         Root_Source  : GPR.Source_Id;
         Iter         : Source_Iterator;

         Dummy : Boolean;

      begin
         if not Insert_No_Roots (Source) then

            --  Was already in the queue

            return False;
         end if;

         if With_Roots then
            Debug_Output ("looking for roots of", Name_Id (Source.Id.File));

            Root_Arr :=
              GPR.Util.Value_Of
                (Name      => Name_Roots,
                 In_Arrays => Source.Id.Project.Decl.Arrays,
                 Shared    => Source.Tree.Shared);

            Roots :=
              GPR.Util.Value_Of
                (Index     => Name_Id (Source.Id.File),
                 Src_Index => 0,
                 In_Array  => Root_Arr,
                 Shared    => Source.Tree.Shared);

            --  If there is no roots for the specific main, try the language

            if Roots = Nil_Variable_Value then
               Roots :=
                 GPR.Util.Value_Of
                   (Index                  => Source.Id.Language.Name,
                    Src_Index              => 0,
                    In_Array               => Root_Arr,
                    Shared                 => Source.Tree.Shared,
                    Force_Lower_Case_Index => True);
            end if;

            --  Then try "*"

            if Roots = Nil_Variable_Value then
               Name_Len := 1;
               Name_Buffer (1) := '*';

               Roots :=
                 GPR.Util.Value_Of
                   (Index                  => Name_Find,
                    Src_Index              => 0,
                    In_Array               => Root_Arr,
                    Shared                 => Source.Tree.Shared,
                    Force_Lower_Case_Index => True);
            end if;

            if Roots = Nil_Variable_Value then
               Debug_Output ("   -> no roots declared");

            else
               List := Roots.Values;

               --  case of empty root list
               if List = Nil_String then
                  Source.Id.Roots :=
                    new Source_Roots'
                      (Root => No_Source,
                       Next => null);
               end if;

               Pattern_Loop :
               while List /= Nil_String loop
                  Elem := Source.Tree.Shared.String_Elements.Table (List);
                  Get_Name_String (Elem.Value);
                  To_Lower (Name_Buffer (1 .. Name_Len));
                  Unit_Name := Name_Find;

                  --  Check if it is a unit name or a pattern

                  Pat_Root := False;

                  for J in 1 .. Name_Len loop
                     if Name_Buffer (J) not in 'a' .. 'z' and then
                        Name_Buffer (J) not in '0' .. '9' and then
                        Name_Buffer (J) /= '_'            and then
                        Name_Buffer (J) /= '.'
                     then
                        Pat_Root := True;
                        exit;
                     end if;
                  end loop;

                  if Pat_Root then
                     begin
                        Root_Pattern :=
                          Compile
                            (Pattern => Name_Buffer (1 .. Name_Len),
                             Glob    => True);

                     exception
                        when Error_In_Regexp =>
                           Error_Msg_Name_1 := Unit_Name;
                           Error_Msg
                             ("invalid pattern %", Roots.Location);
                           exit Pattern_Loop;
                     end;
                  end if;

                  Roots_Found := False;
                  Iter        := For_Each_Source (Source.Tree);

                  Source_Loop :
                  loop
                     Root_Source := GPR.Element (Iter);
                     exit Source_Loop when Root_Source = No_Source;

                     Root_Found := False;
                     if Pat_Root then
                        Root_Found := Root_Source.Unit /= No_Unit_Index
                          and then Match
                            (Get_Name_String (Root_Source.Unit.Name),
                             Root_Pattern);

                     else
                        Root_Found :=
                          Root_Source.Unit /= No_Unit_Index
                            and then Root_Source.Unit.Name = Unit_Name;
                     end if;

                     if Root_Found then
                        case Root_Source.Kind is
                        when Impl =>
                           null;

                        when Spec =>
                           Root_Found := Other_Part (Root_Source) = No_Source;

                        when Sep =>
                           Root_Found := False;
                        end case;
                     end if;

                     if Root_Found then
                        Roots_Found := True;
                        Debug_Output
                          ("   -> ", Name_Id (Root_Source.Display_File));
                        Dummy := Queue.Insert_No_Roots
                          (Source => (Tree    => Source.Tree,
                                      Id      => Root_Source,
                                      Closure => False));

                        Initialize_Source_Record (Root_Source);

                        if Other_Part (Root_Source) /= No_Source then
                           Initialize_Source_Record (Other_Part (Root_Source));
                        end if;

                        --  Save the root for the binder

                        Source.Id.Roots := new Source_Roots'
                          (Root => Root_Source,
                           Next => Source.Id.Roots);

                        exit Source_Loop when not Pat_Root;
                     end if;

                     Next (Iter);
                  end loop Source_Loop;

                  if not Roots_Found then
                     if Pat_Root then
                        if not Quiet_Output then
                           Error_Msg_Name_1 := Unit_Name;
                           Error_Msg
                             ("?no unit matches pattern %", Roots.Location);
                        end if;

                     else
                        Error_Msg
                          ("Unit " & Get_Name_String (Unit_Name)
                           & " does not exist", Roots.Location);
                     end if;
                  end if;

                  List := Elem.Next;
               end loop Pattern_Loop;
            end if;
         end if;

         return True;
      end Insert;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Source     : Source_Info;
         With_Roots : Boolean := False)
      is
         Discard : Boolean;
      begin
         Discard := Insert (Source, With_Roots);
      end Insert;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty return Boolean is
      begin
         return Q_Processed >= Q.Last;
      end Is_Empty;

      ------------------------
      -- Is_Virtually_Empty --
      ------------------------

      function Is_Virtually_Empty return Boolean is
      begin
         if One_Queue_Per_Obj_Dir then
            for J in Q_First .. Q.Last loop
               if not Q.Table (J).Processed
                 and then Available_Obj_Dir (Q.Table (J).Info)
               then
                  return False;
               end if;
            end loop;

            return True;

         else
            return Is_Empty;
         end if;
      end Is_Virtually_Empty;

      ----------------------
      -- Set_Obj_Dir_Busy --
      ----------------------

      procedure Set_Obj_Dir_Busy (Obj_Dir : Path_Name_Type) is
      begin
         if One_Queue_Per_Obj_Dir then
            Busy_Obj_Dirs.Set (Obj_Dir, True);
         end if;
      end Set_Obj_Dir_Busy;

      ----------------------
      -- Set_Obj_Dir_Free --
      ----------------------

      procedure Set_Obj_Dir_Free (Obj_Dir : Path_Name_Type) is
      begin
         if One_Queue_Per_Obj_Dir then
            Busy_Obj_Dirs.Set (Obj_Dir, False);
         end if;
      end Set_Obj_Dir_Free;

      ----------
      -- Size --
      ----------

      function Size return Natural is
      begin
         return Q.Last;
      end Size;

      -------------
      -- Element --
      -------------

      function Element (Rank : Positive) return File_Name_Type is
      begin
         if Rank <= Q.Last then
            return Q.Table (Rank).Info.Id.File;
         else
            return No_File;
         end if;
      end Element;

      ----------------------------
      -- Insert_Project_Sources --
      ----------------------------

      procedure Insert_Project_Sources
        (Project        : Project_Id;
         Project_Tree   : Project_Tree_Ref;
         All_Projects   : Boolean;
         Unique_Compile : Boolean)
      is

         procedure Do_Insert
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Context : Project_Context);
         --  Local procedures must be commented ???

         ---------------
         -- Do_Insert --
         ---------------

         procedure Do_Insert
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Context : Project_Context)
         is
            Unit_Based : constant Boolean :=
                           Unique_Compile
                             or else not Builder_Data (Tree).Closure_Needed;
            --  When Unit_Based is True, we enqueue all compilable sources
            --  including the unit based (Ada) one. When Unit_Based is False,
            --  put the Ada sources only when they are in a library project.

            Iter    : Source_Iterator;
            Source  : GPR.Source_Id;
            OK      : Boolean;
            Closure : Boolean;

            Proj : Project_Id;

         begin
            --  Nothing to do when "-u" was specified and some files were
            --  specified on the command line

            if Unique_Compile and then Mains.Number_Of_Mains (Tree) > 0 then
               return;
            end if;

            Iter := For_Each_Source (Tree);
            loop
               Source := GPR.Element (Iter);
               exit when Source = No_Source;
               Proj := Ultimate_Extending_Project_Of (Source.Project);

               if Is_Allowed_Language (Source.Language.Name)
                 and then Is_Compilable (Source)
                 and then (All_Projects
                            or else Is_Extending (Project, Source.Project))
                 and then not Source.Locally_Removed
                 and then Source.Replaced_By = No_Source
                 and then not Proj.Externally_Built
                 and then Source.Kind /= Sep
                 and then Source.Path /= No_Path_Information
               then
                  if Source.Kind = Impl
                    or else (Source.Unit /= No_Unit_Index
                              and then Source.Kind = Spec
                              and then (Other_Part (Source) = No_Source
                                          or else
                                        Other_Part (Source).Locally_Removed))
                  then
                     if (Unit_Based
                          or else Source.Unit = No_Unit_Index
                          or else Source.Project.Library
                          or else Context.In_Aggregate_Lib
                          or else Project.Qualifier = Aggregate_Library)
                       and then not Is_Subunit (Source)
                     then
                        OK := True;
                        Closure := False;

                        if Source.Unit /= No_Unit_Index
                          and then
                            (Source.Project.Library
                              or else Project.Qualifier = Aggregate_Library
                              or else Context.In_Aggregate_Lib)
                          and then Source.Project.Standalone_Library /= No
                        then
                           --  Check if the unit is in the interface

                           OK := False;

                           declare
                              List    : String_List_Id;
                              Element : String_Element;

                           begin
                              List := Source.Project.Lib_Interface_ALIs;
                              while List /= Nil_String loop
                                 Element :=
                                   Project_Tree.Shared.String_Elements.Table
                                     (List);

                                 if Element.Value = Name_Id (Source.Dep_Name)
                                 then
                                    OK := True;
                                    Closure := True;
                                    exit;
                                 end if;

                                 List := Element.Next;
                              end loop;
                           end;
                        end if;

                        if OK then
                           Queue.Insert
                             (Source => (Tree    => Tree,
                                         Id      => Source,
                                         Closure => Closure));
                        end if;
                     end if;
                  end if;
               end if;

               Next (Iter);
            end loop;
         end Do_Insert;

         procedure Insert_All is
           new For_Project_And_Aggregated_Context (Do_Insert);

      begin
         Insert_All (Project, Project_Tree);
      end Insert_Project_Sources;

      -------------------------------
      -- Insert_Withed_Sources_For --
      -------------------------------

      procedure Insert_Withed_Sources_For
        (The_ALI               : ALI.ALI_Id;
         Project_Tree          : Project_Tree_Ref;
         Excluding_Shared_SALs : Boolean := False)
      is
         Sfile  : File_Name_Type;
         Afile  : File_Name_Type;
         Src_Id : GPR.Source_Id;

      begin
         --  Insert in the queue the unmarked source files (i.e. those which
         --  have never been inserted in the queue and hence never considered).

         for J in ALI.ALIs.Table (The_ALI).First_Unit ..
           ALI.ALIs.Table (The_ALI).Last_Unit
         loop
            for K in ALI.Units.Table (J).First_With ..
              ALI.Units.Table (J).Last_With
            loop
               Sfile := ALI.Withs.Table (K).Sfile;

               --  Skip generics

               if Sfile /= No_File then
                  Afile := ALI.Withs.Table (K).Afile;

                  Src_Id := Source_Files_Htable.Get
                              (Project_Tree.Source_Files_HT, Sfile);
                  while Src_Id /= No_Source loop
                     Initialize_Source_Record (Src_Id);

                     if Is_Compilable (Src_Id)
                       and then Src_Id.Dep_Name = Afile
                     then
                        case Src_Id.Kind is
                           when Spec =>
                              declare
                                 Bdy : constant GPR.Source_Id :=
                                         Other_Part (Src_Id);
                              begin
                                 if Bdy /= No_Source
                                   and then not Bdy.Locally_Removed
                                 then
                                    Src_Id := Other_Part (Src_Id);
                                 end if;
                              end;

                           when Impl =>
                              if Is_Subunit (Src_Id) then
                                 Src_Id := No_Source;
                              end if;

                           when Sep =>
                              Src_Id := No_Source;
                        end case;

                        exit;
                     end if;

                     Src_Id := Src_Id.Next_With_File_Name;
                  end loop;

                  --  If Excluding_Shared_SALs is True, do not insert in the
                  --  queue the sources of a shared Stand-Alone Library.

                  if Src_Id /= No_Source
                    and then (not Excluding_Shared_SALs
                               or else Src_Id.Project.Standalone_Library = No
                               or else Src_Id.Project.Library_Kind = Static)
                  then
                     Queue.Insert
                       (Source => (Tree    => Project_Tree,
                                   Id      => Src_Id,
                                   Closure => True));
                  end if;
               end if;
            end loop;
         end loop;
      end Insert_Withed_Sources_For;

   end Queue;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Builder_Project_Tree_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Binding_Data_Record, Binding_Data);

      TmpB, Binding : Binding_Data := Data.Binding;

   begin
      while Binding /= null loop
         TmpB := Binding.Next;
         Unchecked_Free (Binding);
         Binding := TmpB;
      end loop;
   end Free;

   ------------------
   -- Builder_Data --
   ------------------

   function Builder_Data
     (Tree : Project_Tree_Ref) return Builder_Data_Access
   is
   begin
      if Tree.Appdata = null then
         Tree.Appdata := new Builder_Project_Tree_Data;
      end if;

      return Builder_Data_Access (Tree.Appdata);
   end Builder_Data;

   --------------------------------
   -- Compute_Compilation_Phases --
   --------------------------------

   procedure Compute_Compilation_Phases
     (Tree                  : Project_Tree_Ref;
      Root_Project          : Project_Id;
      Option_Unique_Compile : Boolean := False;   --  Was "-u" specified ?
      Option_Compile_Only   : Boolean := False;   --  Was "-c" specified ?
      Option_Bind_Only      : Boolean := False;
      Option_Link_Only      : Boolean := False)
   is
      procedure Do_Compute (Project : Project_Id; Tree : Project_Tree_Ref);

      ----------------
      -- Do_Compute --
      ----------------

      procedure Do_Compute (Project : Project_Id; Tree : Project_Tree_Ref) is
         Data       : constant Builder_Data_Access := Builder_Data (Tree);
         All_Phases : constant Boolean :=
                        not Option_Compile_Only
                        and then not Option_Bind_Only
                        and then not Option_Link_Only;
         --  Whether the command line asked for all three phases. Depending on
         --  the project settings, we might still disable some of the phases.

         Has_Mains : constant Boolean := Data.Number_Of_Mains > 0;
         --  Whether there are some main units defined for this project tree
         --  (either from one of the projects, or from the command line)

      begin
         if Option_Unique_Compile then

            --  If -u or -U is specified on the command line, disregard any -c,
            --  -b or -l switch: only perform compilation.

            Data.Closure_Needed   := False;
            Data.Need_Compilation := True;
            Data.Need_Binding     := False;
            Data.Need_Linking     := False;

         else
            Data.Closure_Needed   :=
              Has_Mains
                or else (Root_Project.Library
                          and then Root_Project.Standalone_Library /= No);
            Data.Need_Compilation := All_Phases or Option_Compile_Only;
            Data.Need_Binding     := All_Phases or Option_Bind_Only;
            Data.Need_Linking     := (All_Phases or Option_Link_Only)
                                       and Has_Mains;
         end if;

         if Current_Verbosity = High then
            Debug_Output ("compilation phases: "
                          & " compile=" & Data.Need_Compilation'Img
                          & " bind="    & Data.Need_Binding'Img
                          & " link="    & Data.Need_Linking'Img
                          & " closure=" & Data.Closure_Needed'Img
                          & " mains="   & Data.Number_Of_Mains'Img,
                          Project.Name);
         end if;
      end Do_Compute;

      procedure Compute_All is new For_Project_And_Aggregated (Do_Compute);

   begin
      Compute_All (Root_Project, Tree);
   end Compute_Compilation_Phases;

   ------------------------------
   -- Compute_Builder_Switches --
   ------------------------------

   procedure Compute_Builder_Switches
     (Project_Tree        : Project_Tree_Ref;
      Env                 : in out GPR.Tree.Environment;
      Main_Project        : Project_Id;
      Only_For_Lang       : Name_Id := No_Name)
   is
      Builder_Package  : constant Package_Id :=
                           Value_Of (Name_Builder, Main_Project.Decl.Packages,
                                     Project_Tree.Shared);

      Global_Compilation_Array    : Array_Element_Id := No_Array_Element;
      Global_Compilation_Elem     : Array_Element;
      Global_Compilation_Switches : Variable_Value;

      Default_Switches_Array : Array_Id;

      Builder_Switches_Lang : Name_Id := No_Name;

      List             : String_List_Id;
      Element          : String_Element;

      Index            : Name_Id;
      Source           : GPR.Source_Id;

      Lang              : Name_Id := No_Name;  --  language index for Switches
      Switches_For_Lang : Variable_Value := Nil_Variable_Value;
      --  Value of Builder'Default_Switches(lang)

      Name              : Name_Id := No_Name;  --  main file index for Switches
      Switches_For_Main : Variable_Value := Nil_Variable_Value;
      --  Switches for a specific main. When there are several mains, Name is
      --  set to No_Name, and Switches_For_Main might be left with an actual
      --  value (so that we can display a warning that it was ignored).

      Other_Switches : Variable_Value := Nil_Variable_Value;
      --  Value of Builder'Switches(others)

      Defaults : Variable_Value := Nil_Variable_Value;

      Switches : Variable_Value := Nil_Variable_Value;
      --  The computed builder switches

      Success          : Boolean := False;
   begin
      if Builder_Package /= No_Package then
         Global_Compilation_Array := Value_Of
           (Name      => Name_Global_Compilation_Switches,
            In_Arrays => Project_Tree.Shared.Packages.Table
              (Builder_Package).Decl.Arrays,
            Shared    => Project_Tree.Shared);

         if Main_Project.Qualifier = Aggregate or else
           Main_Project.Qualifier = Aggregate_Library
         then
            Other_Switches := GPR.Util.Value_Of
              (Name                    => All_Other_Names,
               Index                   => 0,
               Attribute_Or_Array_Name => Name_Switches,
               In_Package              => Builder_Package,
               Shared                  => Project_Tree.Shared);

         else
            Mains.Reset;

            --  If there is no main, and there is only one compilable language,
            --  use this language as the switches index.

            if Mains.Number_Of_Mains (Project_Tree) = 0 then
               if Only_For_Lang = No_Name then
                  declare
                     Language : Language_Ptr := Main_Project.Languages;

                  begin
                     while Language /= No_Language_Index loop
                        if Language.Config.Compiler_Driver /= No_File
                           and then
                           Language.Config.Compiler_Driver /= Empty_File
                        then
                           if Lang /= No_Name then
                              Lang := No_Name;
                              exit;
                           else
                              Lang := Language.Name;
                           end if;
                        end if;
                        Language := Language.Next;
                     end loop;
                  end;
               else
                  Lang := Only_For_Lang;
               end if;

            else
               for Index in 1 .. Mains.Number_Of_Mains (Project_Tree) loop
                  Source := Mains.Next_Main.Source;

                  if Source /= No_Source then
                     if Switches_For_Main = Nil_Variable_Value then
                        Switches_For_Main := Value_Of
                          (Name                    => Name_Id (Source.File),
                           Attribute_Or_Array_Name => Name_Switches,
                           In_Package              => Builder_Package,
                           Shared                  => Project_Tree.Shared,
                           Force_Lower_Case_Index  => False,
                           Allow_Wildcards         => True);

                        --  If not found, try without extension.
                        --  That's because gnatmake accepts truncated file
                        --  names in Builder'Switches

                        if Switches_For_Main = Nil_Variable_Value
                          and then Source.Unit /= null
                        then
                           Switches_For_Main := Value_Of
                             (Name                    => Source.Unit.Name,
                              Attribute_Or_Array_Name => Name_Switches,
                              In_Package              => Builder_Package,
                              Shared                  => Project_Tree.Shared,
                              Force_Lower_Case_Index  => False,
                              Allow_Wildcards         => True);
                        end if;
                     end if;

                     if Index = 1 then
                        Lang := Source.Language.Name;
                        Name := Name_Id (Source.File);
                     else
                        Name := No_Name;  --  Can't use main specific switches

                        if Lang /= Source.Language.Name then
                           Lang := No_Name;
                        end if;
                     end if;
                  end if;
               end loop;
            end if;

            Default_Switches_Array :=
              Project_Tree.Shared.Packages.Table (Builder_Package).Decl.Arrays;

            while Default_Switches_Array /= No_Array
              and then
                Project_Tree.Shared.Arrays.Table
                  (Default_Switches_Array).Name /= Name_Default_Switches
            loop
               Default_Switches_Array :=
                 Project_Tree.Shared.Arrays.Table
                   (Default_Switches_Array).Next;
            end loop;

            if Global_Compilation_Array /= No_Array_Element
              and then Default_Switches_Array /= No_Array
            then
               GPR.Err.Error_Msg
                 (Env.Flags,
                  "Default_Switches forbidden in presence of "
                  & "Global_Compilation_Switches. Use Switches instead.",
                  Project_Tree.Shared.Arrays.Table
                    (Default_Switches_Array).Location);
               Fail_Program
                 (Project_Tree,
                  "*** illegal combination of Builder attributes");
            end if;

            if Lang /= No_Name then
               Switches_For_Lang := GPR.Util.Value_Of
                 (Name                    => Lang,
                  Index                   => 0,
                  Attribute_Or_Array_Name => Name_Switches,
                  In_Package              => Builder_Package,
                  Shared                  => Project_Tree.Shared,
                  Force_Lower_Case_Index  => True);

               Defaults := GPR.Util.Value_Of
                 (Name                    => Lang,
                  Index                   => 0,
                  Attribute_Or_Array_Name => Name_Default_Switches,
                  In_Package              => Builder_Package,
                  Shared                  => Project_Tree.Shared,
                  Force_Lower_Case_Index  => True);
            end if;

            Other_Switches := GPR.Util.Value_Of
              (Name                    => All_Other_Names,
               Index                   => 0,
               Attribute_Or_Array_Name => Name_Switches,
               In_Package              => Builder_Package,
               Shared                  => Project_Tree.Shared);

            if not Quiet_Output
              and then Mains.Number_Of_Mains (Project_Tree) > 1
              and then Switches_For_Main /= Nil_Variable_Value
            then
               --  More than one main, but we had main-specific switches that
               --  are ignored.

               if Switches_For_Lang /= Nil_Variable_Value then
                  Put_Line
                    ("Warning: using Builder'Switches("""
                     & Get_Name_String (Lang)
                     & """), as there are several mains");

               elsif Other_Switches /= Nil_Variable_Value then
                  Put_Line
                    ("Warning: using Builder'Switches(others), "
                     & "as there are several mains");

               elsif Defaults /= Nil_Variable_Value then
                  Put_Line
                    ("Warning: using Builder'Default_Switches("""
                     & Get_Name_String (Lang)
                     & """), as there are several mains");
               else
                  Put_Line
                    ("Warning: using no switches from package "
                     & "Builder, as there are several mains");
               end if;
            end if;
         end if;

         Builder_Switches_Lang := Lang;

         if Name /= No_Name then
            --  Get the switches for the single main
            Switches := Switches_For_Main;
         end if;

         if Switches = Nil_Variable_Value or else Switches.Default then
            --  Get the switches for the common language of the mains
            Switches := Switches_For_Lang;
         end if;

         if Switches = Nil_Variable_Value or else Switches.Default then
            Switches := Other_Switches;
         end if;

         --  For backward compatibility with gnatmake, if no Switches
         --  are declared, check for Default_Switches (<language>).

         if Switches = Nil_Variable_Value or else Switches.Default then
            Switches := Defaults;
         end if;

         --  If switches have been found, scan them

         if Switches /= Nil_Variable_Value and then not Switches.Default then
            List := Switches.Values;

            while List /= Nil_String loop
               Element := Project_Tree.Shared.String_Elements.Table (List);
               Get_Name_String (Element.Value);

               if Name_Len /= 0 then
                  declare
                     --  Add_Switch might itself be using the name_buffer, so
                     --  we make a temporary here.
                     Switch : constant String := Name_Buffer (1 .. Name_Len);
                  begin
                     Success := Add_Switch
                       (Switch      => Switch,
                        For_Lang    => Builder_Switches_Lang,
                        For_Builder => True,
                        Has_Global_Compilation_Switches =>
                          Global_Compilation_Array /= No_Array_Element);
                  end;

                  if not Success then
                     for J in reverse 1 .. Name_Len loop
                        Name_Buffer (J + J) := Name_Buffer (J);
                        Name_Buffer (J + J - 1) := ''';
                     end loop;

                     Name_Len := Name_Len + Name_Len;

                     GPR.Err.Error_Msg
                       (Env.Flags,
                        '"' & Name_Buffer (1 .. Name_Len)
                        & """ is not a builder switch. Consider moving "
                        & "it to Global_Compilation_Switches.",
                        Element.Location);

                     Fail_Program
                       (Project_Tree,
                        "*** illegal switch """
                        & Get_Name_String (Element.Value) & '"');
                  end if;
               end if;

               List := Element.Next;
            end loop;
         end if;

         --  Reset the Builder Switches language

         Builder_Switches_Lang := No_Name;

         --  Take into account attributes Global_Compilation_Switches

         while Global_Compilation_Array /= No_Array_Element loop
            Global_Compilation_Elem :=
              Project_Tree.Shared.Array_Elements.Table
                (Global_Compilation_Array);

            Get_Name_String (Global_Compilation_Elem.Index);
            To_Lower (Name_Buffer (1 .. Name_Len));
            Index := Name_Find;

            if Only_For_Lang = No_Name or else Index = Only_For_Lang then
               Global_Compilation_Switches := Global_Compilation_Elem.Value;

               if Global_Compilation_Switches /= Nil_Variable_Value
                 and then not Global_Compilation_Switches.Default
               then
                  --  We have found an attribute
                  --  Global_Compilation_Switches for a language: put the
                  --  switches in the appropriate table.

                  List := Global_Compilation_Switches.Values;
                  while List /= Nil_String loop
                     Element :=
                       Project_Tree.Shared.String_Elements.Table (List);

                     if Element.Value /= No_Name then
                        Success := Add_Switch
                          (Switch      => Get_Name_String (Element.Value),
                           For_Lang    => Index,
                           For_Builder => False,
                           Has_Global_Compilation_Switches =>
                             Global_Compilation_Array /= No_Array_Element);
                     end if;

                     List := Element.Next;
                  end loop;
               end if;
            end if;

            Global_Compilation_Array := Global_Compilation_Elem.Next;
         end loop;
      end if;
   end Compute_Builder_Switches;

   --------------
   -- Unescape --
   --------------

   function Unescape (Path : String) return String is
      Result : String (1 .. Path'Length);
      Last   : Natural := 0;
      Index  : Integer;
   begin
      if On_Windows then
         return Path;
      end if;

      Index := Path'First;
      while Index <= Path'Last loop
         if Path (Index) = '\' then
            if Index < Path'Last and Path (Index + 1) = '\' then
               Last := Last + 1;
               Result (Last) := '\';
               Index := Index + 1;
            end if;

         else
            Last := Last + 1;
            Result (Last) := Path (Index);
         end if;

         Index := Index + 1;
      end loop;

      return Result (1 .. Last);
   end Unescape;

   ---------------------
   -- Write_Path_File --
   ---------------------

   procedure Write_Path_File (FD : File_Descriptor) is
      Last   : Natural;
      Status : Boolean;

   begin
      Name_Len := 0;

      for Index in Directories.First .. Directories.Last loop
         Add_Str_To_Name_Buffer (Get_Name_String (Directories.Table (Index)));
         Add_Char_To_Name_Buffer (ASCII.LF);
      end loop;

      Last := Write (FD, Name_Buffer (1)'Address, Name_Len);

      if Last = Name_Len then
         Close (FD, Status);
      else
         Status := False;
      end if;

      if not Status then
         GPR.Com.Fail ("could not write temporary file");
      end if;
   end Write_Path_File;

end Gpr_Build_Util;
