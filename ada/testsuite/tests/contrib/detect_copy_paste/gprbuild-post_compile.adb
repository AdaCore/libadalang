------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2011-2017, AdaCore                     --
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

with Ada.Containers.Ordered_Sets;
with Ada.Directories;
with Ada.Text_IO;                 use Ada, Ada.Text_IO;

with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Gpr_Build_Util; use Gpr_Build_Util;
with Gpr_Script;     use Gpr_Script;
with Gpr_Util;       use Gpr_Util;
with Gprexch;        use Gprexch;
with GPR.Debug;      use GPR.Debug;
with GPR.Env;
with GPR.Err;        use GPR.Err;
with GPR.Names;      use GPR.Names;
with GPR.Opt;
with GPR.Snames;     use GPR.Snames;
with GPR.Tempdir;
with GPR.Util;       use GPR.Util;

package body Gprbuild.Post_Compile is

   type Lang_Names is array (Positive range <>) of Language_Ptr;
   type Lang_Names_Ptr is access Lang_Names;

   Langs : Lang_Names_Ptr := new Lang_Names (1 .. 4);
   Last_Lang : Natural := 0;

   procedure Build_Library
     (For_Project  : Project_Id;
      Project_Tree : Project_Tree_Ref;
      No_Create    : Boolean);
   --  Build, if necessary, the library of a library project. If No_Create
   --  is True then the actual static or shared library is not built, yet
   --  the exchange file with dependencies is created.

   procedure Post_Compilation_Phase
     (Main_Project : Project_Id; Project_Tree : Project_Tree_Ref);

   function Is_Included_In_Global_Archive
     (Object_Name : File_Name_Type;
      Project     : Project_Id) return Boolean;
   --  Return True if the object Object_Name is not overridden by a source
   --  in a project extending project Project.

   type Library_Object is record
      Path  : Path_Name_Type;
      TS    : Time_Stamp_Type;
      Known : Boolean;
   end record;

   --  Dependency Files

   type Dep_Name;
   type Dep_Ptr is access Dep_Name;
   type Dep_Name is record
      Name : String_Access;
      Next : Dep_Ptr;
   end record;

   First_Dep : Dep_Ptr;
   --  Head of the list of dependency file path names

   procedure Add_Dep (Name : String);
   --  Insert a dependency file path name in the list starting at First_Dep,
   --  at the right place so that the list is sorted.

   ----------------
   -- Add_Dep --
   ----------------

   procedure Add_Dep (Name : String) is
      Next : Dep_Ptr := First_Dep;
   begin
      if Next = null or else Name < Next.Name.all then
         First_Dep := new Dep_Name'(new String'(Name), Next);

      else
         while Next.Next /= null and then
           Name > Next.Next.Name.all
         loop
            Next := Next.Next;
         end loop;

         Next.Next := new Dep_Name'(new String'(Name), Next.Next);
      end if;
   end Add_Dep;

   -------------------
   -- Build_Library --
   -------------------

   procedure Build_Library
     (For_Project  : Project_Id;
      Project_Tree : Project_Tree_Ref;
      No_Create    : Boolean)
   is
      package Library_Objs is new GNAT.Table
        (Table_Component_Type => Library_Object,
         Table_Index_Type     => Integer,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100);
      --  Objects that are in the library file with their time stamps

      package Library_SAL_Projs is new GNAT.Table
        (Table_Component_Type => Project_Id,
         Table_Index_Type     => Integer,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100);
      --  List of non extended projects that are part of a Stand-Alone
      --  (aggregate) library project.

      package Library_Sources is new GNAT.Table
        (Table_Component_Type => Source_Id,
         Table_Index_Type     => Integer,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100);
      --  Library Ada sources of Stand-Alone library, that is sources of the
      --  project in the closure of the interface.

      package Closure_Sources is new GNAT.Table
        (Table_Component_Type => Source_Id,
         Table_Index_Type     => Integer,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100);
      --  Library Ada sources of Stand-Alone library, that is sources
      --  in the closure of the interface, including in imported projects.

      package Interface_ALIs is new GNAT.HTable.Simple_HTable
        (Header_Num => GPR.Header_Num,
         Element    => Boolean,
         No_Element => False,
         Key        => File_Name_Type,
         Hash       => GPR.Hash,
         Equal      => "=");
      --  The ALI files in the interface sets

      Expected_File_Name : String_Access;
      --  Expected library file name

      Mapping_Path : Path_Name_Type := No_Path;
      --  The path name of an eventual binder mapping file

      Mapping_FD : File_Descriptor := Invalid_FD;
      --  A File Descriptor for an eventual binder mapping file

      Library_Options_Success : Boolean := False;

      package Lang_Set is new Containers.Ordered_Sets (Name_Id);

      procedure Get_Objects;
      --  Get the paths of the object files of the library in table
      --  Library_Objs.

      procedure Write_List
        (File  : Text_IO.File_Type;
         Label : Library_Section;
         List  : String_List_Id);
      --  Write values in list into section Label in the given file. Ouptut
      --  Label if it is not the current section.

      procedure Write_Name_List
        (File  : Text_IO.File_Type;
         Label : Library_Section;
         List  : Name_List_Index);
      --  Write name list values into the File, output Label first. Ouptut
      --  Label if it is not the current section.

      --  Procedures to write specific sections of the exchange file

      procedure Write_Object_Files;
      procedure Write_Object_Directory;
      procedure Write_Compilers;
      procedure Write_Compiler_Leading_Switches;
      procedure Write_Compiler_Trailing_Switches;
      procedure Write_Partial_Linker;
      procedure Write_Shared_Lib_Minimum_Options;
      procedure Write_Library_Version;
      procedure Write_Runtime_Library_Dir;
      procedure Write_Auto_Init;
      procedure Write_Run_Path_Option;
      procedure Write_Leading_Library_Options;
      procedure Write_Library_Options (Success : out Boolean);
      procedure Write_Library_Rpath_Options;
      procedure Write_Imported_Libraries;
      procedure Write_Dependency_Files;
      procedure Write_Toolchain_Version;
      procedure Write_Interface_Dep_Files;
      procedure Write_Other_Interfaces;
      procedure Write_Interface_Obj_Files;
      procedure Write_Sources;
      procedure Write_Response_Files;
      procedure Write_Mapping_File;

      Project_Name       : constant String :=
                             Get_Name_String (For_Project.Name);
      Current_Dir        : constant String := Get_Current_Dir;

      Exchange_File      : Text_IO.File_Type;
      Exchange_File_Name : String_Access;

      Latest_Object_TS : Time_Stamp_Type := Empty_Time_Stamp;

      Library_Builder_Name      : String_Access;
      Library_Builder           : String_Access;
      Library_Needs_To_Be_Built : Boolean := False;

      Object_Path : Path_Name_Type;
      Object_TS   : Time_Stamp_Type;
      Source      : Source_Id;
      Project     : Project_Id;
      Disregard   : Boolean;
      Path_Found  : Boolean;
      Iter        : Source_Iterator;

      Current_Section : Library_Section := No_Library_Section;

      -----------------
      -- Get_Objects --
      -----------------

      procedure Get_Objects is

         package Library_ALIs is new GNAT.HTable.Simple_HTable
           (Header_Num => GPR.Header_Num,
            Element    => Boolean,
            No_Element => False,
            Key        => File_Name_Type,
            Hash       => GPR.Hash,
            Equal      => "=");
         --  The ALI files of the Stand-Alone Library project

         package Processed_ALIs is new GNAT.HTable.Simple_HTable
           (Header_Num => GPR.Header_Num,
            Element    => Boolean,
            No_Element => False,
            Key        => File_Name_Type,
            Hash       => GPR.Hash,
            Equal      => "=");
         --  The ALI files that have been processed to check if the
         --  corresponding library unit is in the interface set.

         Never : constant Time_Stamp_Type := (others => '9');
         --  A time stamp that is greater than any real one

         procedure Process
           (Proj : Project_Id;
            Tree : Project_Tree_Ref);
         --  Get objects for non Stand-Alone library

         procedure Process_ALI
           (The_ALI : File_Name_Type;
            Proj    : Project_Id);
         --  Check if the closure of a library unit which is or should be in
         --  the interface set is also in the interface set. Issue a warning
         --  for each missing library unit.

         procedure Process_Standalone
           (Proj : Project_Id;
            Tree : Project_Tree_Ref);
         --  Get objects for a Stand-Alone Library

         procedure Get_Closure;
         --  For Stand-Alone libraries, get the closure of the Ada interface
         --  and put the object files in Library_Objs.

         -------------
         -- Process --
         -------------

         procedure Process
           (Proj  : Project_Id;
            Tree  : Project_Tree_Ref)
         is
            pragma Unreferenced (Tree);

            Never : constant Time_Stamp_Type := (others => '9');
            --  A time stamp that is greater than any real one

            Source : Source_Id;
            Iter   : Source_Iterator;
         begin
            Iter := For_Each_Source (Project_Tree, Proj);
            loop
               Source := GPR.Element (Iter);
               exit when Source = No_Source;

               --  Always get the time stamps when the main project is an
               --  aggregate project.

               Initialize_Source_Record
                 (Source, Always => Main_Project.Qualifier = Aggregate);

               Change_To_Object_Directory (Source.Project);

               if Is_Compilable (Source)
                 and then Source.Replaced_By = No_Source
                 and then Source.Language.Config.Objects_Linked
                 and then
                   ((Source.Unit = No_Unit_Index
                     and then Source.Kind = Impl)
                    or else
                      (Source.Unit /= No_Unit_Index
                       and then (Source.Kind = Impl
                                 or else Other_Part (Source) = No_Source)
                       and then not Is_Subunit (Source)))
                 and then
                   (not Source.Project.Externally_Built
                    or else not For_Project.Externally_Built
                    or else Source.Project.Extended_By /= No_Project)
               then
                  Library_Objs.Append
                    ((Path  => Source.Object_Path,
                      TS    => Source.Object_TS,
                      Known => False));

                  if Source.Object_TS = Empty_Time_Stamp then
                     Latest_Object_TS := Never;

                     if not Library_Needs_To_Be_Built then
                        Library_Needs_To_Be_Built := True;

                        if Opt.Verbosity_Level > Opt.Low then
                           Put ("      -> missing object file: ");
                           Get_Name_String (Source.Object);
                           Put_Line (Name_Buffer (1 .. Name_Len));
                        end if;
                     end if;

                  elsif Source.Object_TS > Latest_Object_TS then
                     Latest_Object_TS := Source.Object_TS;
                  end if;
               end if;

               Next (Iter);
            end loop;
         end Process;

         -----------------
         -- Process_ALI --
         -----------------

         Interface_Incomplete : Boolean := False;

         procedure Process_ALI
           (The_ALI : File_Name_Type;
            Proj    : Project_Id)
         is
            use ALI;
            Text       : Text_Buffer_Ptr;
            Idread     : ALI_Id;
            First_Unit : Unit_Id;
            Last_Unit  : ALI.Unit_Id;
            Unit_Data  : ALI.Unit_Record;
            Afile      : File_Name_Type;

         begin
            --  Nothing to do if the ALI file has already been processed.
            --  This happens if an interface imports another interface.

            if not Processed_ALIs.Get (The_ALI) then
               Processed_ALIs.Set (The_ALI, True);
               Text := Read_Library_Info (The_ALI);

               if Text /= null then
                  Idread :=
                    Scan_ALI
                      (F          => The_ALI,
                       T          => Text,
                       Ignore_ED  => False,
                       Err        => True,
                       Read_Lines => "W");
                  Free (Text);

                  if Idread /= No_ALI_Id then
                     First_Unit := ALI.ALIs.Table (Idread).First_Unit;
                     Last_Unit  := ALI.ALIs.Table (Idread).Last_Unit;

                     --  Process both unit (spec and body) if the body is
                     --  needed by the spec (inline or generic). Otherwise,
                     --  just process the spec.

                     if First_Unit /= Last_Unit and then
                       not ALI.Units.Table (Last_Unit).Body_Needed_For_SAL
                     then
                        First_Unit := Last_Unit;
                     end if;

                     for Unit in First_Unit .. Last_Unit loop
                        Unit_Data := ALI.Units.Table (Unit);

                        --  Check if each withed unit which is in the library
                        --  is also in the interface set, if it has not yet
                        --  been processed.

                        for W in Unit_Data.First_With ..
                                 Unit_Data.Last_With
                        loop
                           Afile := Withs.Table (W).Afile;

                           if Afile /= No_File
                             and then Library_ALIs.Get (Afile)
                             and then not Processed_ALIs.Get (Afile)
                           then
                              if not Interface_ALIs.Get (Afile) then
                                 if not Interface_Incomplete then
                                    Put
                                      ("Error: In library project """);
                                    Get_Name_String (Proj.Name);
                                    To_Mixed (Name_Buffer (1 .. Name_Len));
                                    Put (Name_Buffer (1 .. Name_Len));
                                    Put_Line ("""");
                                    Interface_Incomplete := True;
                                 end if;

                                 Put ("         Unit """);
                                 Get_Name_String (Withs.Table (W).Uname);
                                 To_Mixed (Name_Buffer (1 .. Name_Len - 2));
                                 Put (Name_Buffer (1 .. Name_Len - 2));
                                 Put_Line (""" is not in the interface set");
                                 Put ("         but it is needed by ");

                                 case Unit_Data.Utype is
                                 when Is_Spec =>
                                    Put ("the spec of ");

                                 when Is_Body =>
                                    Put ("the body of ");

                                 when others =>
                                    null;
                                 end case;

                                 Put ("""");
                                 Get_Name_String (Unit_Data.Uname);
                                 To_Mixed (Name_Buffer (1 .. Name_Len - 2));
                                 Put (Name_Buffer (1 .. Name_Len - 2));
                                 Put_Line ("""");
                              end if;

                              --  Now, process this unit

                              Process_ALI (Afile, Proj);
                           end if;
                        end loop;
                     end loop;
                  end if;
               end if;
            end if;
         end Process_ALI;

         ------------------------
         -- Process_Standalone --
         ------------------------

         procedure Process_Standalone
           (Proj : Project_Id;
            Tree : Project_Tree_Ref)
         is
            Source : Source_Id;
            Iter   : Source_Iterator;

            List : String_List_Id;
            Elem : String_Element;
            OK   : Boolean;

         begin
            Processed_ALIs.Reset;
            Library_ALIs.Reset;
            Interface_ALIs.Reset;
            Interface_Incomplete := False;

            if Proj.Qualifier /= Aggregate_Library
              and then Proj.Extended_By = No_Project
            then
               Library_SAL_Projs.Append (Proj);
            end if;

            Iter := For_Each_Source (Project_Tree, Proj);
            loop
               Source := GPR.Element (Iter);
               exit when Source = No_Source;

               Change_To_Object_Directory (Source.Project);

               Initialize_Source_Record (Source);

               if Is_Compilable (Source)
                 and then Source.Replaced_By = No_Source
                 and then Source.Language.Config.Objects_Linked
                 and then
                   ((Source.Unit = No_Unit_Index
                     and then Source.Kind = Impl)
                    or else
                      (Source.Unit /= No_Unit_Index
                       and then (Source.Kind = Impl
                                 or else Other_Part (Source) = No_Source)
                       and then not Is_Subunit (Source)))
                 and then
                   (not Source.Project.Externally_Built
                    or else not For_Project.Externally_Built
                    or else Source.Project.Extended_By /= No_Project)
               then
                  if Source.Unit = No_Unit_Index then
                     OK := True;
                     Library_Objs.Append
                       ((Path  => Source.Object_Path,
                         TS    => Source.Object_TS,
                         Known => False));

                  else
                     Library_ALIs.Set (Source.Dep_Name, True);

                     --  Check if it is an interface and record if it is one

                     OK := False;
                     List := For_Project.Lib_Interface_ALIs;
                     while List /= Nil_String loop
                        Elem := Project_Tree.Shared.String_Elements.Table
                          (List);

                        if Elem.Value = Name_Id (Source.Dep_Name)
                        then
                           OK := True;
                           Library_Sources.Append (Source);
                           Interface_ALIs.Set (Source.Dep_Name, True);
                           exit;
                        end if;

                        List := Elem.Next;
                     end loop;
                  end if;

                  if OK and then Source.Object_TS = Empty_Time_Stamp then
                     Latest_Object_TS := Never;

                     if not Library_Needs_To_Be_Built then
                        Library_Needs_To_Be_Built := True;

                        if Opt.Verbosity_Level > Opt.Low then
                           Put ("      -> missing object file: ");
                           Get_Name_String (Source.Object);
                           Put_Line (Name_Buffer (1 .. Name_Len));
                        end if;
                     end if;

                  elsif OK and then Source.Object_TS > Latest_Object_TS then
                     Latest_Object_TS := Source.Object_TS;
                  end if;
               end if;

               Next (Iter);
            end loop;

            --  Only check the interface of the aggregate SAL, not those of
            --  the aggregated projects.

            if Proj = For_Project then

               --  Check if the interface set is complete

               declare
                  Iface : String_List_Id := Proj.Lib_Interface_ALIs;
                  ALI   : File_Name_Type;

               begin
                  while Iface /= Nil_String loop
                     ALI :=
                       File_Name_Type
                         (Tree.Shared.String_Elements.Table (Iface).Value);
                     Process_ALI (ALI, Proj);
                     Iface :=
                       Tree.Shared.String_Elements.Table (Iface).Next;
                  end loop;
               end;

               if Interface_Incomplete then
                  Fail_Program
                    (Project_Tree, "incomplete Stand-Alone Library interface");
               end if;
            end if;
         end Process_Standalone;

         -----------------
         -- Get_Closure --
         -----------------

         procedure Get_Closure is
            Index    : Natural := 0;
            The_ALI  : ALI.ALI_Id;
            Text     : Text_Buffer_Ptr;
            Dep_Path : Path_Name_Type;
            Dep_TS   : aliased File_Attributes := Unknown_Attributes;
            Sfile    : File_Name_Type;
            Afile    : File_Name_Type;
            Src_Id   : GPR.Source_Id;
            OK       : Boolean;
            Source   : Source_Id;

            procedure Add_To_Mapping
              (Source          : Source_Id;
               From_Object_Dir : Boolean);
            --  Add data for Source in binder mapping file. Use the ALI file
            --  in the library ALI directory if From_Object_Dir is False and
            --  the project is a library project. Otherwise, use the ALI file
            --  in the object directory.

            --------------------
            -- Add_To_Mapping --
            --------------------

            procedure Add_To_Mapping
              (Source : Source_Id;
               From_Object_Dir : Boolean)
            is
               Unit : Unit_Index;

               ALI_Unit : Unit_Name_Type := No_Unit_Name;
               --  The unit name of an ALI file

               ALI_Name : File_Name_Type := No_File;
               --  The file name of the ALI file

               ALI_Project : Project_Id := No_Project;
               --  The project of the ALI file

            begin
               if Source = No_Source then
                  return;
               end if;

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

               --  If we have something to put in the mapping then do it now.
               --  If the project is extended, look for the ALI file in the
               --  project, then in the extending projects in order, and use
               --  the last one found.

               if ALI_Name /= No_File then

                  --  Look in the project and the projects that are extending
                  --  it to find the real ALI

                  declare
                     ALI      : constant String := Get_Name_String (ALI_Name);
                     ALI_Path : Name_Id         := No_Name;
                     Bytes    : Integer;
                     pragma Unreferenced (Bytes);

                  begin
                     loop
                        --  For library projects, use the library ALI
                        --  directory, for other projects, use the
                        --  object directory.

                        if ALI_Project.Library and then not From_Object_Dir
                        then
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

                        --  Second line is the ALI file name

                        Get_Name_String (ALI_Name);
                        Add_Char_To_Name_Buffer (ASCII.LF);
                        Bytes :=
                          Write
                            (Mapping_FD,
                             Name_Buffer (1)'Address,
                             Name_Len);

                        --  Third line is the ALI path name

                        Get_Name_String (ALI_Path);
                        Add_Char_To_Name_Buffer (ASCII.LF);
                        Bytes :=
                          Write
                            (Mapping_FD,
                             Name_Buffer (1)'Address,
                             Name_Len);
                     end if;
                  end;
               end if;
            end Add_To_Mapping;

            From_Object_Dir : Boolean;

         --  Start of processing for Get_Closure

         begin
            Closure_Sources.Init;
            for J in 1 .. Library_Sources.Last loop
               Closure_Sources.Append (Library_Sources.Table (J));
            end loop;

            while Index < Closure_Sources.Last loop
               From_Object_Dir := False;
               Index := Index + 1;
               Source := Closure_Sources.Table (Index);

               for J in 1 .. Library_Sources.Last loop
                  if Source = Library_Sources.Table (J) then
                     From_Object_Dir := True;
                     exit;
                  end if;
               end loop;

               Add_To_Mapping (Source, From_Object_Dir);
               Dep_Path := Source.Dep_Path;
               Dep_TS   := Source.Dep_TS;
               Text := Read_Library_Info_From_Full
                 (File_Name_Type (Dep_Path), Dep_TS'Access);

               if Text /= null then
                  The_ALI :=
                    ALI.Scan_ALI
                      (File_Name_Type (Dep_Path),
                       Text,
                       Ignore_ED     => False,
                       Err           => True,
                       Read_Lines    => "W");
                  Free (Text);

                  --  Get the withed sources

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

                           if Src_Id /= No_Source then
                              declare
                                 Proj : Project_Id := Src_Id.Project;
                                 New_In_Closure : Boolean := True;
                              begin
                                 OK := False;

                                 while Proj.Extended_By /= No_Project loop
                                    Proj := Proj.Extended_By;
                                 end loop;

                                 for J in 1 .. Library_SAL_Projs.Last loop
                                    if Proj = Library_SAL_Projs.Table (J) then
                                       OK := True;
                                       exit;
                                    end if;
                                 end loop;

                                 for J in 1 .. Closure_Sources.Last loop
                                    if Closure_Sources.Table (J) = Src_Id
                                    then
                                       New_In_Closure := False;
                                       exit;
                                    end if;
                                 end loop;

                                 if New_In_Closure then
                                    Closure_Sources.Append (Src_Id);
                                 end if;

                                 if OK then
                                    for J in 1 .. Library_Sources.Last loop
                                       if Src_Id = Library_Sources.Table (J)
                                       then
                                          OK := False;
                                          exit;
                                       end if;
                                    end loop;

                                    if OK then
                                       Library_Sources.Append (Src_Id);
                                       Initialize_Source_Record (Src_Id);

                                       if Src_Id.Object_TS = Empty_Time_Stamp
                                       then
                                          Latest_Object_TS := Never;

                                          if not Library_Needs_To_Be_Built then
                                             Library_Needs_To_Be_Built := True;

                                             if Opt.Verbosity_Level > Opt.Low
                                             then
                                                Put
                                                  ("      ->" &
                                                   "missing object file: ");
                                                Get_Name_String
                                                  (Src_Id.Object);
                                                Put_Line
                                                  (Name_Buffer
                                                     (1 .. Name_Len));
                                             end if;
                                          end if;

                                       elsif Src_Id.Object_TS >
                                               Latest_Object_TS
                                       then
                                          Latest_Object_TS := Src_Id.Object_TS;
                                       end if;
                                    end if;
                                 end if;
                              end;
                           end if;
                        end if;
                     end loop;
                  end loop;
               end if;
            end loop;
         end Get_Closure;

         procedure Process_Non_Standalone_Aggregate_Library is
           new For_Project_And_Aggregated (Process);

         procedure Process_Standalone_Aggregate_Library is
           new For_Project_And_Aggregated (Process_Standalone);

         Proj : Project_Id := For_Project;

      --  Start of processing of Get_Objects

      begin
         Library_Objs.Init;
         Library_Sources.Init;
         Library_Projs.Init;
         Library_SAL_Projs.Init;

         if For_Project.Qualifier = Aggregate_Library then
            if For_Project.Standalone_Library = No then
               Process_Non_Standalone_Aggregate_Library
                 (For_Project, Project_Tree);
            else
               Process_Standalone_Aggregate_Library
                 (For_Project, Project_Tree);
            end if;

         else
            while Proj /= No_Project loop
               if For_Project.Standalone_Library = No then
                  Process (Proj, Project_Tree);
               else
                  Process_Standalone (Proj, Project_Tree);
               end if;

               Proj := Proj.Extends;
            end loop;
         end if;

         if For_Project.Standalone_Library /= No then
            --  Create the binder maping file

            Tempdir.Create_Temp_File (Mapping_FD, Mapping_Path);
            Record_Temp_File (Project_Tree.Shared, Mapping_Path);

            Get_Closure;

            Close (Mapping_FD);

            --  Put all the object files in the closure in Library_Objs

            for J in 1 .. Library_Sources.Last loop
               Source := Library_Sources.Table (J);
               Library_Objs.Append
                 ((Path  => Source.Object_Path,
                   TS    => Source.Object_TS,
                   Known => False));
            end loop;
         end if;
      end Get_Objects;

      ------------------------
      -- Write_Object_Files --
      ------------------------

      procedure Write_Object_Files is
      begin
         if Library_Objs.Last > 0 then
            Put_Line (Exchange_File, Library_Label (Object_Files));

            for J in 1 .. Library_Objs.Last loop
               Put_Line
                 (Exchange_File,
                  Get_Name_String (Library_Objs.Table (J).Path));
            end loop;
         end if;
      end Write_Object_Files;

      ----------------------------
      -- Write_Object_Directory --
      ----------------------------

      procedure Write_Object_Directory is

         package Object_Projects is new GNAT.Table
           (Table_Component_Type => Project_Id,
            Table_Index_Type     => Integer,
            Table_Low_Bound      => 1,
            Table_Initial        => 10,
            Table_Increment      => 100);
         --  The projects that have already be found when looking for object
         --  directories.

         package Object_Directories is new GNAT.HTable.Simple_HTable
           (Header_Num => GPR.Header_Num,
            Element    => Boolean,
            No_Element => False,
            Key        => Path_Name_Type,
            Hash       => Hash,
            Equal      => "=");
         --  The object directories that have already be found

         Prj : Project_Id;

         procedure Get_Object_Projects (Prj : Project_Id);
         --  Recursive procedure to collect the aggregated projects

         function Is_In_Object_Projects (Prj : Project_Id) return Boolean;
         --  Returns True iff Prj is in table Object_Projects

         function Is_In_Object_Directories
           (Dir : Path_Name_Type)
            return Boolean;
         --  Returns True iff Dir is in table Object_Directories

         -------------------------
         -- Get_Object_Projects --
         -------------------------

         procedure Get_Object_Projects (Prj : Project_Id) is
         begin
            if Prj.Qualifier = Aggregate_Library then
               declare
                  List : Aggregated_Project_List := Prj.Aggregated_Projects;
               begin
                  while List /= null loop
                     Get_Object_Projects (List.Project);
                     List := List.Next;
                  end loop;
               end;
            else
               --  Add object directories of the project and of the projects it
               --  extends, if any.

               declare
                  Proj : Project_Id := Prj;
               begin
                  while Proj /= No_Project loop
                     if not Is_In_Object_Projects (Proj) then
                        Object_Projects.Append (Proj);

                        if Proj.Object_Directory /= No_Path_Information
                          and then not Is_In_Object_Directories
                            (Proj.Object_Directory.Display_Name)
                        then
                           Object_Directories.Set
                             (Proj.Object_Directory.Display_Name, True);
                           Put_Line
                             (Exchange_File,
                              Get_Name_String
                                (Proj.Object_Directory.Display_Name));
                        end if;
                     end if;

                     Proj := Proj.Extends;
                  end loop;
               end;
            end if;
         end Get_Object_Projects;

         ------------------------------
         -- Is_In_Object_Directories --
         ------------------------------

         function Is_In_Object_Directories
           (Dir : Path_Name_Type)
            return Boolean
         is
         begin
            return Object_Directories.Get (Dir);
         end Is_In_Object_Directories;

         ---------------------------
         -- Is_In_Object_Projects --
         ---------------------------

         function Is_In_Object_Projects (Prj : Project_Id) return Boolean is
         begin
            for J in 1 .. Object_Projects.Last loop
               if Prj = Object_Projects.Table (J) then
                  return True;
               end if;
            end loop;

            return False;
         end Is_In_Object_Projects;

      --  Start of processing for Write_Object_Directory

      begin
         Object_Projects.Init;
         Object_Directories.Reset;
         Put_Line (Exchange_File, Library_Label (Object_Directory));
         Get_Object_Projects (For_Project);

         for J in 1 .. Object_Projects.Last loop
            Prj := Object_Projects.Table (J);

            --  Add object directories of imported non library projects

            Process_Imported_Non_Libraries (Prj);

            declare
               Proj : Project_Id;
            begin
               for J in 1 .. Non_Library_Projs.Last loop
                  Proj := Non_Library_Projs.Table (J);
                  Get_Object_Projects (Proj);
               end loop;
            end;

            --  Add ALI dir directories of imported projects (only if it
            --  is not an externally built project or if the project has
            --  sources). This skip the library projects with no sources
            --  used for example to add a system library to the linker.

            declare
               List : Project_List := Prj.All_Imported_Projects;
            begin
               while List /= null loop
                  if not Is_In_Object_Projects (List.Project) and then
                     (not List.Project.Externally_Built
                      or else List.Project.Source_Dirs /= Nil_String)
                  then
                     if List.Project.Library_ALI_Dir /= No_Path_Information
                     then
                        Put_Line
                          (Exchange_File,
                           Get_Name_String
                             (List.Project.Library_ALI_Dir.Display_Name));

                     elsif List.Project.Library_Dir /= No_Path_Information
                     then
                        Put_Line
                          (Exchange_File,
                           Get_Name_String
                             (List.Project.Library_Dir.Display_Name));
                     end if;
                  end if;

                  List := List.Next;
               end loop;
            end;
         end loop;
      end Write_Object_Directory;

      ---------------------
      -- Write_Compilers --
      ---------------------

      procedure Write_Compilers is

         procedure Compilers_For
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Dummy   : in out Boolean);
         --  Write compilers for the given project

         Dummy     : Boolean := True;
         Lang_Seen : Lang_Set.Set;

         -------------------
         -- Compilers_For --
         -------------------

         procedure Compilers_For
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Dummy   : in out Boolean)
         is
            pragma Unreferenced (Tree, Dummy);
            Lang     : Language_Ptr := Project.Languages;
            Compiler : String_Access;
         begin
            --  Exchange file, Compilers section

            while Lang /= No_Language_Index loop
               if not Lang_Seen.Contains (Lang.Name) then
                  Lang_Seen.Insert (Lang.Name);

                  Compiler := Get_Compiler_Driver_Path (Project_Tree, Lang);
                  if Compiler /= null then
                     Put_Line (Exchange_File, Get_Name_String (Lang.Name));
                     Put_Line (Exchange_File, Compiler.all);

                  elsif Lang.Config.Compiler_Driver /= No_File then
                     Put_Line (Exchange_File, Get_Name_String (Lang.Name));
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (Lang.Config.Compiler_Driver));
                  end if;
               end if;

               Lang := Lang.Next;
            end loop;
         end Compilers_For;

         procedure For_Imported is
           new For_Every_Project_Imported (Boolean, Compilers_For);

      --  Start of processing for Write_Compilers

      begin
         Put_Line (Exchange_File, Library_Label (Compilers));

         Compilers_For (For_Project, Project_Tree, Dummy);

         if For_Project.Qualifier = Aggregate_Library then
            For_Imported (For_Project, Project_Tree, Dummy);
         end if;
      end Write_Compilers;

      -------------------------------------
      -- Write_Compiler_Leading_Switches --
      -------------------------------------

      procedure Write_Compiler_Leading_Switches is

         procedure Compiler_Leading_Switches_For
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Dummy   : in out Boolean);
         --  Write compilers for the given project

         Dummy     : Boolean := True;
         Lang_Seen : Lang_Set.Set;

         -----------------------------------
         -- Compiler_Leading_Switches_For --
         -----------------------------------

         procedure Compiler_Leading_Switches_For
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Dummy   : in out Boolean)
         is
            pragma Unreferenced (Tree, Dummy);
            Lang : Language_Ptr := Project.Languages;
            Indx : Name_List_Index;
            Node : Name_Node;

         begin
            while Lang /= No_Language_Index loop
               if not Lang_Seen.Contains (Lang.Name) then
                  Lang_Seen.Insert (Lang.Name);
                  Indx := Lang.Config.Compiler_Leading_Required_Switches;

                  if Indx /= No_Name_List then
                     Put_Line
                       (Exchange_File,
                        "language=" & Get_Name_String (Lang.Name));

                     while Indx /= No_Name_List loop
                        Node := Project_Tree.Shared.Name_Lists.Table (Indx);
                        Put_Line (Exchange_File, Get_Name_String (Node.Name));
                        Indx := Node.Next;
                     end loop;
                  end if;
               end if;

               Lang := Lang.Next;
            end loop;
         end Compiler_Leading_Switches_For;

         procedure For_Imported is new For_Every_Project_Imported
           (Boolean, Compiler_Leading_Switches_For);

      --  Start of processing for Write_Compiler_Leading_Switches

      begin
         Put_Line (Exchange_File, Library_Label (Compiler_Leading_Switches));

         Compiler_Leading_Switches_For (For_Project, Project_Tree, Dummy);

         if For_Project.Qualifier = Aggregate_Library then
            For_Imported (For_Project, Project_Tree, Dummy);
         end if;
      end Write_Compiler_Leading_Switches;

      --------------------------------------
      -- Write_Compiler_Trailing_Switches --
      --------------------------------------

      procedure Write_Compiler_Trailing_Switches is

         procedure Compiler_Trailing_Switches_For
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Dummy   : in out Boolean);
         --  Write compilers for the given project

         Dummy     : Boolean := True;
         Lang_Seen : Lang_Set.Set;

         ------------------------------------
         -- Compiler_Trailing_Switches_For --
         ------------------------------------

         procedure Compiler_Trailing_Switches_For
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Dummy   : in out Boolean)
         is
            pragma Unreferenced (Tree, Dummy);
            Lang : Language_Ptr := Project.Languages;
            Indx : Name_List_Index;
            Node : Name_Node;
         begin
            while Lang /= No_Language_Index loop
               if not Lang_Seen.Contains (Lang.Name) then
                  Lang_Seen.Insert (Lang.Name);
                  Indx := Lang.Config.Compiler_Trailing_Required_Switches;

                  if Indx /= No_Name_List then
                     Put_Line
                       (Exchange_File,
                        "language=" & Get_Name_String (Lang.Name));

                     while Indx /= No_Name_List loop
                        Node := Project_Tree.Shared.Name_Lists.Table (Indx);
                        Put_Line (Exchange_File, Get_Name_String (Node.Name));
                        Indx := Node.Next;
                     end loop;
                  end if;
               end if;

               Lang := Lang.Next;
            end loop;
         end Compiler_Trailing_Switches_For;

         procedure For_Imported is new For_Every_Project_Imported
           (Boolean, Compiler_Trailing_Switches_For);

      --  Start of processing for Write_Compiler_Trailing_Switches

      begin
         Put_Line
           (Exchange_File, Library_Label (Compiler_Trailing_Switches));

         Compiler_Trailing_Switches_For (For_Project, Project_Tree, Dummy);

         if For_Project.Qualifier = Aggregate_Library then
            For_Imported (For_Project, Project_Tree, Dummy);
         end if;
      end Write_Compiler_Trailing_Switches;

      ----------------
      -- Write_List --
      ----------------

      procedure Write_List
        (File  : Text_IO.File_Type;
         Label : Library_Section;
         List  : String_List_Id)
      is
         Current      : String_List_Id := List;
         Element      : String_Element;
         Output_Label : Boolean := True;
      begin
         while Current /= Nil_String loop
            Element :=
              Project_Tree.Shared.String_Elements.Table (Current);
            Get_Name_String (Element.Value);

            if Name_Len /= 0 then
               if Output_Label and then Current_Section /= Label then
                  Put_Line (File, Library_Label (Label));
                  Output_Label := False;
                  Current_Section := Label;
               end if;

               Put_Line (File, Name_Buffer (1 .. Name_Len));
            end if;

            Current := Element.Next;
         end loop;
      end Write_List;

      ---------------------
      -- Write_Name_List --
      ---------------------

      procedure Write_Name_List
        (File  : Text_IO.File_Type;
         Label : Library_Section;
         List  : Name_List_Index)
      is
         Current : Name_List_Index := List;
         Nam     : Name_Node;
      begin
         if List /= No_Name_List then
            if Current_Section /= Label then
               Put_Line (File, Library_Label (Label));
               Current_Section := Label;
            end if;

            while Current /= No_Name_List loop
               Nam := Project_Tree.Shared.Name_Lists.Table (Current);
               Put_Line (File, Get_Name_String (Nam.Name));
               Current := Nam.Next;
            end loop;
         end if;
      end Write_Name_List;

      --------------------------
      -- Write_Partial_Linker --
      --------------------------

      procedure Write_Partial_Linker is
         List : constant Name_List_Index :=
                  For_Project.Config.Lib_Partial_Linker;
      begin
         if List /= No_Name_List then
            Write_Name_List (Exchange_File, Partial_Linker, List);
         end if;
      end Write_Partial_Linker;

      --------------------------------------
      -- Write_Shared_Lib_Minimum_Options --
      --------------------------------------

      procedure Write_Shared_Lib_Minimum_Options is
         Library_Options : Variable_Value := Nil_Variable_Value;
      begin
         --  Output the minimal options to build a shared library (standard
         --  or encapsulated).

         if For_Project.Standalone_Library = Encapsulated then
            Library_Options :=
              Value_Of
              (Name_Library_Encapsulated_Options,
               For_Project.Decl.Attributes, Project_Tree.Shared);

            if not Library_Options.Default then
               Write_List
                 (Exchange_File,
                  Gprexch.Shared_Lib_Minimum_Options,
                  Library_Options.Values);
            end if;

         else
            Write_Name_List
              (Exchange_File,
               Shared_Lib_Minimum_Options,
               For_Project.Config.Shared_Lib_Min_Options);
         end if;
      end Write_Shared_Lib_Minimum_Options;

      ---------------------------
      -- Write_Library_Version --
      ---------------------------

      procedure Write_Library_Version is
         List : constant Name_List_Index :=
                  For_Project.Config.Lib_Version_Options;
      begin
         if List /= No_Name_List then
            Write_Name_List (Exchange_File, Library_Version_Options, List);
         end if;
      end Write_Library_Version;

      -------------------------------
      -- Write_Runtime_Library_Dir --
      -------------------------------

      procedure Write_Runtime_Library_Dir is

         use type Ada.Containers.Count_Type;

         procedure RTL_For
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Dummy   : in out Boolean);
         --  Write runtime libraries for the given project

         Dummy     : Boolean := True;
         Lang_Seen : Lang_Set.Set;

         -------------
         -- RTL_For --
         -------------

         procedure RTL_For
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Dummy   : in out Boolean)
         is
            pragma Unreferenced (Tree, Dummy);
            List : Language_Ptr := Project.Languages;
            Lib_Dirs : Name_List_Index;
            Nam_Nod : Name_Node;
         begin
            while List /= No_Language_Index loop
               if List.Config.Runtime_Library_Dirs /= No_Name_List then
                  Lib_Dirs := List.Config.Runtime_Library_Dirs;

                  while Lib_Dirs /= No_Name_List loop
                     Nam_Nod :=
                       Project_Tree.Shared.Name_Lists.Table (Lib_Dirs);
                     if not Lang_Seen.Contains (Nam_Nod.Name)
                     then
                        if Lang_Seen.Length = 0 then
                           Put_Line
                             (Exchange_File,
                              Library_Label (Runtime_Library_Dir));
                        end if;
                        Lang_Seen.Insert (Nam_Nod.Name);

                        Put_Line (Exchange_File, Get_Name_String (List.Name));
                        Put_Line
                          (Exchange_File, Get_Name_String (Nam_Nod.Name));
                     end if;

                     Lib_Dirs := Nam_Nod.Next;
                  end loop;
               end if;

               List := List.Next;
            end loop;
         end RTL_For;

         procedure For_Imported is
           new For_Every_Project_Imported (Boolean, RTL_For);

      --  Start of processing for Write_Runtime_Library_Dir

      begin
         RTL_For (For_Project, Project_Tree, Dummy);

         if For_Project.Qualifier = Aggregate_Library then
            For_Imported (For_Project, Project_Tree, Dummy);
         end if;
      end Write_Runtime_Library_Dir;

      ---------------------
      -- Write_Auto_Init --
      ---------------------

      procedure Write_Auto_Init is
      begin
         if For_Project.Standalone_Library /= No then
            if For_Project.Lib_Auto_Init then
               Put_Line (Exchange_File, Library_Label (Auto_Init));
            end if;

            declare
               Binder_Package : constant Package_Id :=
                                  Value_Of
                                    (Name        => Name_Binder,
                                     In_Packages =>
                                       For_Project.Decl.Packages,
                                     Shared      => Project_Tree.Shared);

            begin
               if Binder_Package /= No_Package then
                  declare
                     Defaults : constant Array_Element_Id :=
                                  Value_Of
                                    (Name      => Name_Default_Switches,
                                     In_Arrays =>
                                       Project_Tree.Shared.Packages.Table
                                         (Binder_Package).Decl.Arrays,
                                     Shared    => Project_Tree.Shared);
                     Switches : Variable_Value := Nil_Variable_Value;

                  begin
                     if Defaults /= No_Array_Element then
                        Switches :=
                          Value_Of
                            (Index     => Name_Ada,
                             Src_Index => 0,
                             In_Array  => Defaults,
                             Shared    => Project_Tree.Shared);

                        if not Switches.Default then
                           Write_List
                             (Exchange_File, Gprexch.Binding_Options,
                              Switches.Values);
                        end if;
                     end if;
                  end;
               end if;
            end;
         end if;
      end Write_Auto_Init;

      ---------------------------
      -- Write_Run_Path_Option --
      ---------------------------

      procedure Write_Run_Path_Option is
         List : constant Name_List_Index :=
                  For_Project.Config.Run_Path_Option;
      begin
         if Opt.Run_Path_Option and then List /= No_Name_List then
            Write_Name_List (Exchange_File, Run_Path_Option, List);

            if For_Project.Config.Separate_Run_Path_Options then
               Put_Line
                 (Exchange_File,
                  Library_Label (Gprexch.Separate_Run_Path_Options));
            end if;
         end if;
      end Write_Run_Path_Option;

      -----------------------------------
      -- Write_Leading_Library_Options --
      -----------------------------------

      procedure Write_Leading_Library_Options is
         Leading_Library_Options : Variable_Value := Nil_Variable_Value;
      begin
         --  If attribute Leading_Library_Options was specified, add these
         --  additional options.

         Leading_Library_Options :=
           Value_Of
           (Name_Leading_Library_Options,
            For_Project.Decl.Attributes, Project_Tree.Shared);

         if not Leading_Library_Options.Default then
            Write_List
              (Exchange_File,
               Gprexch.Leading_Library_Options,
               Leading_Library_Options.Values);
         end if;
      end Write_Leading_Library_Options;

      ---------------------------
      -- Write_Library_Options --
      ---------------------------

      procedure Write_Library_Options (Success : out Boolean) is

         procedure Write_All_Linker_Options (Project : Project_Id);
         --  Write all linker options for all project imported by Project. This
         --  is called only when building an encapsulated library.

         ------------------------------
         -- Write_All_Linker_Options --
         ------------------------------

         procedure Write_All_Linker_Options (Project : Project_Id) is
            Section_Out : Boolean := Current_Section = Gprexch.Library_Options;
            L           : Project_List := Project.All_Imported_Projects;
         begin
            --  For all imported projects

            while L /= null loop
               Check_Package : declare
                  P              : constant Project_Id := L.Project;
                  Linker_Package : constant Package_Id :=
                                     Value_Of
                                       (Name        => Name_Linker,
                                        In_Packages => P.Decl.Packages,
                                        Shared      => Project_Tree.Shared);
               begin
                  --  Check linker package for a definition of Linker_Options

                  if Linker_Package /= No_Package then
                     Check_Attribute : declare
                        Opts : constant Variable_Value :=
                                 Value_Of
                                   (Variable_Name     => Name_Linker_Options,
                                    In_Variables =>
                                      Project_Tree.Shared.Packages.Table
                                        (Linker_Package).Decl.Attributes,
                                    Shared       => Project_Tree.Shared);

                     begin
                        --  If a Linker_Options attribute is found, output it
                        --  into the Library_Options section.

                        if not Opts.Default then
                           Output_Options : declare
                              List : String_List_Id := Opts.Values;
                              Elem : String_Element;
                           begin
                              while List /= Nil_String loop
                                 Elem :=
                                   Project_Tree.Shared.
                                     String_Elements.Table (List);

                                 --  First ensure the section is opended

                                 if not Section_Out then
                                    Put_Line
                                      (Exchange_File,
                                       Library_Label
                                         (Gprexch.Library_Options));
                                    Section_Out := True;
                                 end if;

                                 Put_Line
                                   (Exchange_File,
                                    "-L"
                                    & Get_Name_String (P.Library_Dir.Name));
                                 Put_Line
                                   (Exchange_File,
                                    Get_Name_String (Elem.Value));

                                 List := Elem.Next;
                              end loop;
                           end Output_Options;
                        end if;
                     end Check_Attribute;
                  end if;
               end Check_Package;

               L := L.Next;
            end loop;
         end Write_All_Linker_Options;

         Library_Options : Variable_Value := Nil_Variable_Value;

      --  Start of processing for Write_Library_Options

      begin
         Success := True;

         --  If attribute Library_Options was specified, add these
         --  additional options.

         Library_Options :=
           Value_Of
           (Name_Library_Options,
            For_Project.Decl.Attributes, Project_Tree.Shared);

         if not Library_Options.Default then
            Write_List
              (Exchange_File,
               Gprexch.Library_Options, Library_Options.Values);

            --  For static libraries, check that the library options are
            --  existing object files.

            if For_Project.Library_Kind = Static or else
              For_Project.Library_Kind = Static_Pic
            then
               declare
                  List : String_List_Id := Library_Options.Values;
                  Elem : String_Element;
                  OK   : Boolean;
               begin
                  while List /= Nil_String loop
                     Elem := Project_Tree.Shared.String_Elements.Table (List);
                     Get_Name_String (Elem.Value);

                     if Is_Absolute_Path (Name_Buffer (1 .. Name_Len)) then
                        OK := Is_Regular_File (Name_Buffer (1 .. Name_Len));

                     else
                        OK := Is_Regular_File
                          (Get_Name_String (For_Project.Directory.Name) &
                          Directory_Separator &
                          Name_Buffer (1 .. Name_Len));
                     end if;

                     if not OK then
                        Error_Msg
                          (Msg           => "unknown object file " &
                                             Name_Buffer (1 .. Name_Len),
                           Flag_Location => Library_Options.Location);
                        Success := False;
                     end if;

                     List := Elem.Next;
                  end loop;
               end;
            end if;
         end if;

         --  For encapsulated libraries we also want to add the Linker_Options
         --  for all imported projects.

         if For_Project.Standalone_Library = Encapsulated then
            Write_All_Linker_Options (For_Project);
         end if;
      end Write_Library_Options;

      ---------------------------------
      -- Write_Library_Rpath_Options --
      ---------------------------------

      procedure Write_Library_Rpath_Options is

         procedure Add_Language (Lang : Language_Ptr);
         --  Add language Name in array Langs if not already there

         procedure Find_Languages
           (Project    : Project_Id;
            Tree       : Project_Tree_Ref;
            With_State : in out Boolean);
         --  Find the languages of a project

         procedure Find_All_Languages is new
           For_Every_Project_Imported (Boolean, Find_Languages);

         procedure Get_Directory;
         --  Get a directory for one language

         procedure Get_Languages;
         --  Put in Langs the languages of the project tree rooted at project
         --  For_Project.

         ------------------
         -- Add_Language --
         ------------------

         procedure Add_Language (Lang : Language_Ptr) is
         begin

            --  Only add a language if it is not already in the list

            for J in 1 .. Last_Lang loop
               if Lang.Name = Langs (J).Name then
                  return;
               end if;
            end loop;

            --  Double array Langs if already full

            if Last_Lang = Langs'Last then
               declare
                  New_Langs : constant Lang_Names_Ptr :=
                    new Lang_Names (1 .. 2 * Langs'Length);

               begin
                  New_Langs (Langs'Range) := Langs.all;
                  Langs := New_Langs;
               end;
            end if;

            Last_Lang := Last_Lang + 1;
            Langs (Last_Lang) := Lang;
         end Add_Language;

         --------------------
         -- Find_Languages --
         --------------------

         procedure Find_Languages
           (Project    : Project_Id;
            Tree       : Project_Tree_Ref;
            With_State : in out Boolean)
         is
            pragma Unreferenced (Tree);
            pragma Unreferenced (With_State);

            Lang : Language_Ptr := Project.Languages;

         begin
            while Lang /= No_Language_Index loop
               Add_Language (Lang);
               Lang := Lang.Next;
            end loop;
         end Find_Languages;

         -------------------
         -- Get_Languages --
         -------------------

         procedure Get_Languages is
            OK : Boolean := True;
         begin
            Last_Lang := 0;

            Find_Languages (For_Project, Project_Tree, OK);

            Find_All_Languages
              (By                 => For_Project,
               Tree               => Project_Tree,
               With_State         => OK,
               Include_Aggregated => False);
         end Get_Languages;

         List : Array_Element_Id;
         Elem : Array_Element;
         Label_Issued : Boolean := False;
         Lang_Index : Natural;
         Lang_Ptr   : Language_Ptr;

         Opt_List : String_List_Id;
         Opt_Elem : String_Element;

         -------------------
         -- Get_Directory --
         -------------------

         procedure Get_Directory is
            Opt_Nmb : Natural := 0;

            Args : Argument_List_Access;
            FD : File_Descriptor;
            Pname : Path_Name_Type;
            Return_Code : Integer;
            pragma Unreferenced (Return_Code);

            File : Text_File;
            Line : String (1 .. 1000);
            Last : Natural;

            Disregard : Boolean;
            pragma Unreferenced (Disregard);

         begin
            --  Check that the compiler driver exists

            if Lang_Ptr.Config.Compiler_Driver_Path = null then
               Lang_Ptr.Config.Compiler_Driver_Path :=
                 Locate_Exec_On_Path
                   (Exec_Name =>
                      Get_Name_String (Lang_Ptr.Config.Compiler_Driver));
            end if;

            if Lang_Ptr.Config.Compiler_Driver_Path /= null then

               --  Count the options

               while Opt_List /= Nil_String loop
                  Opt_Elem :=
                    Project_Tree.Shared.String_Elements.Table (Opt_List);
                  Opt_Nmb := Opt_Nmb + 1;
                  Opt_List := Opt_Elem.Next;
               end loop;

               Args := new Argument_List (1 .. Opt_Nmb);

               --  Put the options in Args

               Opt_Nmb := 0;
               Opt_List := Elem.Value.Values;

               while Opt_List /= Nil_String loop
                  Opt_Elem :=
                    Project_Tree.Shared.String_Elements.Table (Opt_List);
                  Opt_Nmb := Opt_Nmb + 1;
                  Args (Opt_Nmb) :=
                    new String'(Get_Name_String (Opt_Elem.Value));
                  Opt_List := Opt_Elem.Next;
               end loop;

               --  Create a temporary file and invoke the compiler with the
               --  options redirecting the output to this temporary file.

               Tempdir.Create_Temp_File (FD, Pname);
               Spawn
                 (Program_Name =>
                    Lang_Ptr.Config.Compiler_Driver_Path.all,
                  Args => Args.all,
                  Output_File_Descriptor => FD,
                  Return_Code => Return_Code);
               Close (FD);
               Free (Args);

               --  Now read the temporary file and get the first non empty
               --  line, if any.

               Open (File, Get_Name_String (Pname));

               if Is_Valid (File) then
                  Last := 0;
                  while not End_Of_File (File) loop
                     Get_Line (File, Line, Last);
                     exit when Last > 0;
                  end loop;

                  --  Get the directory name of the path

                  if Last /= 0 then
                     declare
                        Dir : constant String :=
                          Dir_Name (Normalize_Pathname (Line (1 .. Last)));

                     begin

                        --  If it is in fact a directory, put it in the
                        --  exchange file.

                        if Is_Directory (Dir) then
                           if not Label_Issued then
                              Put_Line
                                (Exchange_File,
                                 Library_Label
                                   (Gprexch.Library_Rpath_Options));
                              Label_Issued := True;
                           end if;

                           Put_Line (Exchange_File, Dir);
                        end if;
                     end;
                  end if;
               end if;

               if Is_Valid (File) then
                  Close (File);
               end if;

               --  Delete the temporary file, if gprbuild was not invoked
               --  with -dn.

               if not Debug_Flag_N then
                  Delete_File (Get_Name_String (Pname), Disregard);
               end if;
            end if;
         end Get_Directory;

      --  Start of processing for Write_Library_Rpath_Options

      begin
         if Opt.Run_Path_Option
           and then For_Project.Config.Run_Path_Option /= No_Name_List
         then
            List :=
              Value_Of
                (Name_Library_Rpath_Options,
                 For_Project.Decl.Arrays, Project_Tree.Shared);

            if List /= No_Array_Element then
               Get_Languages;

               while Last_Lang /= 0 and then List /= No_Array_Element loop
                  Elem := Project_Tree.Shared.Array_Elements.Table (List);
                  Lang_Index := 0;

                  for J in 1 .. Last_Lang loop
                     if Elem.Index = Langs (J).Name then
                        Lang_Index := J;
                        exit;
                     end if;
                  end loop;

                  if Lang_Index /= 0 then
                     Lang_Ptr := Langs (Lang_Index);

                     --  Remove language from the list so that rpath options
                     --  are not looked for twice for the same language.

                     Langs (Lang_Index .. Last_Lang - 1) :=
                       Langs (Lang_Index + 1 .. Last_Lang);
                     Last_Lang := Last_Lang - 1;

                     --  Invoke the compiler for the language, followed by
                     --  the options and put the result into a temporary file.

                     Opt_List := Elem.Value.Values;

                     --  Nothing to do if there is no options

                     if Opt_List /= Nil_String then
                           Get_Directory;
                     end if;
                  end if;

                  List := Elem.Next;
               end loop;
            end if;
         end if;
      end Write_Library_Rpath_Options;

      ------------------------------
      -- Write_Imported_Libraries --
      ------------------------------

      procedure Write_Imported_Libraries is
      begin
         --  If there are imported libraries, put their data in the exchange
         --  file.

         if Library_Projs.Last > 0 then
            Put_Line (Exchange_File, Library_Label (Imported_Libraries));

            for J in reverse 1 .. Library_Projs.Last loop
               if For_Project.Qualifier /= Aggregate_Library
                 or else Library_Projs.Table (J).Proj.Externally_Built
               then
                  Put_Line
                    (Exchange_File,
                     Get_Name_String
                       (Library_Projs.Table (J).
                            Proj.Library_Dir.Display_Name));
                  Put_Line
                    (Exchange_File,
                     Get_Name_String
                       (Library_Projs.Table (J).Proj.Library_Name));
               end if;
            end loop;
         end if;
      end Write_Imported_Libraries;

      ----------------------------
      -- Write_Dependency_Files --
      ----------------------------

      procedure Write_Dependency_Files is

         procedure Process (Proj : Project_Id; Tree : Project_Tree_Ref);

         procedure Add (Source : Source_Id);

         ---------
         -- Add --
         ---------

         procedure Add (Source : Source_Id) is
         begin
            if Source.Unit = No_Unit_Index
              or else For_Project.Standalone_Library = No
            then
               Add_Dep (Get_Name_String (Source.Dep_Path));
            end if;
         end Add;

         -------------
         -- Process --
         -------------

         procedure Process (Proj : Project_Id; Tree : Project_Tree_Ref) is
            pragma Unreferenced (Tree);
            Current_Proj : Project_Id := Proj;
            Source       : Source_Id;

         begin
            while Current_Proj /= No_Project loop
               declare
                  Iter : Source_Iterator;
               begin
                  Iter := For_Each_Source (Project_Tree, Current_Proj);

                  loop
                     Source := GPR.Element (Iter);
                     exit when Source = No_Source;

                     if not Source.Locally_Removed
                       and then Source.Dep_Path /= No_Path
                       and then
                         (not Source.Project.Externally_Built
                          or else not For_Project.Externally_Built
                          or else Source.Project.Extended_By /= No_Project)
                     then
                        if Source.Kind = Spec then
                           if Other_Part (Source) = No_Source then
                              Add (Source);
                           end if;

                        elsif not Is_Subunit (Source) then
                           Add (Source);
                        end if;
                     end if;

                     Next (Iter);
                  end loop;
               end;

               Current_Proj := Current_Proj.Extends;
            end loop;
         end Process;

         procedure Process_Aggregate_Library is
           new For_Project_And_Aggregated (Process);

      --  Start of processing for Write_Dependency_Files

      begin
         Put_Line (Exchange_File, Library_Label (Dependency_Files));
         First_Dep := null;

         Process_Aggregate_Library (For_Project, Project_Tree);

         if For_Project.Standalone_Library /= No then
            for J in 1 .. Library_Sources.Last loop
               Source := Library_Sources.Table (J);
               Add_Dep (Get_Name_String (Source.Dep_Path));
            end loop;
         end if;

         while First_Dep /= null loop
            Put_Line (Exchange_File, First_Dep.Name.all);
            First_Dep := First_Dep.Next;
         end loop;
      end Write_Dependency_Files;

      ------------------------
      -- Write_Mapping_File --
      ------------------------

      procedure Write_Mapping_File is
      begin
         if Mapping_Path /= No_Path then
            Put_Line (Exchange_File, Library_Label (Mapping_File));
            Put_Line (Exchange_File, Get_Name_String (Mapping_Path));
         end if;
      end Write_Mapping_File;

      -----------------------------
      -- Write_Toolchain_Version --
      -----------------------------

      procedure Write_Toolchain_Version is

         use type Ada.Containers.Count_Type;

         procedure Toolchain_Version_For
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Dummy   : in out Boolean);
         --  Write runtime libraries for the given project

         Dummy     : Boolean := True;
         Lang_Seen : Lang_Set.Set;

         ---------------------------
         -- Toolchain_Version_For --
         ---------------------------

         procedure Toolchain_Version_For
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            Dummy   : in out Boolean)
         is
            pragma Unreferenced (Tree, Dummy);
            List : Language_Ptr := Project.Languages;
         begin
            while List /= No_Language_Index loop
               if (List.Config.Toolchain_Version /= No_Name or else
                   List.Config.Runtime_Library_Version /= No_Name)
                 and then not Lang_Seen.Contains (List.Name)
               then
                  if Lang_Seen.Length = 0 then
                     Put_Line
                       (Exchange_File, Library_Label (Toolchain_Version));
                  end if;
                  Lang_Seen.Insert (List.Name);

                  Put_Line (Exchange_File, Get_Name_String (List.Name));

                  if List.Config.Runtime_Library_Version /= No_Name then
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (List.Config.Runtime_Library_Version));
                  else
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (List.Config.Toolchain_Version));
                  end if;
               end if;

               List := List.Next;
            end loop;
         end Toolchain_Version_For;

         procedure For_Imported is
           new For_Every_Project_Imported (Boolean, Toolchain_Version_For);

      --  Start of processing for Write_Toolchain_Version

      begin
         Toolchain_Version_For (For_Project, Project_Tree, Dummy);

         if For_Project.Qualifier = Aggregate_Library then
            For_Imported (For_Project, Project_Tree, Dummy);
         end if;
      end Write_Toolchain_Version;

      -------------------------------
      -- Write_Interface_Dep_Files --
      -------------------------------

      procedure Write_Interface_Dep_Files is
         Interface_ALIs : String_List_Id :=
                            For_Project.Lib_Interface_ALIs;
         Element        : String_Element;
      begin
         Put_Line (Exchange_File, Library_Label (Interface_Dep_Files));

         while Interface_ALIs /= Nil_String loop
            Element :=
              Project_Tree.Shared.String_Elements.Table (Interface_ALIs);

            --  Find the source to get the absolute path of the ALI file

            declare
               Next_Proj : Project_Id;
               Iter      : Source_Iterator;
            begin
               Next_Proj := For_Project.Extends;

               if For_Project.Qualifier = Aggregate_Library then
                  Iter := For_Each_Source (Project_Tree);
               else
                  Iter := For_Each_Source (Project_Tree, For_Project);
               end if;

               loop
                  while GPR.Element (Iter) /= No_Source
                    and then
                    (GPR.Element (Iter).Unit = null
                     or else GPR.Element (Iter).Dep_Name /=
                       File_Name_Type (Element.Value))
                  loop
                     Next (Iter);
                  end loop;

                  Source := GPR.Element (Iter);
                  exit when Source /= No_Source
                    or else Next_Proj = No_Project;

                  Iter := For_Each_Source (Project_Tree, Next_Proj);
                  Next_Proj := Next_Proj.Extends;
               end loop;

               if Source /= No_Source then
                  if Source.Kind = Sep then
                     Source := No_Source;

                  elsif Source.Kind = Spec
                    and then Other_Part (Source) /= No_Source
                  then
                     Source := Other_Part (Source);
                  end if;
               end if;

               if Source /= No_Source then
                  if Source.Project /= Project
                    and then not Is_Extending (For_Project, Source.Project)
                    and then not (For_Project.Qualifier = Aggregate_Library)
                  then
                     Source := No_Source;
                  end if;
               end if;

               if Source /= No_Source then
                  Put_Line (Exchange_File, Get_Name_String (Source.Dep_Path));
               end if;
            end;

            Interface_ALIs := Element.Next;
         end loop;
      end Write_Interface_Dep_Files;

      ----------------------------
      -- Write_Other_Interfaces --
      ----------------------------

      procedure Write_Other_Interfaces is
         Interfaces : String_List_Id :=
                        For_Project.Other_Interfaces;
         Element    : String_Element;
      begin
         Put_Line (Exchange_File, Library_Label (Other_Interfaces));

         while Interfaces /= Nil_String loop
            Element :=
              Project_Tree.Shared.String_Elements.Table (Interfaces);
            Put_Line (Exchange_File, Get_Name_String (Element.Value));
            Interfaces := Element.Next;
         end loop;
      end Write_Other_Interfaces;

      -------------------------------
      -- Write_Interface_Obj_Files --
      -------------------------------

      procedure Write_Interface_Obj_Files is
         List      : String_List_Id :=
                       For_Project.Lib_Interface_ALIs;
         Element   : String_Element;
         Other_Int : Boolean := False;

         function Base_Name (Name : Name_Id) return String;
         --  File name without path nor extension

         ---------------
         -- Base_Name --
         ---------------

         function Base_Name (Name : Name_Id) return String is
            N : constant String := Get_Name_String (Name);
         begin
            return Base_Name (N, File_Extension (N));
         end Base_Name;

      begin
         Put_Line (Exchange_File, Library_Label (Interface_Obj_Files));

         while List /= Nil_String loop
            Element :=
              Project_Tree.Shared.String_Elements.Table (List);

            --  Find the source to get the absolute path of the ALI file

            declare
               Next_Proj : Project_Id;
               Iter      : Source_Iterator;
            begin
               Next_Proj := For_Project.Extends;

               if For_Project.Qualifier = Aggregate_Library then
                  Iter := For_Each_Source (Project_Tree);
               else
                  Iter := For_Each_Source (Project_Tree, For_Project);
               end if;

               loop
                  --  Look for the Source_Id corresponding to this unit

                  while GPR.Element (Iter) /= No_Source
                    and then
                       --  Either an foreign language, we need the
                       --  implementation of this unit.
                      ((Other_Int
                        and then
                        (Base_Name (Name_Id (GPR.Element (Iter).Object)) /=
                             Base_Name (Element.Value)
                         or else GPR.Element (Iter).Kind = Spec))
                       --  Or and Ada unit, when need the dependency file
                       or else
                         (not Other_Int and then
                            (GPR.Element (Iter).Unit = null
                             or else GPR.Element (Iter).Dep_Name /=
                                 File_Name_Type (Element.Value))))
                  loop
                     Next (Iter);
                  end loop;

                  Source := GPR.Element (Iter);

                  exit when Source /= No_Source
                    or else Next_Proj = No_Project;

                  Iter := For_Each_Source (Project_Tree, Next_Proj);
                  Next_Proj := Next_Proj.Extends;
               end loop;

               if Source /= No_Source then
                  if Source.Kind = Sep then
                     Source := No_Source;

                  elsif Source.Kind = Spec
                    and then Other_Part (Source) /= No_Source
                  then
                     Source := Other_Part (Source);
                  end if;
               end if;

               if Source /= No_Source then
                  if Source.Project /= Project
                    and then not Is_Extending (For_Project, Source.Project)
                    and then not (For_Project.Qualifier = Aggregate_Library)
                  then
                     Source := No_Source;
                  end if;
               end if;

               if Source /= No_Source then
                  Put_Line
                    (Exchange_File, Get_Name_String (Source.Object_Path));
               end if;
            end;

            List := Element.Next;

            if List = Nil_String and then not Other_Int then
               List := For_Project.Other_Interfaces;
               Other_Int := True;
            end if;
         end loop;
      end Write_Interface_Obj_Files;

      -------------------
      -- Write_Sources --
      -------------------

      procedure Write_Sources is
      begin
         Put_Line (Exchange_File, Library_Label (Sources));

         --  Copy the path of the sources

         Project := For_Project;

         while Project /= No_Project loop
            Iter := For_Each_Source (Project_Tree, Project);

            loop
               Source := GPR.Element (Iter);
               exit when Source = No_Source;

               if not Source.Locally_Removed
                 and then Source.Replaced_By = No_Source
               then
                  Put_Line
                    (Exchange_File,
                     Get_Name_String (Source.Path.Display_Name));
               end if;

               Next (Iter);
            end loop;

            Project := Project.Extends;
         end loop;
      end Write_Sources;

      --------------------------
      -- Write_Response_Files --
      --------------------------

      procedure Write_Response_Files is
      begin
         if For_Project.Config.Max_Command_Line_Length > 0
           and then For_Project.Config.Resp_File_Format /= None
         then
            Put_Line (Exchange_File, Library_Label (Max_Command_Line_Length));
            Put_Line
              (Exchange_File, For_Project.Config.Max_Command_Line_Length'Img);

            Put_Line
              (Exchange_File, Library_Label (Gprexch.Response_File_Format));
            Put_Line
              (Exchange_File, For_Project.Config.Resp_File_Format'Img);

            if For_Project.Config.Resp_File_Options /= No_Name_List then
               Write_Name_List
                 (Exchange_File, Response_File_Switches,
                  For_Project.Config.Resp_File_Options);
            end if;
         end if;
      end Write_Response_Files;

   --  Start of processing for Build_Library

   begin
      --  Check if there is an object directory

      if For_Project.Object_Directory.Display_Name = No_Path then
         Fail_Program
           (Project_Tree,
            "no object directory for library project " &
            Get_Name_String (For_Project.Display_Name));
      end if;

      --  Check consistentcy and build environment

      if For_Project.Config.Lib_Support = None then
         Fail_Program (Project_Tree,
                       "library projects not supported on this platform");

      elsif not Is_Static (For_Project)
            and then For_Project.Config.Lib_Support /= Full
      then
         Fail_Program
           (Project_Tree,
            "shared library projects not supported on this platform");

      elsif not For_Project.Config.Lib_Encapsulated_Supported
        and then For_Project.Standalone_Library = Encapsulated
      then
         Fail_Program
           (Project_Tree,
            "encapsulated library projects not supported on this platform");
      end if;

      if For_Project.Config.Library_Builder = No_Path then
         Fail_Program (Project_Tree, "no library builder specified");

      else
         Library_Builder :=
           Locate_Exec_On_Path
             (Get_Name_String (For_Project.Config.Library_Builder));

         if Library_Builder = null then
            Fail_Program
              (Project_Tree,
               "could not locate library builder """ &
               Get_Name_String (For_Project.Config.Library_Builder) & '"');

         else
            Library_Builder_Name :=
              new String'(Ada.Directories.Base_Name
                            (Ada.Directories.Simple_Name
                               (Library_Builder.all)));
         end if;
      end if;

      if Is_Static (For_Project) then
         Check_Archive_Builder;

      elsif For_Project.Standalone_Library /= No then
         Check_Object_Lister;
         Check_Export_File;
         Check_Library_Symbol_File;
      end if;

      Library_Needs_To_Be_Built := Opt.Force_Compilations;

      if not Library_Needs_To_Be_Built and then
         Opt.Verbosity_Level > Opt.Low
      then
         Put ("   Checking library ");
         Get_Name_String (For_Project.Library_Name);
         Put (Name_Buffer (1 .. Name_Len));
         Put_Line (" ...");
      end if;

      Get_Objects;

      --  Work occurs in the object directory

      Change_To_Object_Directory (For_Project);

      --  Get the name of of the library exchange file

      Get_Name_String (For_Project.Library_Name);
      Add_Str_To_Name_Buffer (Library_Exchange_Suffix);
      Exchange_File_Name := new String'(Name_Buffer (1 .. Name_Len));

      if not Library_Needs_To_Be_Built then
         declare
            TS : constant Time_Stamp_Type :=
                   File_Stamp (File_Name_Type'(Name_Find));

         begin
            if String (TS) < String (Latest_Object_TS) then
               Library_Needs_To_Be_Built := True;

               if Opt.Verbosity_Level > Opt.Low then
                  if TS = Empty_Time_Stamp then
                     Put_Line
                       ("      -> library exchange file " &
                        Exchange_File_Name.all &
                        " does not exist");

                  else
                     Put_Line
                       ("      -> object files more recent than" &
                        " library exchange file " &
                        Exchange_File_Name.all);
                  end if;
               end if;

            else
               begin
                  Open (Exchange_File, In_File, Exchange_File_Name.all);

                  if End_Of_File (Exchange_File) then
                     if Opt.Verbosity_Level > Opt.Low then
                        Put ("      -> library exchange file """);
                        Put (Exchange_File_Name.all);
                        Put_Line (""" is empty");
                     end if;

                     Library_Needs_To_Be_Built := True;
                  end if;

               exception
                  when others =>
                     if Opt.Verbosity_Level > Opt.Low then
                        Put
                          ("      -> library exchange file """);
                        Put (Exchange_File_Name.all);
                        Put_Line (""" cannot be open");
                     end if;

                     Library_Needs_To_Be_Built := True;
               end;
            end if;
         end;
      end if;

      if not Library_Needs_To_Be_Built then
         --  The exchange file is open in input

         --  Get the path of the library file that should be the first field

         Get_Line (Exchange_File, Name_Buffer, Name_Len);

         if Name_Buffer (1 .. Name_Len) /= Library_Label (Library_Path) then
            Library_Needs_To_Be_Built := True;
            Close (Exchange_File);

            if  Opt.Verbosity_Level > Opt.Low then
               Put_Line
                 ("      -> library exchange file "
                  & Exchange_File_Name.all & " has wrong format");
            end if;

         else
            Get_Line (Exchange_File, Name_Buffer, Name_Len);

            declare
               Lib_File_Name     : constant String :=
                                     Base_Name (Name_Buffer (1 .. Name_Len));
               Shared_Lib_Prefix : String_Access := new String'("lib");
               Shared_Lib_Suffix : String_Access := new String'(".so");
               Archive_Suffix    : String_Access := new String'(".a");
            begin
               if Is_Static (For_Project) then
                  if For_Project.Config.Archive_Suffix /= No_File then
                     Archive_Suffix :=
                       new String'
                         (Get_Name_String (For_Project.Config.Archive_Suffix));
                  end if;

                  Expected_File_Name :=
                    new String'
                      ("lib" &
                       Get_Name_String (For_Project.Library_Name) &
                       Archive_Suffix.all);
               else
                  if For_Project.Config.Shared_Lib_Prefix /= No_File then
                     Shared_Lib_Prefix :=
                       new String'
                         (Get_Name_String
                            (For_Project.Config.Shared_Lib_Prefix));
                  end if;

                  if For_Project.Config.Shared_Lib_Suffix /= No_File then
                     Shared_Lib_Suffix :=
                       new String'
                         (Get_Name_String
                            (For_Project.Config.Shared_Lib_Suffix));
                  end if;

                  Expected_File_Name :=
                    new String'
                      (Shared_Lib_Prefix.all &
                         Get_Name_String (For_Project.Library_Name) &
                           Shared_Lib_Suffix.all);
               end if;

               if Lib_File_Name /= Expected_File_Name.all then
                  Library_Needs_To_Be_Built := True;
                  Close (Exchange_File);

                  if  Opt.Verbosity_Level > Opt.Low then
                     Put_Line ("      -> incorrect library file name");
                     Put_Line ("   expected " & Expected_File_Name.all);
                     Put_Line ("     actual " & Lib_File_Name);
                  end if;
               end if;
            end;

            if not Library_Needs_To_Be_Built then
               For_Project.Library_TS :=
                 File_Stamp (File_Name_Type'(Name_Find));

               if String (For_Project.Library_TS) <
                  String (Latest_Object_TS)
               then
                  Library_Needs_To_Be_Built := True;
                  Close (Exchange_File);

                  if Opt.Verbosity_Level > Opt.Low then
                     Put_Line
                       ("      -> " &
                        "object file(s) more recent than library file " &
                        Exchange_File_Name.all);
                  end if;
               end if;
            end if;
         end if;
      end if;

      if not Library_Needs_To_Be_Built then
         --  The next line should be the object file label, followed by the
         --  object paths and time stamps.

         Get_Line (Exchange_File, Name_Buffer, Name_Len);

         if Name_Buffer (1 .. Name_Len) /= Library_Label (Object_Files) then
            Library_Needs_To_Be_Built := True;

            if Opt.Verbosity_Level > Opt.Low then
               Put_Line ("      -> library exchange file "
                           & Exchange_File_Name.all & " has wrong format");
            end if;
         end if;

         while not Library_Needs_To_Be_Built
           and then not End_Of_File (Exchange_File)
         loop
            Get_Line (Exchange_File, Name_Buffer, Name_Len);

            exit when Name_Buffer (1) = '[';

            Object_Path := Name_Find;

            Library_Needs_To_Be_Built := True;

            if End_Of_File (Exchange_File) then
               if Opt.Verbosity_Level > Opt.Low then
                  Put_Line
                    ("      -> library exchange file " &
                     Exchange_File_Name.all & " has wrong format");
               end if;

            else
               Get_Line (Exchange_File, Name_Buffer, Name_Len);

               if Name_Len = Time_Stamp_Length then
                  Object_TS :=
                    Time_Stamp_Type (Name_Buffer (1 .. Name_Len));

                  Path_Found := False;
                  for Index in 1 .. Library_Objs.Last loop
                     if Object_Path = Library_Objs.Table (Index).Path then
                        Path_Found := True;
                        Library_Needs_To_Be_Built :=
                          Object_TS /= Library_Objs.Table (Index).TS;
                        Library_Objs.Table (Index).Known := True;
                        exit;
                     end if;
                  end loop;

                  --  If the object file is not found, it may be that the path
                  --  in the library is the same as the path of the object
                  --  files, but with different symbolic links. So, we try
                  --  again resolving the symbolic links.

                  if not Path_Found then
                     declare
                        Norm_Path : constant String :=
                                      Normalize_Pathname
                                        (Get_Name_String (Object_Path));

                     begin
                        for Index in 1 .. Library_Objs.Last loop
                           if Norm_Path =
                             Normalize_Pathname
                               (Get_Name_String
                                    (Library_Objs.Table (Index).Path))
                           then
                              Library_Needs_To_Be_Built :=
                                Object_TS /= Library_Objs.Table (Index).TS;
                              Library_Objs.Table (Index).Known := True;
                              exit;
                           end if;
                        end loop;
                     end;
                  end if;

                  if Library_Needs_To_Be_Built and then
                    Opt.Verbosity_Level > Opt.Low
                  then
                     Put ("      -> object file ");
                     Put (Get_Name_String (Object_Path));
                     Put_Line
                       (" does not exist or have wrong time stamp");
                  end if;

               else
                  if Opt.Verbosity_Level > Opt.Low then
                     Put_Line
                       ("      -> library exchange file " &
                        Exchange_File_Name.all &
                        " has wrong format");
                  end if;
               end if;
            end if;
         end loop;

         Close (Exchange_File);

         if not Library_Needs_To_Be_Built then
            for Index in 1 .. Library_Objs.Last loop
               if not Library_Objs.Table (Index).Known then
                  Library_Needs_To_Be_Built := True;

                  if Opt.Verbosity_Level > Opt.Low then
                     Put
                       ("      -> library was built without object file ");
                     Put_Line
                       (Get_Name_String (Library_Objs.Table (Index).Path));
                  end if;

                  exit;
               end if;
            end loop;
         end if;
      end if;

      if not Library_Needs_To_Be_Built then
         --  Check if in a project imported directly or indirectly the time
         --  stamp of a library is greater than the time stamp of this library.

         declare
            List    : Project_List;
            Proj2   : Project_Id;

            Lib_Timestamp1 : constant Time_Stamp_Type :=
                               For_Project.Library_TS;

         begin
            List := For_Project.All_Imported_Projects;
            while List /= null loop
               Proj2 := List.Project;

               if Proj2.Library then
                  if Proj2.Need_To_Build_Lib
                    or else
                      (Lib_Timestamp1 < Proj2.Library_TS)
                  then
                     Library_Needs_To_Be_Built := True;

                     if  Opt.Verbosity_Level > Opt.Low then
                        Put
                          ("      -> library file for project ");
                        Put (Get_Name_String (Proj2.Display_Name));
                        Put
                          (" is more recent than library file for project ");
                        Put_Line
                          (Get_Name_String (For_Project.Display_Name));
                     end if;

                     exit;
                  end if;
               end if;

               List := List.Next;
            end loop;
         end;
      end if;

      if not Library_Needs_To_Be_Built then
         if Opt.Verbosity_Level > Opt.Low then
            if For_Project = Main_Project then
               Put ('"');
               Put (Expected_File_Name.all);
               Put_Line (""" up to date");

            else
               Put_Line ("      -> up to date");
            end if;
         end if;

      else
         --  Create the library exchange file

         begin
            Create (Exchange_File, Out_File, Exchange_File_Name.all);

         exception
            when others =>
               Fail_Program
                 (Project_Tree,
                  "unable to create library exchange file " &
                  Exchange_File_Name.all);
         end;

         if Opt.Quiet_Output then
            Put_Line (Exchange_File, Library_Label (Quiet));

         elsif Opt.Verbose_Mode then
            if Opt.Verbosity_Level = Opt.Low then
               Put_Line (Exchange_File, Library_Label (Verbose_Low));
            else
               Put_Line (Exchange_File, Library_Label (Verbose_Higher));
            end if;
         end if;

         if No_SAL_Binding then
            Put_Line (Exchange_File, Library_Label (Gprexch.No_SAL_Binding));
         end if;

         Write_Object_Files;

         --  Library name

         Put_Line (Exchange_File, Library_Label (Library_Name));
         Put_Line (Exchange_File, Get_Name_String (For_Project.Library_Name));

         --  Library version

         if For_Project.Lib_Internal_Name /= No_Name then
            Put_Line (Exchange_File, Library_Label (Library_Version));
            Put_Line (Exchange_File,
                      Get_Name_String (For_Project.Lib_Internal_Name));
         end if;

         --  Library directory

         Put_Line (Exchange_File, Library_Label (Library_Directory));
         Put_Line
           (Exchange_File,
            Get_Name_String (For_Project.Library_Dir.Display_Name));

         --  Project directory

         Put_Line (Exchange_File, Library_Label (Project_Directory));
         Put_Line
           (Exchange_File,
            Get_Name_String (For_Project.Directory.Display_Name));

         if For_Project.Library_ALI_Dir /= No_Path_Information
           and then
             For_Project.Library_ALI_Dir.Name /= For_Project.Library_Dir.Name
         then
            Put_Line
              (Exchange_File, Library_Label (Library_Dependency_Directory));
            Put_Line
              (Exchange_File,
               Get_Name_String (For_Project.Library_ALI_Dir.Display_Name));
         end if;

         Write_Object_Directory;

         Write_Compilers;

         Write_Compiler_Leading_Switches;

         Write_Compiler_Trailing_Switches;

         Write_Partial_Linker;

         if No_Create then
            Put_Line (Exchange_File, Library_Label (Gprexch.No_Create));
         end if;

         if Is_Static (For_Project) then
            Put_Line (Exchange_File, Library_Label (Static));

            Put_Line (Exchange_File, Library_Label (Archive_Builder));
            Put_Line (Exchange_File, Archive_Builder_Path.all);

            for J in 1 .. Archive_Builder_Opts.Last loop
               Put_Line (Exchange_File, Archive_Builder_Opts.Options (J).all);
            end loop;

            if Archive_Builder_Append_Opts.Last > 0 then
               Put_Line
                 (Exchange_File,
                  Library_Label (Archive_Builder_Append_Option));

               for J in 1 .. Archive_Builder_Append_Opts.Last loop
                  Put_Line
                    (Exchange_File,
                     Archive_Builder_Append_Opts.Options (J).all);
               end loop;
            end if;

            if For_Project.Config.Archive_Suffix /= No_File then
               Put_Line (Exchange_File, Library_Label (Archive_Suffix));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (For_Project.Config.Archive_Suffix));
            end if;

            if Archive_Indexer_Path /= null then
               Put_Line (Exchange_File, Library_Label (Archive_Indexer));
               Put_Line (Exchange_File, Archive_Indexer_Path.all);

               for J in 1 .. Archive_Indexer_Opts.Last loop
                  Put_Line
                    (Exchange_File, Archive_Indexer_Opts.Options (J).all);
               end loop;
            end if;

         else
            --  Driver_Name

            if For_Project.Config.Shared_Lib_Driver /= No_File then
               Put_Line (Exchange_File, Library_Label (Driver_Name));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (For_Project.Config.Shared_Lib_Driver));
            end if;

            --  Shared_Lib_Prefix

            if For_Project.Config.Shared_Lib_Prefix /= No_File then
               Put_Line (Exchange_File, Library_Label (Shared_Lib_Prefix));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (For_Project.Config.Shared_Lib_Prefix));
            end if;

            --  Shared_Lib_Suffix

            if For_Project.Config.Shared_Lib_Suffix /= No_File then
               Put_Line (Exchange_File, Library_Label (Shared_Lib_Suffix));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (For_Project.Config.Shared_Lib_Suffix));
            end if;

            Write_Shared_Lib_Minimum_Options;

            Write_Library_Version;

            --  Symbolic_Link_Supported

            if For_Project.Config.Symbolic_Link_Supported then
               Put_Line
                 (Exchange_File, Library_Label (Symbolic_Link_Supported));
            end if;

            --  Major_Minor_Id_Supported

            if For_Project.Config.Lib_Maj_Min_Id_Supported then
               Put_Line
                 (Exchange_File, Library_Label (Major_Minor_Id_Supported));
            end if;

            Process_Imported_Libraries
              (For_Project, There_Are_SALs => Disregard);

            Write_Runtime_Library_Dir;

            --  Relocatable

            if not Is_Static (For_Project) then
               Put_Line (Exchange_File, Library_Label (Relocatable));
            end if;

            --  Auto_init

            Write_Auto_Init;

            --  Gprexch.Install_Name

            if Opt.Run_Path_Option and then
               For_Project.Config.Library_Install_Name_Option /= No_Name
            then
               Put_Line (Exchange_File, Library_Label (Gprexch.Install_Name));
               Put_Line
                 (Exchange_File,
                  Get_Name_String
                    (For_Project.Config.Library_Install_Name_Option));
            end if;

            Write_Run_Path_Option;

            Write_Leading_Library_Options;

            Write_Library_Rpath_Options;

            Write_Imported_Libraries;
         end if;

         Write_Library_Options (Library_Options_Success);

         Write_Dependency_Files;

         Write_Mapping_File;

         Write_Toolchain_Version;

         if For_Project.Standalone_Library /= No then
            if For_Project.Lib_Auto_Init then
               Put_Line (Exchange_File, Library_Label (Auto_Init));
            end if;

            Write_Interface_Dep_Files;

            if For_Project.Other_Interfaces /= Nil_String then
               Write_Other_Interfaces;
            end if;

            if For_Project.Library_Src_Dir /= No_Path_Information then
               --  Copy_Source_Dir

               Put_Line (Exchange_File, Library_Label (Copy_Source_Dir));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (For_Project.Library_Src_Dir.Display_Name));

               Write_Sources;
            end if;

            --  Standalone mode

            Put_Line (Exchange_File, Library_Label (Standalone_Mode));
            Put_Line
              (Exchange_File,
               Standalone'Image (For_Project.Standalone_Library));

            if For_Project.Symbol_Data.Symbol_Policy = Restricted then
               if Library_Symbol_File /= null then
                  Put_Line
                    (Exchange_File,
                     Library_Label (Gprexch.Library_Symbol_File));
                  Put_Line (Exchange_File, Library_Symbol_File.all);

               elsif Object_Lister_Path /= null then
                  --  Write interface objects

                  Write_Interface_Obj_Files;

                  --  Write object lister

                  Put_Line (Exchange_File, Library_Label (Object_Lister));
                  Put_Line (Exchange_File, Object_Lister_Path.all);

                  for J in 1 .. Object_Lister_Opts.Last loop
                     Put_Line
                       (Exchange_File, Object_Lister_Opts.Options (J).all);
                  end loop;

                  Put_Line
                    (Exchange_File,
                     Library_Label (Gprexch.Object_Lister_Matcher));
                  Put_Line (Exchange_File, Object_Lister_Matcher.all);
               end if;

               if Export_File_Switch /= null then
                  --  Write export symbols format

                  Put_Line (Exchange_File, Library_Label (Export_File));
                  Put_Line
                    (Exchange_File,
                     GPR.Export_File_Format'Image (Export_File_Format));
                  Put_Line (Exchange_File, Export_File_Switch.all);
               end if;
            end if;

         elsif For_Project.Other_Interfaces /= Nil_String then
            Write_Other_Interfaces;
         end if;

         Write_Response_Files;

         if Debug.Debug_Flag_N then
            Put_Line (Exchange_File, Library_Label (Keep_Temporary_Files));
         end if;

         if Build_Script_Name /= null then
            Put_Line (Exchange_File, Library_Label (Script_Path));
            Put_Line (Exchange_File, Build_Script_Name.all);
         end if;

         Close (Exchange_File);

         declare
            Arguments : constant Argument_List := (1 => Exchange_File_Name);
            Success   : Boolean;

         begin
            if Library_Options_Success then
               if not Opt.Quiet_Output then
                  if Opt.Verbose_Mode then
                     Put_Line
                       (Library_Builder.all & " " & Exchange_File_Name.all);

                  else
                     Display
                       (Section  => Build_Libraries,
                        Command  => Library_Builder_Name.all,
                        Argument => Exchange_File_Name.all);
                  end if;
               end if;

               Spawn (Library_Builder.all, Arguments, Success);

            else
               Success := False;
            end if;

            if not Success then
               Fail_Program
                 (Project_Tree,
                  "could not build library for project " & Project_Name);
            end if;
         end;
      end if;

      --  Restore the current working directory to its previous value

      Change_Dir (Current_Dir);
   end Build_Library;

   -----------------------------------
   -- Is_Included_In_Global_Archive --
   -----------------------------------

   function Is_Included_In_Global_Archive
     (Object_Name : File_Name_Type;
      Project     : Project_Id) return Boolean
   is
      Proj   : Project_Id;
      Source : Source_Id;
      Iter   : Source_Iterator;

   begin
      --  If a source is overriden in an extending project, then the object
      --  file is not included in the global archive.

      Proj := Project.Extended_By;
      while Proj /= No_Project loop
         Iter := For_Each_Source (Project_Tree, Proj);
         loop
            Source := GPR.Element (Iter);
            exit when Source = No_Source;

            if Object_To_Global_Archive (Source)
              and then Source.Object = Object_Name
            then
               return False;
            end if;

            Next (Iter);
         end loop;
         Proj := Proj.Extended_By;
      end loop;

      Iter := For_Each_Source (Project_Tree, Project);

      loop
         Source := GPR.Element (Iter);
         exit when Source = No_Source;

         if Object_To_Global_Archive (Source)
           and then Source.Object =  Object_Name
         then
            return Source.Language.Config.Objects_Linked;
         end if;

         Next (Iter);
      end loop;

      return True;
   end Is_Included_In_Global_Archive;

   ---------
   -- Run --
   ---------

   procedure Run is

      Data : Process_Data;
      Main : Main_Info;
      OK   : Boolean;

      procedure Do_Post (Project : Project_Id; Tree : Project_Tree_Ref);

      -------------
      -- Do_Post --
      -------------

      procedure Do_Post (Project : Project_Id; Tree : Project_Tree_Ref) is
      begin
         if Builder_Data (Tree).Need_Binding
           and then not Stop_Spawning
         then
            Post_Compilation_Phase (Project, Tree);
         end if;
      end Do_Post;

      procedure Post_Compile_All is new For_Project_And_Aggregated (Do_Post);

   begin
      Outstanding_Processes := 0;
      Stop_Spawning := False;

      if Main_Project.Qualifier = Aggregate_Library then
         --  For an aggregate library we do not want to build separate
         --  libraries if any, this means that at this point we want to
         --  handle only the main aggregate library project.
         Post_Compilation_Phase (Main_Project, Project_Tree);

      else
         Post_Compile_All (Main_Project, Project_Tree);
      end if;

      while Outstanding_Processes > 0 loop
         Await_Process (Data, OK);

         if not OK then
            Record_Failure (Data.Main);
         end if;

         Display_Processes ("bind");
      end loop;

      if Bad_Processes.Last = 1 then
         Main := Bad_Processes.Table (1);
         Fail_Program
           (Main.Tree,
            "unable to bind " & Get_Name_String (Main.File));

      elsif Bad_Processes.Last > 1 then
         for J in 1 .. Bad_Processes.Last loop
            Main := Bad_Processes.Table (J);
            Put ("   binding of ");
            Put (Get_Name_String (Main.File));
            Put_Line (" failed");
         end loop;

         Fail_Program (Main.Tree, "*** post compilation phase failed");
      end if;
   end Run;

   ----------------------------
   -- Post_Compilation_Phase --
   ----------------------------

   procedure Post_Compilation_Phase
     (Main_Project : Project_Id; Project_Tree : Project_Tree_Ref)
   is
      Exchange_File : Text_IO.File_Type;
      Line          : String (1 .. 1_000);
      Last          : Natural;

      Proj_List : Project_List;

      Shared_Libs : Boolean := False;

      Bind_Exchange_TS                 : Time_Stamp_Type;
      Bind_Object_TS                   : Time_Stamp_Type;
      Binder_Driver_Needs_To_Be_Called : Boolean := False;

      Project_Path    : Name_Id;
      Project_File_TS : Time_Stamp_Type;

      There_Are_Stand_Alone_Libraries : Boolean := False;
      --  Set to True if there are SALS in the project tree

      procedure Bind_Language
        (Main_Proj            : Project_Id;
         Main                 : String;
         Main_Base_Name_Index : File_Name_Type;
         Main_File            : Main_Info;
         Main_Id              : File_Name_Type;
         B_Data               : Binding_Data);
      --  Do the "binding" phase for the language describeb in B_Data

      procedure Add_Dependency_Files
        (For_Project : Project_Id;
         Language    : Language_Ptr;
         Main_Source : Source_Id;
         Dep_Files   : out Boolean);
      --  Put the dependency files of the project in the binder exchange file

      procedure Wait_For_Available_Slot;

      --------------------------
      -- Add_Dependency_Files --
      --------------------------

      procedure Add_Dependency_Files
        (For_Project : Project_Id;
         Language    : Language_Ptr;
         Main_Source : Source_Id;
         Dep_Files   : out Boolean)
      is
         Config : constant Language_Config := Language.Config;
         Roots  : Roots_Access;
         Iter   : Source_Iterator;

         procedure Put_Dependency_File (Source : Source_Id);
         --  Put in the exchange file the dependency file path name for source
         --  Source, if applicable.

         -------------------------
         -- Put_Dependency_File --
         -------------------------

         procedure Put_Dependency_File (Source : Source_Id) is
         begin
            if Source.Language.Name = Language.Name
              and then
                ((Config.Kind = File_Based and then Source.Kind = Impl)
                 or else
                   (Config.Kind = Unit_Based
                    and then
                      Source.Unit /= No_Unit_Index
                    and then
                      Source.Unit /= Main_Source.Unit
                    and then
                      (Source.Kind = Impl
                       or else Other_Part (Source) = No_Source)
                    and then not Is_Subunit (Source)))
              and then Is_Included_In_Global_Archive
                (Source.Object, Source.Project)
            then
               if Source.Project = For_Project
                 or not Source.Project.Library
                 or Config.Kind = File_Based
               then
                  Add_Dep (Get_Name_String (Source.Dep_Path));
                  Dep_Files := True;

               elsif Source.Project.Standalone_Library = No then
                  Get_Name_String
                    (Source.Project.Library_ALI_Dir.Display_Name);
                  Get_Name_String_And_Append (Name_Id (Source.Dep_Name));
                  Add_Dep (Name_Buffer (1 .. Name_Len));
                  Dep_Files := True;
               end if;
            end if;
         end Put_Dependency_File;

      --  Start of processing for Add_Dependency_Files

      begin
         Dep_Files := False;

         Roots := Main_Source.Roots;

         if Roots = null then
            if Main_Source.Unit = No_Unit_Index then
               if Main_Project.Qualifier = Aggregate_Library then
                  Iter := For_Each_Source (Project_Tree);
               else
                  Iter := For_Each_Source
                    (Project_Tree, Encapsulated_Libs => False);
               end if;

               while GPR.Element (Iter) /= No_Source loop
                  Initialize_Source_Record (GPR.Element (Iter));

                  --  Do not bind the non compilable sources, such as those
                  --  that have been locally removed.

                  if Is_Compilable (GPR.Element (Iter)) then
                     Put_Dependency_File (GPR.Element (Iter));
                  end if;

                  Next (Iter);
               end loop;
            end if;

         else
            --  Put the Roots
            while Roots /= null loop
               if Roots.Root /= No_Source then
                  Put_Dependency_File (Roots.Root);
               end if;

               Roots := Roots.Next;
            end loop;
         end if;
      end Add_Dependency_Files;

      -------------------
      -- Bind_Language --
      -------------------

      procedure Bind_Language
        (Main_Proj            : Project_Id;
         Main                 : String;
         Main_Base_Name_Index : File_Name_Type;
         Main_File            : Main_Info;
         Main_Id              : File_Name_Type;
         B_Data               : Binding_Data)
      is
         package Project_File_Paths is new GNAT.HTable.Simple_HTable
           (Header_Num => GPR.Header_Num,
            Element    => Boolean,
            No_Element => False,
            Key        => Name_Id,
            Hash       => Hash,
            Equal      => "=");

         Main_Source : constant Source_Id := Main_File.Source;

         Bind_Exchange                    : String_Access;
         Options_Instance                 : Bind_Option_Table_Ref :=
                                              No_Bind_Option_Table;
         Dep_Files                        : Boolean;
         Lang_Index                       : Language_Ptr;
         Object_File_Suffix_Label_Written : Boolean;

      begin
         Binder_Driver_Needs_To_Be_Called := Opt.Force_Compilations;

         --  First check if the binder driver needs to be called.
         --  It needs to be called if
         --    1) there is no existing binder exchange file
         --    2) there is no binder generated object file
         --    3) there is a dependency file of the language that
         --       is more recent than any of these two files

         if not Binder_Driver_Needs_To_Be_Called and then
           Opt.Verbosity_Level > Opt.Low
         then
            Put_Line
              ("   Checking binder generated files for " & Main & "...");
         end if;

         Bind_Exchange :=
           Binder_Exchange_File_Name
             (Main_Base_Name_Index, B_Data.Binder_Prefix);
         Bind_Exchange_TS :=
           File_Stamp
             (Path_Name_Type'(Create_Name (Bind_Exchange.all)));

         if not Binder_Driver_Needs_To_Be_Called then
            if Bind_Exchange_TS = Empty_Time_Stamp then
               Binder_Driver_Needs_To_Be_Called := True;

               if Opt.Verbosity_Level > Opt.Low then
                  Put_Line
                    ("      -> binder exchange file " &
                     Bind_Exchange.all &
                     " does not exist");
               end if;

            else
               begin
                  Open (Exchange_File, In_File, Bind_Exchange.all);

               exception
                  when others =>
                     Binder_Driver_Needs_To_Be_Called := True;

                     if Opt.Verbosity_Level > Opt.Low then
                        Put_Line
                          ("      -> could not open " &
                           "binder exchange file" &
                           Bind_Exchange.all);
                     end if;
               end;
            end if;
         end if;

         if not Binder_Driver_Needs_To_Be_Called then
            begin
               Get_Line (Exchange_File, Line, Last);
            exception
               when others =>
                  Binder_Driver_Needs_To_Be_Called := True;

                  if Opt.Verbosity_Level > Opt.Low then
                     Put_Line
                       ("      -> previous gprbind failed, or " &
                        Bind_Exchange.all &
                        " corrupted");
                  end if;
            end;
         end if;

         --  Check the generated object file

         if not Binder_Driver_Needs_To_Be_Called then
            if Line (1 .. Last) /= Binding_Label (Generated_Object_File)
              or else End_Of_File (Exchange_File)
            then
               Binder_Driver_Needs_To_Be_Called := True;

               if Opt.Verbosity_Level > Opt.Low then
                  Put_Line
                    ("      -> previous gprbind failed, or " &
                     Bind_Exchange.all &
                     " corrupted");
               end if;

            else
               Get_Line (Exchange_File, Line, Last);
               Bind_Object_TS :=
                 File_Stamp
                   (Path_Name_Type'(Create_Name (Line (1 .. Last))));

               --  Do not perform this check in CodePeer mode where there is
               --  no object file per se.

               if Bind_Object_TS = Empty_Time_Stamp
                 and not Opt.CodePeer_Mode
               then
                  Binder_Driver_Needs_To_Be_Called := True;

                  if Opt.Verbosity_Level > Opt.Low then
                     Put_Line
                       ("      -> binder generated object " &
                        Line (1 .. Last) &
                        " does not exist");
                  end if;
               end if;
            end if;
         end if;

         if not Binder_Driver_Needs_To_Be_Called then
            if End_Of_File (Exchange_File) then
               Binder_Driver_Needs_To_Be_Called := True;

            else
               Get_Line (Exchange_File, Line, Last);

               if Line (1 .. Last) /= Binding_Label (Project_Files)
                 or else End_Of_File (Exchange_File)
               then
                  Binder_Driver_Needs_To_Be_Called := True;
               end if;
            end if;

            if Binder_Driver_Needs_To_Be_Called then
               if Opt.Verbosity_Level > Opt.Low then
                  Put_Line
                    ("      -> previous gprbind failed, or " &
                     Bind_Exchange.all & " corrupted");
               end if;

            else
               --  Populate the hash table Project_File_Paths with
               --  the paths of all project files in the closure
               --  of the main project.

               Project_File_Paths.Reset;

               Project_File_Paths.Set
                 (Name_Id (Main_Proj.Path.Display_Name), True);

               Proj_List := Main_Proj.All_Imported_Projects;

               while Proj_List /= null loop
                  Project_File_Paths.Set
                    (Name_Id (Proj_List.Project.Path.Display_Name),
                     True);
                  Proj_List := Proj_List.Next;
               end loop;

               --  Get the project file paths from the exchange
               --  file and check if they are the expected project
               --  files with the same time stamps.

               while not End_Of_File (Exchange_File) loop
                  Get_Line (Exchange_File, Name_Buffer, Name_Len);
                  exit when Name_Len > 0 and then Name_Buffer (1) = '[';

                  if End_Of_File (Exchange_File) then
                     Binder_Driver_Needs_To_Be_Called := True;

                     if Opt.Verbosity_Level > Opt.Low then
                        Put_Line
                          ("      -> previous gprbind failed, "
                           & "or " & Bind_Exchange.all & " corrupted");
                     end if;

                     exit;
                  end if;

                  Project_Path := Name_Find;

                  if Project_File_Paths.Get (Project_Path) then
                     Project_File_Paths.Remove (Project_Path);
                     Get_Line (Exchange_File, Line, Last);

                     Project_File_TS :=
                       File_Stamp (Path_Name_Type (Project_Path));

                     if String (Project_File_TS) /= Line (1 .. Last) then
                        Binder_Driver_Needs_To_Be_Called := True;

                        if Opt.Verbosity_Level > Opt.Low then
                           Put_Line
                             ("      -> project file "
                              & Get_Name_String (Project_Path)
                              & " has been modified");
                        end if;

                        exit;
                     end if;

                  else
                     Binder_Driver_Needs_To_Be_Called := True;

                     if Opt.Verbosity_Level > Opt.Low then
                        Put_Line
                          ("      -> unknown project file "
                           & Get_Name_String (Project_Path));
                     end if;

                     exit;
                  end if;
               end loop;

               --  Check if there are still project file paths in
               --  the hash table.

               if not Binder_Driver_Needs_To_Be_Called
                 and then Project_File_Paths.Get_First
               then
                  Binder_Driver_Needs_To_Be_Called := True;

                  if Opt.Verbosity_Level > Opt.Low then
                     Put_Line ("      -> more project files");
                  end if;
               end if;
            end if;
         end if;

         if Is_Open (Exchange_File) then
            Close (Exchange_File);
         end if;

         if not Binder_Driver_Needs_To_Be_Called then

            Queue.Initialize (Opt.One_Compilation_Per_Obj_Dir, Force => True);

            declare
               Config          : constant Language_Config :=
                                   B_Data.Language.Config;
               Source_Identity : Source_Id;
               Roots           : Roots_Access;
               Source          : Source_Id;
               Iter            : Source_Iterator;

            begin
               --  Put the root sources in the queue

               if Main_Source.Language.Name = B_Data.Language.Name then
                  Queue.Insert
                    (Source => (Tree    => Main_File.Tree,
                                Id      => Main_File.Source,
                                Closure => False));
               end if;

               Roots := Main_Source.Roots;

               while Roots /= null loop
                  Queue.Insert
                    (Source => (Tree    => Main_File.Tree,
                                Id      => Roots.Root,
                                Closure => False));
                  Roots := Roots.Next;
               end loop;

               --  If main is not unit base and there is no root,
               --  check all sources with the language name of the
               --  binder, except those that are not interfaces of
               --  their project.

               if Queue.Is_Empty then
                  Iter := For_Each_Source (Project_Tree);

                  Loop1 : loop
                     Source := GPR.Element (Iter);
                     exit Loop1 when Source = No_Source;

                     if Source.Language.Name = B_Data.Language.Name
                       and then not Source.Locally_Removed
                       and then Is_Compilable (Source)
                       and then
                         ((Config.Kind = File_Based
                           and then Source.Kind = Impl)
                          or else
                            (Config.Kind = Unit_Based
                             and then Source.Unit /= No_Unit_Index
                             and then Source.Unit /= Main_Source.Unit
                             and then (Source.Kind = Impl
                                       or else Other_Part (Source) = No_Source)
                             and then not Is_Subunit (Source)))
                       and then Source.In_Interfaces
                     then
                        declare
                           Proj  : Project_Id;
                           Src   : Source_Id;
                           Iter2 : Source_Iterator;

                        begin
                           --  If a source is overriden in an
                           --  extending project, then the object file
                           --  is not included in the global archive.

                           Proj := Source.Project.Extended_By;
                           Loop2 : while Proj /= No_Project loop
                              Iter2 := For_Each_Source (Project_Tree, Proj);
                              loop
                                 Src := GPR.Element (Iter2);
                                 exit when Src = No_Source;

                                 exit Loop1 when
                                   Src.Object = Source.Object;

                                 Next (Iter2);
                              end loop;
                              Proj := Proj.Extended_By;
                           end loop Loop2;
                        end;

                        Queue.Insert
                          (Source => (Tree    => Main_File.Tree,
                                      Id      => Source,
                                      Closure => False));
                     end if;

                     Next (Iter);
                  end loop Loop1;
               end if;

               --  Get each file from the queue and check its
               --  dependency file.

               declare
                  Dep_TS   : aliased File_Attributes := Unknown_Attributes;
                  Dep_File : File_Name_Type;
                  Dep_Path : Path_Name_Type;
                  Stamp    : Time_Stamp_Type;
                  The_ALI  : ALI.ALI_Id;
                  Text     : Text_Buffer_Ptr;
                  Found    : Boolean;
                  Source   : Queue.Source_Info;
               begin
                  while not Queue.Is_Empty loop
                     Queue.Extract (Found, Source);
                     Source_Identity := Source.Id;

                     Initialize_Source_Record (Source_Identity);

                     --  Get the dependency file for this source

                     Dep_File := Source_Identity.Dep_Name;
                     Dep_Path := Source_Identity.Dep_Path;
                     Dep_TS   := Source_Identity.Dep_TS;

                     --  For a library file, if there is no ALI file
                     --  in the object directory, check in the Library
                     --  ALI directory.

                     if not Is_Regular_File (Get_Name_String (Dep_Path))
                       and then Source_Identity.Project.Library
                       and then
                         Source_Identity.Project.Library_ALI_Dir /=
                           No_Path_Information
                     then
                        Name_Len := 0;
                        Add_Str_To_Name_Buffer
                          (Get_Name_String
                             (Source_Identity.Project
                              .Library_ALI_Dir.Display_Name));
                        Add_Char_To_Name_Buffer (Directory_Separator);
                        Add_Str_To_Name_Buffer (Get_Name_String (Dep_File));

                        Dep_TS := Unknown_Attributes;
                        if Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
                           Dep_Path := Name_Find;
                        end if;
                     end if;

                     declare
                        Proj : Project_Id :=
                                 Source_Identity.Project.Extended_By;
                     begin
                        while Proj /= No_Project loop
                           Name_Len := 0;

                           if Proj.Library
                             and then
                               Proj.Library_ALI_Dir /= No_Path_Information
                           then
                              Add_Str_To_Name_Buffer
                                (Get_Name_String
                                   (Proj.Library_ALI_Dir.Display_Name));

                           else
                              Add_Str_To_Name_Buffer
                                (Get_Name_String
                                   (Proj.Object_Directory.Display_Name));
                           end if;

                           Add_Char_To_Name_Buffer
                             (Directory_Separator);
                           Add_Str_To_Name_Buffer
                             (Get_Name_String (Dep_File));

                           --  Check if the dependency file exists in
                           --  the extended project, and if it does,
                           --  replace both Dep_Path and Dep_TS with
                           --  the information for it.

                              if Is_Regular_File (Name_Buffer (1 .. Name_Len))
                              then
                                 Dep_Path := Name_Find;
                              end if;

                           Proj := Proj.Extended_By;
                        end loop;
                     end;

                     Stamp := File_Time_Stamp (Dep_Path, Dep_TS'Access);

                     --  Check the time stamp against the binder
                     --  exchange file time stamp.

                     if Stamp = Empty_Time_Stamp then
                        Binder_Driver_Needs_To_Be_Called := True;

                        if Opt.Verbosity_Level > Opt.Low then
                           Put ("      -> cannot find ");
                           Put_Line (Get_Name_String (Dep_Path));
                        end if;

                        exit;

                     elsif Stamp > Bind_Exchange_TS then
                        Binder_Driver_Needs_To_Be_Called := True;

                        if Opt.Verbosity_Level > Opt.Low then
                           Put ("      -> ");
                           Put (Get_Name_String (Dep_Path));
                           Put_Line
                             (" is more recent that the binder "
                              & "exchange file");
                        end if;

                        exit;
                     else
                        Text := Read_Library_Info_From_Full
                          (File_Name_Type (Dep_Path), Dep_TS'Access);

                        if Text /= null then
                           The_ALI :=
                             ALI.Scan_ALI
                               (File_Name_Type (Dep_Path),
                                Text,
                                Ignore_ED     => False,
                                Err           => True,
                                Read_Lines    => "W");
                           Free (Text);

                           Queue.Insert_Withed_Sources_For
                             (The_ALI,
                              Project_Tree,
                              Excluding_Shared_SALs => True);
                        end if;
                     end if;
                  end loop;
               end;
            end;
         end if;

         if not Binder_Driver_Needs_To_Be_Called then
            if Opt.Verbosity_Level > Opt.Low then
               Put_Line ("      -> up to date");
            end if;

         else
            begin
               Create (Exchange_File, Out_File, Bind_Exchange.all);

            exception
               when others =>
                  Fail_Program
                    (Project_Tree,
                     "unable to create binder exchange file " &
                     Bind_Exchange.all);
            end;

            --  Optional line: Quiet or Verbose

            if Opt.Quiet_Output then
               Put_Line (Exchange_File, Binding_Label (Quiet));

            elsif Opt.Verbose_Mode then
               if Opt.Verbosity_Level = Opt.Low then
                  Put_Line (Exchange_File, Binding_Label (Verbose_Low));
               else
                  Put_Line (Exchange_File, Binding_Label (Verbose_Higher));
               end if;
            end if;

            --  If -dn was used, indicate to gprbind that the
            --  temporary response file, if created, should not
            --  deleted.

            if Debug_Flag_N then
               Put_Line (Exchange_File, Binding_Label (Delete_Temp_Files));
               Put_Line (Exchange_File, "False");
            end if;

            --  If there are Stand-Alone Libraries, tell it to gprbind

            if There_Are_Stand_Alone_Libraries then
               Put_Line
                 (Exchange_File,
                  Binding_Label (Gprexch.There_Are_Stand_Alone_Libraries));
            end if;

            --  If the language is Ada, create a binder mapping file
            --  and pass it to gprbind.

            if B_Data.Language_Name = Name_Ada then
               declare
                  Mapping_Path : constant Path_Name_Type :=
                                   Create_Binder_Mapping_File (Project_Tree);

               begin
                  if Mapping_Path /= No_Path then
                     Put_Line
                       (Exchange_File,
                        Binding_Label (Gprexch.Mapping_File));
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (Mapping_Path));
                  end if;
               end;
            end if;

            --  Send the Toolchain Version if there is one for the language

            if B_Data.Language.Config.Toolchain_Version /= No_Name
              or else
                B_Data.Language.Config.Runtime_Library_Version /= No_Name
            then
               Put_Line (Exchange_File, Binding_Label (Toolchain_Version));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (B_Data.Language.Name));

               if B_Data.Language.Config.Runtime_Library_Version /= No_Name
               then
                  Put_Line
                    (Exchange_File,
                     Get_Name_String
                       (B_Data.Language.Config.Runtime_Library_Version));
               else
                  Put_Line
                    (Exchange_File,
                     Get_Name_String
                       (B_Data.Language.Config.Toolchain_Version));
               end if;
            end if;

            --  Send the object file suffix for each language where it
            --  is declared.

            Lang_Index := Main_Proj.Languages;
            Object_File_Suffix_Label_Written := False;

            while Lang_Index /= No_Language_Index loop
               if Lang_Index.Config.Object_File_Suffix /= No_Name then
                  if not Object_File_Suffix_Label_Written then
                     Put_Line
                       (Exchange_File, Binding_Label
                          (Gprexch.Object_File_Suffix));
                     Object_File_Suffix_Label_Written := True;
                  end if;

                  Put_Line
                    (Exchange_File, Get_Name_String (Lang_Index.Name));
                  Put_Line
                    (Exchange_File,
                     Get_Name_String (Lang_Index.Config.Object_File_Suffix));
               end if;

               Lang_Index := Lang_Index.Next;
            end loop;

            --  Optional line: shared libs

            if Shared_Libs then
               Put_Line (Exchange_File, Binding_Label (Gprexch.Shared_Libs));
            end if;

            --  First, the main base name

            Put_Line (Exchange_File, Binding_Label (Gprexch.Main_Base_Name));
            Put_Line (Exchange_File, Get_Name_String (Main_Base_Name_Index));

            --  Then, the compiler path and required switches

            declare
               Config         : Language_Config renames B_Data.Language.Config;
               List           : Name_List_Index;
               Nam_Nod        : Name_Node;
               Previous_Was_X : Boolean := False;
            begin
               --  Compiler path

               Put_Line
                 (Exchange_File, Binding_Label (Gprexch.Compiler_Path));
               Put_Line
                 (Exchange_File,
                  Get_Compiler_Driver_Path
                    (Project_Tree, B_Data.Language).all);

               --  Leading required switches, if any

               List := Config.Compiler_Leading_Required_Switches;
               if List /= No_Name_List then
                  Put_Line
                    (Exchange_File,
                     Binding_Label (Gprexch.Compiler_Leading_Switches));

                  while List /= No_Name_List loop
                     Nam_Nod := Project_Tree.Shared.Name_Lists.Table (List);

                     declare
                        Arg : constant String :=
                          Get_Name_String (Nam_Nod.Name);
                     begin
                        if Opt.CodePeer_Mode then
                           if Previous_Was_X then
                              Put_Line (Exchange_File, "adascil");

                           --  Strip target specific -m switches in CodePeer
                           --  mode.

                           elsif Arg'Length <= 2 or else Arg (1 .. 2) /= "-m"
                           then
                              Put_Line (Exchange_File, Arg);
                           end if;
                        else
                           Put_Line (Exchange_File, Arg);
                        end if;

                        Previous_Was_X := Arg = "-x";
                     end;

                     List := Nam_Nod.Next;
                  end loop;

                  if Opt.CodePeer_Mode then
                     Put_Line (Exchange_File, "-gnatcC");
                  end if;
               end if;

               --  Trailing required switches, if any

               List := Config.Compiler_Trailing_Required_Switches;
               if List /= No_Name_List then
                  Put_Line
                    (Exchange_File,
                     Binding_Label
                       (Gprexch.Compiler_Trailing_Switches));

                  while List /= No_Name_List loop
                     Nam_Nod :=
                       Project_Tree.Shared.Name_Lists.Table (List);
                     Put_Line
                       (Exchange_File, Get_Name_String (Nam_Nod.Name));
                     List := Nam_Nod.Next;
                  end loop;
               end if;
            end;

            --  Then, the Dependency files

            if Main_Source.Unit /= No_Unit_Index then
               Initialize_Source_Record (Main_Source);
               Put_Line
                 (Exchange_File, Binding_Label (Main_Dependency_File));
               Put_Line
                 (Exchange_File, Get_Name_String (Main_Source.Dep_Path));
            end if;

            --  Add the relevant dependency files, either those in
            --  Roots (<main>) for the project, or all dependency
            --  files in the project tree, if Roots (<main>) is not
            --  specified .

            Put_Line (Exchange_File, Binding_Label (Dependency_Files));
            First_Dep := null;

            Add_Dependency_Files
              (Main_Proj, B_Data.Language, Main_Source, Dep_Files);

            while First_Dep /= null loop
               Put_Line (Exchange_File, First_Dep.Name.all);
               First_Dep := First_Dep.Next;
            end loop;

            --  Put the options, if any

            declare
               The_Packages   : constant Package_Id :=
                                  Main_Proj.Decl.Packages;

               Binder_Package : constant GPR.Package_Id :=
                                  GPR.Util.Value_Of
                                    (Name        => Name_Binder,
                                     In_Packages => The_Packages,
                                     Shared      => Project_Tree.Shared);
               Config         : constant Language_Config :=
                                  B_Data.Language.Config;

               Switches    : Variable_Value;
               Switch_List : String_List_Id;
               Element     : String_Element;

            begin
               --  First, check if there are binder options
               --  specified in the main project file.

               if Binder_Package /= No_Package then
                  declare
                     Defaults : constant Array_Element_Id :=
                                  GPR.Util.Value_Of
                                    (Name      => Name_Default_Switches,
                                     In_Arrays =>
                                       Project_Tree.Shared.Packages.Table
                                         (Binder_Package).Decl.Arrays,
                                     Shared    => Project_Tree.Shared);

                     Switches_Array : constant Array_Element_Id :=
                                        GPR.Util.Value_Of
                                          (Name      => Name_Switches,
                                           In_Arrays =>
                                             Project_Tree.Shared.Packages.Table
                                               (Binder_Package).Decl.Arrays,
                                           Shared    => Project_Tree.Shared);

                  begin
                     Switches :=
                       GPR.Util.Value_Of
                         (Index           => Name_Id (Main_Id),
                          Src_Index       => 0,
                          In_Array        => Switches_Array,
                          Shared          => Project_Tree.Shared,
                          Allow_Wildcards => True);

                     if Switches = Nil_Variable_Value then
                        Switches :=
                          GPR.Util.Value_Of
                            (Index                  =>
                                 B_Data.Language_Name,
                             Src_Index              => 0,
                             In_Array               => Switches_Array,
                             Shared                 => Project_Tree.Shared,
                             Force_Lower_Case_Index => True);
                     end if;

                     if Switches = Nil_Variable_Value then
                        Switches :=
                          GPR.Util.Value_Of
                            (Index                  => All_Other_Names,
                             Src_Index              => 0,
                             In_Array               => Switches_Array,
                             Shared                 => Project_Tree.Shared,
                             Force_Lower_Case_Index => True);
                     end if;

                     if Switches = Nil_Variable_Value then
                        Switches :=
                          GPR.Util.Value_Of
                            (Index     => B_Data.Language_Name,
                             Src_Index => 0,
                             In_Array  => Defaults,
                             Shared    => Project_Tree.Shared);
                     end if;
                  end;
               end if;

               --  If there are binder options, either minimum
               --  binder options, or in the main project file or
               --  on the command line, put them in the exchange
               --  file.

               Options_Instance :=
                 Binder_Options_HTable.Get (B_Data.Language_Name);

               if Config.Binder_Required_Switches /= No_Name_List
                 or else Switches.Kind = GPR.List
                 or else All_Language_Binder_Options.Last > 0
                 or else Options_Instance /= No_Bind_Option_Table
                 or else Opt.CodePeer_Mode
                 or else (B_Data.Language_Name = Name_Ada
                          and then Opt.No_Main_Subprogram)
               then
                  Put_Line
                    (Exchange_File, Binding_Label (Gprexch.Binding_Options));

                  --  First, the required switches, if any

                  declare
                     List : Name_List_Index :=
                              Config.Binder_Required_Switches;
                     Elem : Name_Node;

                  begin
                     while List /= No_Name_List loop
                        Elem :=
                          Project_Tree.Shared.Name_Lists.Table (List);
                        Get_Name_String (Elem.Name);

                        if Name_Len > 0 then
                           Put_Line
                             (Exchange_File,
                              Name_Buffer (1 .. Name_Len));
                        end if;

                        List := Elem.Next;
                     end loop;
                  end;

                  --  Then, the eventual options in the main
                  --  project file.

                  if Switches.Kind = GPR.List then
                     declare
                        Option : String_Access;

                     begin
                        Switch_List := Switches.Values;

                        while Switch_List /= Nil_String loop
                           Element :=
                             Project_Tree.Shared.String_Elements.Table
                               (Switch_List);

                           Get_Name_String (Element.Value);

                           if Name_Len > 0 then
                              Option :=
                                new String'
                                  (Name_Buffer (1 .. Name_Len));
                              Test_If_Relative_Path
                                (Option,
                                 Main_Project_Dir.all,
                                 No_Name);
                              Put_Line (Exchange_File, Option.all);
                           end if;

                           Switch_List := Element.Next;
                        end loop;
                     end;
                  end if;

                  --  Then -P if in CodePeer mode

                  if Opt.CodePeer_Mode then
                     Put_Line (Exchange_File, "-P");
                  end if;

                  --  Then those on the command line, for all
                  --  binder drivers, if any.

                  for J in 1 .. All_Language_Binder_Options.Last loop
                     Put_Line
                       (Exchange_File,
                        All_Language_Binder_Options.Table (J).all);
                  end loop;

                  --  Then -z if specified

                  if B_Data.Language_Name = Name_Ada
                    and then Opt.No_Main_Subprogram
                  then
                     Put_Line (Exchange_File, "-z");
                  end if;

                  --  Finally those on the command line for the
                  --  binder driver of the language

                  if Options_Instance /= No_Bind_Option_Table then
                     for Index in 1 .. Binder_Options.Last
                       (Options_Instance.all)
                     loop
                        Put_Line
                          (Exchange_File,
                           Options_Instance.Table (Index).all);
                     end loop;
                  end if;

               end if;
            end;

            if Build_Script_Name /= null then
               Put_Line (Exchange_File, Binding_Label (Script_Path));
               Put_Line (Exchange_File, Build_Script_Name.all);
            end if;

            --  Finally, the list of the project paths with their
            --  time stamps.

            Put_Line (Exchange_File, Binding_Label (Project_Files));

            --  The main project file is always the first one, so that
            --  gprbind may know the main project dir.

            Put_Line
              (Exchange_File,
               Get_Name_String (Main_Proj.Path.Display_Name));

            Put_Line
              (Exchange_File,
               String (File_Stamp (Main_Proj.Path.Display_Name)));

            Proj_List := Main_Proj.All_Imported_Projects;

            while Proj_List /= null loop
               if Main_Proj.Standalone_Library = Encapsulated
                 or else not Proj_List.From_Encapsulated_Lib
               then
                  Put_Line
                    (Exchange_File,
                     Get_Name_String
                       (Proj_List.Project.Path.Display_Name));

                  Put_Line
                    (Exchange_File,
                     String
                       (File_Stamp
                          (Proj_List.Project.Path.Display_Name)));
               end if;

               Proj_List := Proj_List.Next;
            end loop;

            if Main_Source.Unit = No_Unit_Index and then (not Dep_Files) then
               Close (Exchange_File);

               begin
                  Create (Exchange_File, Out_File, Bind_Exchange.all);

               exception
                  when others =>
                     Fail_Program
                       (Project_Tree,
                        "unable to create binder exchange file " &
                          Bind_Exchange.all);
               end;

               Put_Line (Exchange_File, Binding_Label (Nothing_To_Bind));
               Close (Exchange_File);

               if Opt.Verbosity_Level > Opt.Low then
                  Put_Line ("      -> nothing to bind");
               end if;

            else
               Close (Exchange_File);

               if B_Data.Language.Config.Objects_Path /= No_Name then
                  declare
                     Env_Var   : constant String :=
                                   Get_Name_String
                                     (B_Data.Language.Config.
                                                      Objects_Path);
                     Path_Name : String_Access :=
                                   Main_Proj.Objects_Path;
                  begin
                     if Path_Name = null then
                        if Current_Verbosity = High then
                           Put_Line (Env_Var & " :");
                        end if;

                        Get_Directories
                          (Project_Tree => Project_Tree,
                           For_Project  => Main_Proj,
                           Activity     => Executable_Binding,
                           Languages    => No_Names);

                        Path_Name := Create_Path_From_Dirs;
                        Main_Proj.Objects_Path := Path_Name;
                     end if;

                     Setenv (Env_Var, Path_Name.all);

                     if Opt.Verbosity_Level > Opt.Low then
                        Put (Env_Var);
                        Put (" = ");
                        Put_Line (Path_Name.all);
                     end if;
                  end;

               elsif B_Data.Language.Config.Objects_Path_File /= No_Name then
                  declare
                     Env_Var   : constant String :=
                                   Get_Name_String
                                     (B_Data.Language.Config.
                                                      Objects_Path_File);
                     Path_Name : Path_Name_Type :=
                                   Main_Proj.Objects_Path_File_Without_Libs;
                  begin
                     if Path_Name = No_Path then
                        if Current_Verbosity = High then
                           Put_Line (Env_Var & " :");
                        end if;

                        Get_Directories
                          (Project_Tree => Project_Tree,
                           For_Project  => Main_Proj,
                           Activity     => Executable_Binding,
                           Languages    => No_Names);

                        declare
                           FD     : File_Descriptor;
                           Len    : Integer;
                           Status : Boolean;
                        begin
                           GPR.Env.Create_New_Path_File
                             (Shared    => Project_Tree.Shared,
                              Path_FD   => FD,
                              Path_Name =>
                                Main_Proj.Objects_Path_File_Without_Libs);

                           if FD = Invalid_FD then
                              Fail_Program
                                (Project_Tree,
                                 "could not create temporary path file");
                           end if;

                           Path_Name :=
                             Main_Proj.Objects_Path_File_Without_Libs;

                           for Index in
                             1 .. Gpr_Build_Util.Directories.Last
                           loop
                              Get_Name_String
                                (Gpr_Build_Util.Directories.Table (Index));

                              if Current_Verbosity = High then
                                 Put_Line (Name_Buffer (1 .. Name_Len));
                              end if;

                              Name_Len := Name_Len + 1;
                              Name_Buffer (Name_Len) := ASCII.LF;

                              Len :=
                                Write (FD, Name_Buffer (1)'Address, Name_Len);

                              if Len /= Name_Len then
                                 Fail_Program (Project_Tree, "disk full");
                              end if;
                           end loop;

                           Close (FD, Status);

                           if not Status then
                              Fail_Program (Project_Tree, "disk full");
                           end if;
                        end;
                     end if;

                     Setenv (Env_Var, Get_Name_String (Path_Name));

                     if Opt.Verbosity_Level > Opt.Low then
                        Put (Env_Var);
                        Put (" = ");
                        Put_Line (Get_Name_String (Path_Name));
                     end if;
                  end;
               end if;

               if not Opt.Quiet_Output then
                  if Opt.Verbose_Mode then
                     Name_Len := 0;
                     Add_Str_To_Name_Buffer (B_Data.Binder_Driver_Path.all);
                     Add_Str_To_Name_Buffer (" ");
                     Add_Str_To_Name_Buffer (Bind_Exchange.all);
                     Put_Line (Name_Buffer (1 .. Name_Len));

                  else
                     Display
                       (Section  => GPR.Bind,
                        Command  => Ada.Directories.Base_Name
                                     (Ada.Directories.Simple_Name
                                       (B_Data.Binder_Driver_Path.all)),
                        Argument => Bind_Exchange.all);
                  end if;
               end if;

               declare
                  Pid : Process_Id;
               begin
                  Pid := Non_Blocking_Spawn
                    (B_Data.Binder_Driver_Path.all, (1 => Bind_Exchange));

                  if Pid = Invalid_Pid then
                     Record_Failure (Main_File);

                  else
                     Add_Process (Pid, (Binding, Pid, Main_File));

                     Display_Processes ("bind");
                  end if;
               end;
            end if;
         end if;
      end Bind_Language;

      -----------------------------
      -- Wait_For_Available_Slot --
      -----------------------------

      procedure Wait_For_Available_Slot is
         Data : Process_Data;
         OK   : Boolean;
      begin
         while Outstanding_Processes >= Opt.Maximum_Processes loop
            Await_Process (Data, OK);

            if not OK then
               Record_Failure (Data.Main);
            end if;

            Display_Processes ("bind");
         end loop;
      end Wait_For_Available_Slot;

   --  Start of processing for Post_Compilation_Phase

   begin
      --  Build the libraries, if any

      --  First, get the libraries in building order in table Library_Projs

      if not Opt.CodePeer_Mode then
         Process_Imported_Libraries
           (Main_Project,
            There_Are_SALs     => There_Are_Stand_Alone_Libraries,
            And_Project_Itself => True);

         if Library_Projs.Last > 0 then
            declare
               Lib_Projs : array (1 .. Library_Projs.Last) of Library_Project;
               Proj      : Library_Project;

            begin
               --  Copy the list of library projects in local array Lib_Projs,
               --  as procedure Build_Library uses table Library_Projs.

               for J in 1 .. Library_Projs.Last loop
                  Lib_Projs (J) := Library_Projs.Table (J);
               end loop;

               for J in Lib_Projs'Range loop
                  Proj := Lib_Projs (J);

                  if not Proj.Is_Aggregated then
                     --  Try building a library only if no errors occured in
                     --  library project and projects it depends on.

                     if not Project_Compilation_Failed (Proj.Proj) then
                        if Proj.Proj.Extended_By = No_Project then
                           if not Proj.Proj.Externally_Built then
                              Build_Library
                                (Proj.Proj, Project_Tree,
                                 No_Create => Proj.Is_Aggregated);
                           end if;

                           Shared_Libs := not Is_Static (Proj.Proj);

                        end if;
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end if;

      --  If no main is specified, there is nothing else to do

      if Mains.Number_Of_Mains (Project_Tree) = 0 then
         return;
      end if;

      --  Check if there is a need to call a binder driver

      Find_Binding_Languages (Project_Tree, Main_Project);

      --  Proceed to bind (or rebind if needed) for each main

      Mains.Reset;

      loop
         declare
            Main_File : Main_Info;
         begin
            Main_File := Mains.Next_Main;
            exit when Main_File = No_Main_Info;

            if Main_File.Tree /= Project_Tree
              or else Project_Compilation_Failed (Main_File.Project)
            then
               --  Will be processed later, or do not need any processing in
               --  the case of compilation errors in the project.
               null;

            elsif
               not Builder_Data (Main_File.Tree).There_Are_Binder_Drivers
            then
               if Current_Verbosity = High then
                  Debug_Output ("Post-compilation, no binding required for",
                                Debug_Name (Main_File.Tree));
               end if;

            else
               declare
                  Main                 : constant String :=
                                           Get_Name_String (Main_File.File);
                  Main_Id              : constant File_Name_Type :=
                                           Create_Name (Base_Name (Main));
                  Main_Index           : constant Int := Main_File.Index;
                  B_Data               : Binding_Data;
                  Main_Base_Name_Index : File_Name_Type;
                  Main_Proj            : Project_Id;
                  Index_Separator      : Character;

               begin
                  Main_Proj := Ultimate_Extending_Project_Of
                    (Main_File.Source.Project);

                  --  Get the main base name-index name

                  Index_Separator :=
                    Main_File.Source.Language
                      .Config.Multi_Unit_Object_Separator;

                  Main_Base_Name_Index :=
                    Base_Name_Index_For (Main, Main_Index, Index_Separator);

                  B_Data := Builder_Data (Main_File.Tree).Binding;
                  while B_Data /= null loop
                     Wait_For_Available_Slot;
                     exit when Stop_Spawning;
                     Change_To_Object_Directory (Main_Proj);
                     Bind_Language
                       (Main_Proj, Main, Main_Base_Name_Index,
                        Main_File, Main_Id, B_Data);
                     exit when Stop_Spawning;
                     B_Data := B_Data.Next;
                  end loop;
               end;
            end if;
         end;
      end loop;
   end Post_Compilation_Phase;

end Gprbuild.Post_Compile;
