------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2011-2016, AdaCore                     --
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

with Ada.Directories;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Deallocation; use Ada;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Gpr_Build_Util; use Gpr_Build_Util;
with Gpr_Script;     use Gpr_Script;
with Gpr_Util;       use Gpr_Util;
with Gprexch;        use Gprexch;
with GPR.Debug;      use GPR.Debug;
with GPR.Names;      use GPR.Names;
with GPR.Snames;     use GPR.Snames;
with GPR.Util;       use GPR.Util;

package body Gprbuild.Link is

   type Archive_Data is record
      Checked        : Boolean := False;
      Has_Been_Built : Boolean := False;
      Exists         : Boolean := False;
   end record;

   type Source_Index_Rec is record
      Project : Project_Id;
      Id      : Source_Id;
      Found   : Boolean := False;
   end record;
   --  Used as Source_Indexes component to check if archive needs to be rebuilt

   type Source_Index_Array is array (Positive range <>) of Source_Index_Rec;
   type Source_Indexes_Ref is access Source_Index_Array;

   procedure Free is new Unchecked_Deallocation
     (Source_Index_Array, Source_Indexes_Ref);

   Initial_Source_Index_Count : constant Positive := 20;
   Source_Indexes : Source_Indexes_Ref :=
     new Source_Index_Array (1 .. Initial_Source_Index_Count);
   --  A list of the Source_Ids, with an indication that they have been found
   --  in the archive dependency file.

   procedure Build_Global_Archive
     (For_Project    : Project_Id;
      Project_Tree   : Project_Tree_Ref;
      Has_Been_Built : out Boolean;
      Exists         : out Boolean;
      OK             : out Boolean);
   --  Build, if necessary, the global archive for a main project.
   --  Out parameter Has_Been_Built is True iff the global archive has been
   --  built/rebuilt. Exists is False if there is no need for a global archive.
   --  OK is False when there is a problem building the global archive.

   procedure Link_Main (Main_File : Main_Info);
   --  Link a specific main unit

   procedure Get_Linker_Options (For_Project : Project_Id);
   --  Get the Linker_Options from a project

   procedure Add_Rpath (Path : String);
   --  Add a path name to Rpath

   procedure Rpaths_Relative_To
     (Exec_Dir : Path_Name_Type; Origin : Name_Id);
   --  Change all paths in table Rpaths to paths relative to Exec_Dir, if they
   --  have at least one non root directory in common.

   function Is_In_Library_Project (Object_Path : String) return Boolean;
   --  Return True if Object_Path is the path of an object file in a library
   --  project.

   procedure Display_Command
     (Path    : String_Access;
      Ellipse : Boolean := False);
   --  Display the command for a spawned process, if in Verbose_Mode or not in
   --  Quiet_Output. In non verbose mode, when Ellipse is True, display "..."
   --  in place of the first argument that has Display set to False.

   procedure Add_Argument
     (Arg         : String_Access;
      Display     : Boolean;
      Simple_Name : Boolean := False);
   procedure Add_Argument
     (Arg         : String;
      Display     : Boolean;
      Simple_Name : Boolean := False);
   --  Add an argument to Arguments. Reallocate if necessary

   procedure Add_Arguments
     (Args        : Argument_List;
      Display     : Boolean;
      Simple_Name : Boolean := False);
   --  Add a list of arguments to Arguments. Reallocate if necessary

   No_Archive_Data : constant Archive_Data :=
                       (Checked        => False,
                        Has_Been_Built => False,
                        Exists         => False);

   package Global_Archives_Built is new GNAT.HTable.Simple_HTable
     (Header_Num => GPR.Header_Num,
      Element    => Archive_Data,
      No_Element => No_Archive_Data,
      Key        => Name_Id,
      Hash       => GPR.Hash,
      Equal      => "=");
   --  A hash table to record what global archives have been already built

   package Cache_Args is new GNAT.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100);
   --  A table to cache arguments, to avoid multiple allocation of the same
   --  strings. It is not possible to use a hash table, because String is
   --  an unconstrained type.

   package Rpaths is new GNAT.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 50);
   --  Directories to be put in the run path option

   package Library_Dirs is new GNAT.HTable.Simple_HTable
     (Header_Num => GPR.Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Path_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to store the library dirs, to avoid repeating uselessly
   --  the same switch when linking executables.

   Last_Source : Natural := 0;
   --  The index of the last valid component of Source_Indexes

   Initial_Argument_Count : constant Positive := 20;
   Arguments : Argument_List_Access :=
                 new Argument_List (1 .. Initial_Argument_Count);
   --  Used to store lists of arguments to be used when spawning a process

   Arguments_Displayed : Booleans :=
                           new Boolean_Array (1 .. Initial_Argument_Count);
   --  For each argument in Arguments, indicate if the argument should be
   --  displayed when procedure Display_Command is called.

   Arguments_Simple_Name : Booleans :=
                             new Boolean_Array (1 .. Initial_Argument_Count);
   --  For each argument that should be displayed, indicate that the argument
   --  is a path name and that only the simple name should be displayed.

   Last_Argument : Natural := 0;
   --  Index of the last valid argument in Arguments

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument
     (Arg         : String_Access;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      --  Nothing to do if no argument is specified or if argument is empty

      if Arg /= null and then Arg'Length /= 0 then

         --  Reallocate arrays if necessary

         if Last_Argument = Arguments'Last then
            declare
               New_Arguments : constant Argument_List_Access :=
                                 new Argument_List
                                   (1 .. Last_Argument +
                                           Initial_Argument_Count);

               New_Arguments_Displayed : constant Booleans :=
                                           new Boolean_Array
                                             (1 .. Last_Argument +
                                                     Initial_Argument_Count);

               New_Arguments_Simple_Name : constant Booleans :=
                                             new Boolean_Array
                                               (1 .. Last_Argument +
                                                       Initial_Argument_Count);

            begin
               New_Arguments (Arguments'Range) := Arguments.all;

               --  To avoid deallocating the strings, nullify all components
               --  of Arguments before calling Free.

               Arguments.all := (others => null);

               Free (Arguments);
               Arguments := New_Arguments;

               New_Arguments_Displayed (Arguments_Displayed'Range) :=
                 Arguments_Displayed.all;
               Free (Arguments_Displayed);
               Arguments_Displayed := New_Arguments_Displayed;

               New_Arguments_Simple_Name (Arguments_Simple_Name'Range) :=
                 Arguments_Simple_Name.all;
               Free (Arguments_Simple_Name);
               Arguments_Simple_Name := New_Arguments_Simple_Name;
            end;
         end if;

         --  Add the argument and its display indication

         Last_Argument := Last_Argument + 1;
         Arguments (Last_Argument) := Arg;
         Arguments_Displayed (Last_Argument) := Display;
         Arguments_Simple_Name (Last_Argument) := Simple_Name;
      end if;
   end Add_Argument;

   procedure Add_Argument
     (Arg         : String;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
      Argument : String_Access := null;

   begin
      --  Nothing to do if argument is empty

      if Arg'Length > 0 then

         --  Check if the argument is already in the Cache_Args table. If it is
         --  already there, reuse the allocated value.

         for Index in 1 .. Cache_Args.Last loop
            if Cache_Args.Table (Index).all = Arg then
               Argument := Cache_Args.Table (Index);
               exit;
            end if;
         end loop;

         --  If the argument is not in the cache, create a new entry in the
         --  cache.

         if Argument = null then
            Argument := new String'(Arg);
            Cache_Args.Increment_Last;
            Cache_Args.Table (Cache_Args.Last) := Argument;
         end if;

         --  And add the argument

         Add_Argument (Argument, Display, Simple_Name);
      end if;
   end Add_Argument;

   -------------------
   -- Add_Arguments --
   -------------------

   procedure Add_Arguments
     (Args        : Argument_List;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      --  Reallocate the arrays, if necessary

      if Last_Argument + Args'Length > Arguments'Last then
         declare
            New_Arguments : constant Argument_List_Access :=
                              new Argument_List
                                    (1 .. Last_Argument + Args'Length +
                                          Initial_Argument_Count);

            New_Arguments_Displayed : constant Booleans :=
                                        new Boolean_Array
                                              (1 .. Last_Argument +
                                                    Args'Length +
                                                    Initial_Argument_Count);

         begin
            New_Arguments (1 .. Last_Argument) :=
              Arguments (1 .. Last_Argument);

            --  To avoid deallocating the strings, nullify all components
            --  of Arguments before calling Free.

            Arguments.all := (others => null);
            Free (Arguments);

            Arguments := New_Arguments;
            New_Arguments_Displayed (1 .. Last_Argument) :=
              Arguments_Displayed (1 .. Last_Argument);
            Free (Arguments_Displayed);
            Arguments_Displayed := New_Arguments_Displayed;
         end;
      end if;

      --  Add the new arguments and the display indications

      Arguments (Last_Argument + 1 .. Last_Argument + Args'Length) := Args;
      Arguments_Displayed (Last_Argument + 1 .. Last_Argument + Args'Length) :=
        (others => Display);
      Arguments_Simple_Name (Last_Argument + 1 .. Last_Argument + Args'Length)
        := (others => Simple_Name);
      Last_Argument := Last_Argument + Args'Length;
   end Add_Arguments;

   ---------------
   -- Add_Rpath --
   ---------------

   procedure Add_Rpath (Path : String) is
   begin
      --  Nothing to do if Path is empty

      if Path'Length > 0 then
         --  Nothing to do if the directory is already in the Rpaths table

         for J in 1 .. Rpaths.Last loop
            if Rpaths.Table (J).all = Path then
               return;
            end if;
         end loop;

         Rpaths.Append (new String'(Path));
      end if;
   end Add_Rpath;

   --------------------------
   -- Build_Global_Archive --
   --------------------------

   procedure Build_Global_Archive
     (For_Project    : Project_Id;
      Project_Tree   : Project_Tree_Ref;
      Has_Been_Built : out Boolean;
      Exists         : out Boolean;
      OK             : out Boolean)
   is
      Archive_Name : constant String :=
                       "lib"
                       & Get_Name_String (For_Project.Name)
                       & Archive_Suffix (For_Project);
      --  The name of the archive file for this project

      Archive_Dep_Name : constant String :=
                           "lib"
                           & Get_Name_String (For_Project.Name) & ".deps";
      --  The name of the archive dependency file for this project

      File : GPR.Util.Text_File;

      Object_Path  : Path_Name_Type;
      Time_Stamp   : Time_Stamp_Type;

      First_Object : Natural;

      Discard : Boolean;

      Proj_List    : Project_List;

      Src_Id       : Source_Id;
      S_Id         : Source_Id;

      Success      : Boolean;

      Real_Last_Argument : Positive;
      Current_Object_Pos : Positive;

      Size : Natural;

      Global_Archive_Data : Archive_Data;

      Need_To_Build : Boolean;

      procedure Add_Sources (Proj : Project_Id);
      --  Add all the sources of project Proj to Sources_Index

      procedure Add_Objects (Proj : Project_Id);
      --  Add all the object paths of project Proj to Arguments

      -----------------
      -- Add_Sources --
      -----------------

      procedure Add_Sources (Proj : Project_Id) is
         Project : Project_Id := Proj;
         Id      : Source_Id;
         Iter    : Source_Iterator;

         procedure Add_Source_Id (Project : Project_Id; Id : Source_Id);
         --  Add a source id to Source_Indexes, with Found set to False

         -------------------
         -- Add_Source_Id --
         -------------------

         procedure Add_Source_Id (Project : Project_Id; Id : Source_Id) is
         begin
            --  Reallocate the array, if necessary

            if Last_Source = Source_Indexes'Last then
               declare
                  New_Indexes : constant Source_Indexes_Ref :=
                                  new Source_Index_Array
                                    (1 .. Source_Indexes'Last +
                                                   Initial_Source_Index_Count);
               begin
                  New_Indexes (Source_Indexes'Range) := Source_Indexes.all;
                  Free (Source_Indexes);
                  Source_Indexes := New_Indexes;
               end;
            end if;

            Last_Source := Last_Source + 1;
            Source_Indexes (Last_Source) := (Project, Id, False);
         end Add_Source_Id;

      begin
         while Project /= No_Project loop
            Iter := For_Each_Source (Project_Tree, Project);
            loop
               Id := GPR.Element (Iter);
               exit when Id = No_Source;

               if Is_Compilable (Id)
                 and then Id.Kind = Impl
                 and then Id.Unit = No_Unit_Index
               then
                  Add_Source_Id (Proj, Id);
               end if;

               Next (Iter);
            end loop;

            Project := Project.Extends;
         end loop;
      end Add_Sources;

      -----------------
      -- Add_Objects --
      -----------------

      procedure Add_Objects (Proj : Project_Id) is
         Project : Project_Id := Proj;
         Id      : Source_Id;
         Iter    : Source_Iterator;

         type Arg_Record;
         type Arg_Access is access Arg_Record;
         type Arg_Record is record
            Path    : Name_Id;
            Display : Boolean;
            Simple  : Boolean;
            Next    : Arg_Access;
         end record;

         First_Arg : Arg_Access := null;
         --  Head of the list of arguments in ascending order of paths

         procedure Add
           (Path    : String;
            Display : Boolean;
            Simple  : Boolean);
         --  Ad an argument in the list in the correct order

         ---------
         -- Add --
         ---------

         procedure Add
           (Path    : String;
            Display : Boolean;
            Simple  : Boolean)
         is
            Arg_Ptr : Arg_Access;
            Current : Arg_Access;

         begin
            Name_Len := 0;
            Add_Str_To_Name_Buffer (Path);
            Arg_Ptr := new Arg_Record'(Name_Find, Display, Simple, null);

            if First_Arg = null
              or else Path < Get_Name_String (First_Arg.Path)
            then
               Arg_Ptr.Next := First_Arg;
               First_Arg := Arg_Ptr;

            else
               Current := First_Arg;
               while Current.Next /= null
                 and then Path > Get_Name_String (Current.Next.Path)
               loop
                  Current := Current.Next;
               end loop;

               Arg_Ptr.Next := Current.Next;
               Current.Next := Arg_Ptr;
            end if;
         end Add;

      begin
         loop
            if Project.Object_Directory /= No_Path_Information then
               if Project.Externally_Built then
                  --  If project is externally built, include all object files
                  --  in the object directory in the global archive.

                  declare
                     Obj_Dir : constant String :=
                                 Get_Name_String
                                   (Project.Object_Directory.Display_Name);
                     Dir_Obj : Dir_Type;

                  begin
                     if Is_Regular_File (Obj_Dir) then
                        Open (Dir_Obj, Obj_Dir);

                        loop
                           Read (Dir_Obj, Name_Buffer, Name_Len);
                           exit when Name_Len = 0;

                           Canonical_Case_File_Name
                             (Name_Buffer (1 .. Name_Len));

                           if Name_Len > Object_Suffix'Length
                             and then
                               Name_Buffer
                                 (Name_Len - Object_Suffix'Length + 1
                                  .. Name_Len) = Object_Suffix
                           then
                              Add
                                (Obj_Dir & Directory_Separator &
                                   Name_Buffer (1 .. Name_Len),
                                 Opt.Verbose_Mode,
                                 Simple => not Opt.Verbose_Mode);
                           end if;
                        end loop;

                        Close (Dir_Obj);
                     end if;
                  end;

               else
                  Iter := For_Each_Source (Project_Tree, Project);
                  loop
                     Id := GPR.Element (Iter);
                     exit when Id = No_Source;

                     if Object_To_Global_Archive (Id) then
                        --  The source record may not be initialized if
                        --  gprbuild was called with the switch -l.

                        Initialize_Source_Record (Id);

                        Add
                          (Get_Name_String (Id.Object_Path),
                           Opt.Verbose_Mode,
                           Simple => not Opt.Verbose_Mode);
                     end if;

                     Next (Iter);
                  end loop;
               end if;
            end if;

            Project := Project.Extends;

            exit when Project = No_Project;
         end loop;

         --  Add the object files in the arguments in acending order of paths
         --  so that the global archive is always built the same way.

         while First_Arg /= null loop
            Add_Argument
              (Get_Name_String (First_Arg.Path),
               First_Arg.Display,
               First_Arg.Simple);
            First_Arg := First_Arg.Next;
         end loop;

      end Add_Objects;

   begin
      Exists := False;
      Has_Been_Built := False;
      OK := True;

      --  No need to build the global archive, if it has already been done

      if For_Project.Object_Directory /= No_Path_Information then
         Global_Archive_Data :=
           Global_Archives_Built.Get (Name_Id (For_Project.Path.Name));

         if Global_Archive_Data.Checked then
            Exists         := Global_Archive_Data.Exists;
            Has_Been_Built := Global_Archive_Data.Has_Been_Built;

         else
            Change_To_Object_Directory (For_Project);

            --  Put all non Ada sources in the project tree in Source_Indexes

            Last_Source := 0;

            Add_Sources (For_Project);

            Proj_List := For_Project.All_Imported_Projects;

            while Proj_List /= null loop
               if not Proj_List.Project.Library then
                  Add_Sources (Proj_List.Project);
               end if;

               Proj_List := Proj_List.Next;
            end loop;

            Need_To_Build := Opt.Force_Compilations;

            if not Need_To_Build then
               if Opt.Verbosity_Level > Opt.Low then
                  Put  ("   Checking ");
                  Put  (Archive_Name);
                  Put_Line (" ...");
               end if;

               --  If the archive does not exist, of course it needs to be
               --  built.

               if not Is_Regular_File (Archive_Name) then
                  Need_To_Build := True;

                  if Opt.Verbosity_Level > Opt.Low then
                     Put_Line ("      -> archive does not exist");
                  end if;

               else
                  --  Archive does exist

                  --  Check the archive dependency file

                  Open (File, Archive_Dep_Name);

                  --  If the archive dependency file does not exist, we need to
                  --  to rebuild the archive and to create its dependency file.

                  if not Is_Valid (File) then
                     Need_To_Build := True;

                     if Opt.Verbosity_Level > Opt.Low then
                        Put  ("      -> archive dependency file ");
                        Put  (Archive_Dep_Name);
                        Put_Line (" does not exist");
                     end if;

                  else
                     --  Read the dependency file, line by line

                     while not End_Of_File (File) loop
                        Get_Line (File, Name_Buffer, Name_Len);

                        --  First line is the path of the object file

                        Object_Path := Name_Find;
                        Src_Id := No_Source;

                        --  Check if this object file is for a source of this
                        --  project.

                        for S in 1 .. Last_Source loop
                           S_Id := Source_Indexes (S).Id;

                           if not Source_Indexes (S).Found
                             and then S_Id.Object_Path = Object_Path
                           then
                              --  We have found the object file: get the
                              --  source data, and mark it as found.

                              Src_Id := S_Id;
                              Source_Indexes (S).Found := True;
                              exit;
                           end if;
                        end loop;

                        --  If it is not for a source of this project, then the
                        --  archive needs to be rebuilt.

                        if Src_Id = No_Source then
                           Need_To_Build := True;
                           if Opt.Verbosity_Level > Opt.Low then
                              Put  ("      -> ");
                              Put  (Get_Name_String (Object_Path));
                              Put_Line (" is not an object of any project");
                           end if;

                           exit;
                        end if;

                        --  The second line is the time stamp of the object
                        --  file. If there is no next line, then the dependency
                        --  file is truncated, and the archive need to be
                        --  rebuilt.

                        if End_Of_File (File) then
                           Need_To_Build := True;

                           if Opt.Verbosity_Level > Opt.Low then
                              Put  ("      -> archive dependency file ");
                              Put_Line (" is truncated");
                           end if;

                           exit;
                        end if;

                        Get_Line (File, Name_Buffer, Name_Len);

                        --  If the line has the wrong number of characters,
                        --  then the dependency file is incorrectly formatted,
                        --  and the archive needs to be rebuilt.

                        if Name_Len /= Time_Stamp_Length then
                           Need_To_Build := True;

                           if Opt.Verbosity_Level > Opt.Low then
                              Put  ("      -> archive dependency file ");
                              Put_Line
                                (" is incorrectly formatted (time stamp)");
                           end if;

                           exit;
                        end if;

                        Time_Stamp :=
                          Time_Stamp_Type (Name_Buffer (1 .. Name_Len));

                        --  If the time stamp in the dependency file is
                        --  different from the time stamp of the object file,
                        --  then the archive needs to be rebuilt. The
                        --  comparaison is done with String type values,
                        --  because two values of type Time_Stamp_Type are
                        --  equal if they differ by 2 seconds or less; here the
                        --  check is for an exact match.

                        if String (Time_Stamp) /=
                          String (Src_Id.Object_TS)
                        then
                           Need_To_Build := True;

                           if Opt.Verbosity_Level > Opt.Low  then
                              Put  ("      -> time stamp of ");
                              Put  (Get_Name_String (Object_Path));
                              Put  (" is incorrect in the archive");
                              Put_Line (" dependency file");
                              Put  ("         recorded time stamp: ");
                              Put_Line (String (Time_Stamp));
                              Put  ("           actual time stamp: ");
                              Put_Line (String (Src_Id.Object_TS));
                           end if;

                           exit;

                        elsif Debug_Flag_T then
                           Put  ("      -> time stamp of ");
                           Put  (Get_Name_String (Object_Path));
                           Put  (" is correct in the archive");
                           Put_Line (" dependency file");
                           Put  ("         recorded time stamp: ");
                           Put_Line (String (Time_Stamp));
                           Put  ("           actual time stamp: ");
                           Put_Line (String (Src_Id.Object_TS));
                        end if;
                     end loop;

                     Close (File);
                  end if;
               end if;
            end if;

            if not Need_To_Build then
               for S in 1 .. Last_Source loop
                  if not Source_Indexes (S).Found
                    and then Object_To_Global_Archive (Source_Indexes (S).Id)
                  then
                     Need_To_Build := True;

                     if Opt.Verbosity_Level > Opt.Low then
                        Put ("      -> object file ");
                        Put (Get_Name_String
                                   (Source_Indexes (S).Id.Object_Path));
                        Put_Line (" is not in the dependency file");
                     end if;

                     exit;
                  end if;
               end loop;
            end if;

            if not Need_To_Build then
               if Opt.Verbosity_Level > Opt.Low then
                  Put_Line  ("      -> up to date");
               end if;

               Exists         := True;
               Has_Been_Built := False;

               --  Archive needs to be rebuilt

            else
               Check_Archive_Builder;

               --  If archive already exists, first delete it, but if this is
               --  not possible, continue: if archive cannot be built, we will
               --  fail later on.

               if Is_Regular_File (Archive_Name) then
                  Delete_File (Archive_Name, Discard);
               end if;

               Last_Argument := 0;

               --  Start with the minimal options

               Add_Arguments
                 (Archive_Builder_Opts.Options
                    (1 .. Archive_Builder_Opts.Last),
                  True);

               --  Followed by the archive name

               Add_Argument
                 (Archive_Name, True, Simple_Name => not Opt.Verbose_Mode);

               First_Object := Last_Argument + 1;

               --  Followed by all the object files of the non library projects

               Add_Objects (For_Project);

               Proj_List := For_Project.All_Imported_Projects;

               while Proj_List /= null loop
                  if not Proj_List.Project.Library then
                     Add_Objects (Proj_List.Project);
                  end if;

                  Proj_List := Proj_List.Next;
               end loop;

               --  No global archive, if there is no object file to put into

               if Last_Argument < First_Object then
                  Has_Been_Built := False;
                  Exists         := False;

                  if Opt.Verbosity_Level > Opt.Low
                  then
                     Put_Line ("      -> there is no global archive");
                  end if;

               else
                  Real_Last_Argument := Last_Argument;

                  --  If there is an Archive_Builder_Append_Option, we may have
                  --  to build the archive in chuck.

                  if Archive_Builder_Append_Opts.Last = 0 then
                     Current_Object_Pos := Real_Last_Argument + 1;

                  else
                     Size := 0;
                     for J in 1 .. First_Object - 1 loop
                        Size := Size + Arguments (J)'Length + 1;
                     end loop;

                     Current_Object_Pos := First_Object;
                     while Current_Object_Pos <= Real_Last_Argument loop
                        Size :=
                          Size + Arguments (Current_Object_Pos)'Length + 1;
                        exit when Size > Maximum_Size;
                        Current_Object_Pos := Current_Object_Pos + 1;
                     end loop;

                     Last_Argument := Current_Object_Pos - 1;
                  end if;

                  if not Opt.Quiet_Output then
                     if Opt.Verbose_Mode then
                        Display_Command
                          (Archive_Builder_Path,
                           Ellipse => True);

                     else
                        Display
                          (Section  => GPR.Link,
                           Command  => "archive",
                           Argument => Archive_Name);
                     end if;
                  end if;

                  Spawn_And_Script_Write
                    (Archive_Builder_Path.all,
                     Arguments (1 .. Last_Argument),
                     Success);

                  --  If the archive has not been built completely, add the
                  --  remaining chunks.

                  if Success
                    and then Current_Object_Pos <= Real_Last_Argument
                  then
                     Last_Argument := 0;
                     Add_Arguments
                       (Archive_Builder_Append_Opts.Options
                          (1 .. Archive_Builder_Append_Opts.Last),
                        True);
                     Add_Argument
                       (Archive_Name, True,
                        Simple_Name => not Opt.Verbose_Mode);
                     First_Object := Last_Argument + 1;

                     while Current_Object_Pos <= Real_Last_Argument loop
                        Size := 0;
                        for J in 1 .. First_Object - 1 loop
                           Size := Size + Arguments (J)'Length + 1;
                        end loop;

                        Last_Argument := First_Object - 1;

                        while Current_Object_Pos <= Real_Last_Argument loop
                           Size :=
                             Size + Arguments (Current_Object_Pos)'Length + 1;
                           exit when Size > Maximum_Size;
                           Last_Argument := Last_Argument + 1;
                           Arguments (Last_Argument) :=
                             Arguments (Current_Object_Pos);
                           Current_Object_Pos := Current_Object_Pos + 1;
                        end loop;

                        if Opt.Verbose_Mode then
                           Display_Command
                             (Archive_Builder_Path,
                              Ellipse => True);
                        end if;

                        Spawn_And_Script_Write
                          (Archive_Builder_Path.all,
                           Arguments (1 .. Last_Argument),
                           Success);

                        exit when not Success;
                     end loop;
                  end if;

                  --  If the archive was built, run the archive indexer
                  --  (ranlib) if there is one.

                  if Success then

                     --  If the archive was built, run the archive indexer
                     --  (ranlib), if there is one.

                     if Archive_Indexer_Path /= null then
                        Last_Argument := 0;
                        Add_Arguments
                          (Archive_Indexer_Opts.Options
                             (1 .. Archive_Indexer_Opts.Last),
                           True);
                        Add_Argument
                          (Archive_Name,
                           True,
                           Simple_Name => not Opt.Verbose_Mode);

                        if not Opt.Quiet_Output then
                           if Opt.Verbose_Mode then
                              Display_Command
                                (Archive_Indexer_Path);

                           else
                              Display
                                (Section  => GPR.Link,
                                 Command  => "index",
                                 Argument => Archive_Name);
                           end if;
                        end if;

                        Spawn_And_Script_Write
                          (Archive_Indexer_Path.all,
                           Arguments (1 .. Last_Argument),
                           Success);

                        if not Success then

                           --  Running the archive indexer failed, delete the
                           --  dependency file, if it exists.

                           if Is_Regular_File (Archive_Dep_Name) then
                              Delete_File (Archive_Dep_Name, Success);
                           end if;
                        end if;
                     end if;
                  end if;

                  if Success then
                     --  The archive was correctly built, create its dependency
                     --  file.

                     declare
                        Dep_File : Text_IO.File_Type;

                     begin
                        --  Create the file in Append mode, to avoid automatic
                        --  insertion of an end of line if file is empty.

                        Create (Dep_File, Append_File, Archive_Dep_Name);

                        for S in 1 .. Last_Source loop
                           Src_Id := Source_Indexes (S).Id;
                           if Object_To_Global_Archive (Src_Id) then
                              Put_Line
                                (Dep_File,
                                 Get_Name_String (Src_Id.Object_Path));
                              Put_Line (Dep_File, String (Src_Id.Object_TS));
                           end if;
                        end loop;

                        Close (Dep_File);

                     exception
                        when others =>
                           if Is_Open (Dep_File) then
                              Close (Dep_File);
                           end if;
                     end;

                     Has_Been_Built := True;
                     Exists         := True;

                  else
                     --  Building the archive failed, delete dependency file if
                     --  one exists.

                     if Is_Regular_File (Archive_Dep_Name) then
                        Delete_File (Archive_Dep_Name, Success);
                     end if;

                     Put ("global archive for project ");
                     Put
                       (Get_Name_String (For_Project.Display_Name));
                     Put_Line (" could not be built");
                     OK := False;
                     return;
                  end if;
               end if;
            end if;

            Global_Archives_Built.Set
              (Name_Id (For_Project.Path.Name),
               (Checked        => True,
                Has_Been_Built => Has_Been_Built,
                Exists         => Exists));
         end if;
      end if;
   end Build_Global_Archive;

   ---------------------
   -- Display_Command --
   ---------------------

   procedure Display_Command
     (Path          : String_Access;
      Ellipse       : Boolean := False)
   is
      Display_Ellipse : Boolean := Ellipse;
   begin
      --  Only display the command in Verbose Mode (-v) or when
      --  not in Quiet Output (no -q).

      if not Opt.Quiet_Output then
         Name_Len := 0;

         if Opt.Verbose_Mode then
            if Opt.Verbosity_Level = Opt.Low then
               Add_Str_To_Name_Buffer
                 (Ada.Directories.Base_Name (Path.all));
            else
               Add_Str_To_Name_Buffer (Path.all);
            end if;

            for Arg in 1 .. Last_Argument loop
               if Arguments_Displayed (Arg) then
                  Add_Str_To_Name_Buffer (" ");

                  if Arguments_Simple_Name (Arg) then
                     Add_Str_To_Name_Buffer (Base_Name (Arguments (Arg).all));

                  else
                     Add_Str_To_Name_Buffer (Arguments (Arg).all);
                  end if;

               elsif Display_Ellipse then
                  Add_Str_To_Name_Buffer (" ...");
                  Display_Ellipse := False;
               end if;
            end loop;

            Put_Line (Name_Buffer (1 .. Name_Len));
         end if;
      end if;
   end Display_Command;

   ------------------------
   -- Get_Linker_Options --
   ------------------------

   procedure Get_Linker_Options (For_Project : Project_Id) is
      Linker_Lib_Dir_Option  : String_Access;

      procedure Recursive_Add
        (Proj  : Project_Id;
         Tree  : Project_Tree_Ref;
         Dummy : in out Boolean);
      --  The recursive routine used to add linker options

      -------------------
      -- Recursive_Add --
      -------------------

      procedure Recursive_Add
        (Proj  : Project_Id;
         Tree  : Project_Tree_Ref;
         Dummy : in out Boolean)
      is
         pragma Unreferenced (Dummy);
         Linker_Package : Package_Id;
         Options        : Variable_Value;

      begin
         if Proj /= For_Project then
            Linker_Package :=
              GPR.Util.Value_Of
                (Name        => Name_Linker,
                 In_Packages => Proj.Decl.Packages,
                 Shared      => Tree.Shared);
            Options :=
              GPR.Util.Value_Of
                (Name                    => Name_Ada,
                 Index                   => 0,
                 Attribute_Or_Array_Name => Name_Linker_Options,
                 In_Package              => Linker_Package,
                 Shared                  => Tree.Shared);

            --  If attribute is present, add the project with
            --  the attribute to table Linker_Opts.

            if Options /= Nil_Variable_Value then
               Linker_Opts.Increment_Last;
               Linker_Opts.Table (Linker_Opts.Last) :=
                 (Project => Proj, Options => Options.Values);
            end if;
         end if;
      end Recursive_Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Recursive_Add);

      Dummy : Boolean := False;

      --  Start of processing for Get_Linker_Options

   begin
      if For_Project.Config.Linker_Lib_Dir_Option = No_Name then
         Linker_Lib_Dir_Option := new String'("-L");

      else
         Linker_Lib_Dir_Option :=
           new String'
             (Get_Name_String (For_Project.Config.Linker_Lib_Dir_Option));
      end if;

      Linker_Opts.Init;

      For_All_Projects
        (For_Project, Project_Tree, Dummy, Imported_First => True);

      for Index in reverse 1 .. Linker_Opts.Last loop
         declare
            Options  : String_List_Id := Linker_Opts.Table (Index).Options;
            Proj     : constant Project_Id :=
                         Linker_Opts.Table (Index).Project;
            Option   : Name_Id;
            Dir_Path : constant String :=
                         Get_Name_String (Proj.Directory.Display_Name);

         begin
            while Options /= Nil_String loop
               Option :=
                 Project_Tree.Shared.String_Elements.Table (Options).Value;
               Get_Name_String (Option);

               --  Do not consider empty linker options

               if Name_Len /= 0 then
                  --  Object files and -L switches specified with relative
                  --  paths must be converted to absolute paths.

                  if Name_Len > Linker_Lib_Dir_Option'Length
                    and then
                      Name_Buffer (1 .. Linker_Lib_Dir_Option'Length) =
                        Linker_Lib_Dir_Option.all
                  then
                     if Is_Absolute_Path
                       (Name_Buffer
                          (Linker_Lib_Dir_Option'Length + 1 .. Name_Len))
                     then
                        Add_Argument (Name_Buffer (1 .. Name_Len), True);

                     else
                        declare
                           Dir : constant String :=
                             Dir_Path &
                             Directory_Separator &
                             Name_Buffer
                             (Linker_Lib_Dir_Option'Length + 1 .. Name_Len);
                        begin
                           if Is_Directory (Dir) then
                              Add_Argument
                                (Linker_Lib_Dir_Option.all & Dir, True);
                           else
                              Add_Argument
                                (Name_Buffer (1 .. Name_Len), True);
                           end if;
                        end;
                     end if;

                  elsif Name_Buffer (1) = '-' or else
                      Is_Absolute_Path (Name_Buffer (1 .. Name_Len))
                  then
                     Add_Argument (Name_Buffer (1 .. Name_Len), True);

                  else
                     declare
                        File : constant String :=
                          Dir_Path &
                          Directory_Separator &
                          Name_Buffer (1 .. Name_Len);
                     begin
                        if Is_Regular_File (File) then
                           Add_Argument
                             (File, True, Simple_Name => True);
                        else
                           Add_Argument (Name_Buffer (1 .. Name_Len), True);
                        end if;
                     end;
                  end if;
               end if;

               Options :=
                 Project_Tree.Shared.String_Elements.Table (Options).Next;
            end loop;
         end;
      end loop;
   end Get_Linker_Options;

   ---------------------------
   -- Is_In_Library_Project --
   ---------------------------

   function Is_In_Library_Project (Object_Path : String) return Boolean is
      Path_Id : constant Path_Name_Type := Create_Name (Object_Path);
      Src     : Source_Id;
      Iter    : Source_Iterator;
   begin
      Iter := For_Each_Source (Project_Tree);
      loop
         Src := GPR.Element (Iter);
         exit when Src = No_Source;

         if Src.Object_Path = Path_Id then
            return Src.Project.Library;
         end if;

         Next (Iter);
      end loop;

      return False;
   end Is_In_Library_Project;

   ------------------------
   -- Rpaths_Relative_To --
   ------------------------

   procedure Rpaths_Relative_To
     (Exec_Dir : Path_Name_Type;
      Origin   : Name_Id)
   is
      Exec : String :=
               Normalize_Pathname
                 (Get_Name_String (Exec_Dir),
                  Case_Sensitive => False);

      Last_Exec : Positive;
      Curr_Exec : Positive;
      Last_Path : Positive;
      Curr_Path : Positive;
      Nmb       : Natural;

      Origin_Name : constant String := Get_Name_String (Origin);

   begin
      --  Replace all directory separators with '/' to ease search

      if Directory_Separator /= '/' then
         for J in Exec'Range loop
            if Exec (J) = Directory_Separator then
               Exec (J) := '/';
            end if;
         end loop;
      end if;

      for Npath in 1 .. Rpaths.Last loop
         declare
            Insensitive_Path : String :=
              Normalize_Pathname
                (Rpaths.Table (Npath).all,
                 Case_Sensitive => False);

            Path : constant String :=
                     Normalize_Pathname (Rpaths.Table (Npath).all);

         begin
            --  Replace all directory separators with '/' to ease search

            if Directory_Separator /= '/' then
               for J in Insensitive_Path'Range loop
                  if Insensitive_Path (J) = Directory_Separator then
                     Insensitive_Path (J) := '/';
                  end if;
               end loop;
            end if;

            --  Find the number of common directories between the path and the
            --  exec directory.

            Nmb := 0;
            Curr_Path := Insensitive_Path'First;
            Curr_Exec := Exec'First;
            loop
               exit when
                 Curr_Path > Insensitive_Path'Last
                 or else Curr_Exec > Exec'Last
                 or else Insensitive_Path (Curr_Path) /= Exec (Curr_Exec);

               if Insensitive_Path (Curr_Path) = '/' then
                  Nmb := Nmb + 1;
                  Last_Path := Curr_Path;
                  Last_Exec := Curr_Exec;

               elsif Curr_Exec = Exec'Last
                 and then Curr_Path > Insensitive_Path'Last
               then
                  Nmb := Nmb + 1;
                  Last_Path := Curr_Path + 1;
                  Last_Exec := Curr_Exec + 1;
                  exit;
               end if;

               Curr_Path := Curr_Path + 1;
               Curr_Exec := Curr_Exec + 1;
            end loop;

            --  If there is more than one common directories (the root
            --  directory does not count), then change the absolute path to a
            --  relative path.

            if Nmb > 1 then
               Nmb := 0;

               for J in Last_Exec .. Exec'Last - 1 loop
                  if Exec (J) = '/' then
                     Nmb := Nmb + 1;
                  end if;
               end loop;

               if Nmb = 0 then
                  if Last_Path >= Path'Last then
                     --  Case of the path being the exec dir

                     Rpaths.Table (Npath) :=
                       new String'(Origin_Name & Directory_Separator & ".");

                  else
                     --  Case of the path being a subdir of the exec dir

                     Rpaths.Table (Npath) :=
                       new String'
                         (Origin_Name & Directory_Separator &
                          Path (Last_Path + 1 .. Path'Last));
                  end if;

               else
                  if Last_Path >= Path'Last then
                     --  Case of the exec dir being a subdir of the path

                     Rpaths.Table (Npath) :=
                       new String'
                         (Origin_Name & Directory_Separator &
                          (Nmb - 1) * (".." & Directory_Separator) & "..");

                  else
                     --  General case of path and exec dir having a common root

                     Rpaths.Table (Npath) :=
                       new String'
                         (Origin_Name & Directory_Separator &
                          Nmb * (".." & Directory_Separator) &
                          Path (Last_Path + 1 .. Path'Last));
                  end if;
               end if;
            end if;
         end;
      end loop;
   end Rpaths_Relative_To;

   ---------------
   -- Link_Main --
   ---------------

   procedure Link_Main (Main_File : Main_Info) is

      function Global_Archive_Name (For_Project : Project_Id) return String;
      --  Returns the name of the global archive for a project

      Linker_Name        : String_Access := null;
      Linker_Path        : String_Access;
      Min_Linker_Opts    : Name_List_Index;
      Exchange_File      : Text_IO.File_Type;
      Line               : String (1 .. 1_000);
      Last               : Natural;

      --  Success            : Boolean := False;

      Section            : Binding_Section := No_Binding_Section;

      Linker_Needs_To_Be_Called : Boolean;

      Executable_TS      : Time_Stamp_Type;
      Main_Object_TS     : Time_Stamp_Type;
      Binder_Exchange_TS : Time_Stamp_Type;
      Binder_Object_TS   : Time_Stamp_Type := Dummy_Time_Stamp;
      Global_Archive_TS  : Time_Stamp_Type;

      Global_Archive_Has_Been_Built : Boolean;
      Global_Archive_Exists         : Boolean;
      OK                            : Boolean;

      Disregard          : Boolean;

      B_Data : Binding_Data;

      --  Main already has the right canonical casing
      Main         : constant String := Get_Name_String (Main_File.File);
      Main_Source  : constant Source_Id := Main_File.Source;

      Main_Id        : File_Name_Type;

      Exec_Name      : File_Name_Type;
      Exec_Path_Name : Path_Name_Type;

      Main_Proj      : Project_Id;

      Main_Base_Name_Index : File_Name_Type;

      First_Object_Index : Natural := 0;
      Last_Object_Index  : Natural := 0;

      Index_Separator : Character;

      Response_File_Name : Path_Name_Type := No_Path;
      Response_2         : Path_Name_Type := No_Path;

      -------------------------
      -- Global_Archive_Name --
      -------------------------

      function Global_Archive_Name (For_Project : Project_Id) return String is
      begin
         return
           "lib" &
           Get_Name_String (For_Project.Name) &
           Archive_Suffix (For_Project);
      end Global_Archive_Name;

   begin
      --  Make sure that the table Rpaths is emptied after each main, so
      --  that the same rpaths are not duplicated.

      Rpaths.Set_Last (0);

      Linker_Needs_To_Be_Called := Opt.Force_Compilations;

      Main_Id := Create_Name (Base_Name (Main));
      Main_Proj := Ultimate_Extending_Project_Of (Main_Source.Project);

      Change_To_Object_Directory (Main_Proj);

      --  Build the global archive for this project, if needed

      Build_Global_Archive
        (Main_Proj,
         Main_File.Tree,
         Global_Archive_Has_Been_Built,
         Global_Archive_Exists,
         OK);

      if not OK then
         Stop_Spawning := True;
         Bad_Processes.Append (Main_File);
         return;
      end if;

      --  Get the main base name

      Index_Separator :=
        Main_Source.Language.Config.Multi_Unit_Object_Separator;

      Main_Base_Name_Index :=
        Base_Name_Index_For (Main, Main_File.Index, Index_Separator);

      if not Linker_Needs_To_Be_Called and then
        Opt.Verbosity_Level > Opt.Low
      then
         Put ("   Checking executable for ");
         Put (Get_Name_String (Main_Source.File));
         Put_Line (" ...");
      end if;

      if Output_File_Name /= null then
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Output_File_Name.all);

         --  If an executable name was specified without an extension and
         --  there is a non empty executable suffix, add the suffix to the
         --  executable name.

         if Main_Proj.Config.Executable_Suffix /= No_Name
            and then Length_Of_Name (Main_Proj.Config.Executable_Suffix) > 0
         then
            declare
               Suffix : String := Get_Name_String
                 (Main_Proj.Config.Executable_Suffix);
               File_Name : String := Output_File_Name.all;

            begin
               if Index (File_Name, ".") = 0 then
                  Canonical_Case_File_Name (Suffix);
                  Canonical_Case_File_Name (File_Name);

                  if Name_Len <= Suffix'Length
                    or else File_Name
                      (File_Name'Last - Suffix'Length + 1 .. File_Name'Last)
                    /= Suffix
                  then
                     Add_Str_To_Name_Buffer (Suffix);
                  end if;
               end if;
            end;
         end if;

         Exec_Name := Name_Find;

      else
         Exec_Name := Executable_Of
           (Project  => Main_Proj,
            Shared   => Main_File.Tree.Shared,
            Main     => Main_Id,
            Index    => Main_Source.Index,
            Language => Get_Name_String (Main_Source.Language.Name));
      end if;

      if Main_Proj.Exec_Directory = Main_Proj.Object_Directory
        or else Is_Absolute_Path (Get_Name_String (Exec_Name))
      then
         Exec_Path_Name := Path_Name_Type (Exec_Name);

      else
         Get_Name_String (Main_Proj.Exec_Directory.Display_Name);
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Directory_Separator;
         Add_Str_To_Name_Buffer (Get_Name_String (Exec_Name));
         Exec_Path_Name := Name_Find;
      end if;

      Executable_TS := File_Stamp (Exec_Path_Name);

      if not Linker_Needs_To_Be_Called
        and then Executable_TS = Empty_Time_Stamp
      then
         Linker_Needs_To_Be_Called := True;

         if Opt.Verbosity_Level > Opt.Low then
            Put_Line ("      -> executable does not exist");
         end if;
      end if;

      --  Get the path of the linker driver

      if Main_Proj.Config.Linker /= No_Path then
         Linker_Name := new String'(Get_Name_String (Main_Proj.Config.Linker));

         Linker_Path := Locate_Exec_On_Path (Linker_Name.all);

         if Linker_Path = null then
            Fail_Program
              (Main_File.Tree,
               "unable to find linker " & Linker_Name.all);
         end if;

      else
         Fail_Program
           (Main_File.Tree,
            "no linker specified and " &
              "no default linker in the configuration");
      end if;

      Last_Argument := 0;

      Initialize_Source_Record (Main_Source);

      Main_Object_TS := File_Stamp (File_Name_Type (Main_Source.Object_Path));

      if not Linker_Needs_To_Be_Called then
         if Main_Object_TS = Empty_Time_Stamp then
            if Opt.Verbosity_Level > Opt.Low then
               Put_Line ("      -> main object does not exist");
            end if;

            Linker_Needs_To_Be_Called := True;

         elsif String (Main_Object_TS) > String (Executable_TS) then
            if Opt.Verbosity_Level > Opt.Low then
               Put_Line
                 ("      -> main object more recent than executable");
            end if;

            Linker_Needs_To_Be_Called := True;
         end if;
      end if;

      if Main_Object_TS = Empty_Time_Stamp then
         Put ("main object for ");
         Put (Get_Name_String (Main_Source.File));
         Put_Line (" does not exist");
         Record_Failure (Main_File);
         return;
      end if;

      if Main_Proj = Main_Source.Object_Project then
         Add_Argument (Get_Name_String (Main_Source.Object), True);
      else
         Add_Argument (Get_Name_String (Main_Source.Object_Path), True);
      end if;

      --  Add the Leading_Switches if there are any in package Linker

      declare
         The_Packages   : constant Package_Id :=
                            Main_Proj.Decl.Packages;
         Linker_Package : constant GPR.Package_Id :=
                            GPR.Util.Value_Of
                              (Name        => Name_Linker,
                               In_Packages => The_Packages,
                               Shared      => Main_File.Tree.Shared);

         Switches    : Variable_Value;
         Switch_List : String_List_Id;
         Element     : String_Element;

      begin
         if Linker_Package /= No_Package then
            declare
               Switches_Array : constant Array_Element_Id :=
                                  GPR.Util.Value_Of
                                    (Name      => Name_Leading_Switches,
                                     In_Arrays =>
                                       Main_File.Tree.Shared.Packages.Table
                                         (Linker_Package).Decl.Arrays,
                                     Shared    => Main_File.Tree.Shared);
               Option         : String_Access;

            begin
               Switches :=
                 GPR.Util.Value_Of
                   (Index     => Name_Id (Main_Id),
                    Src_Index => 0,
                    In_Array  => Switches_Array,
                    Shared    => Main_File.Tree.Shared);

               if Switches = Nil_Variable_Value then
                  Switches :=
                    GPR.Util.Value_Of
                      (Index                  =>
                           Main_Source.Language.Name,
                       Src_Index              => 0,
                       In_Array               => Switches_Array,
                       Shared                 => Main_File.Tree.Shared,
                       Force_Lower_Case_Index => True);
               end if;

               if Switches = Nil_Variable_Value then
                  Switches :=
                    GPR.Util.Value_Of
                      (Index                  => All_Other_Names,
                       Src_Index              => 0,
                       In_Array               => Switches_Array,
                       Shared                 => Main_File.Tree.Shared,
                       Force_Lower_Case_Index => True);
               end if;

               case Switches.Kind is
                  when Undefined | Single =>
                     null;

                  when GPR.List =>
                     Switch_List := Switches.Values;

                     while Switch_List /= Nil_String loop
                        Element :=
                          Main_File.Tree.Shared.String_Elements.Table
                            (Switch_List);
                        Get_Name_String (Element.Value);

                        if Name_Len > 0 then
                           Option :=
                             new String'(Name_Buffer (1 .. Name_Len));
                           Add_Argument (Option.all, True);
                        end if;

                        Switch_List := Element.Next;
                     end loop;
               end case;
            end;
         end if;
      end;

      Find_Binding_Languages (Main_File.Tree, Main_File.Project);

      if Builder_Data (Main_File.Tree).There_Are_Binder_Drivers then
         First_Object_Index := Last_Argument + 1;
         Binding_Options.Init;

         B_Data := Builder_Data (Main_File.Tree).Binding;

         Binding_Loop :
         while B_Data /= null loop
            declare
               Exchange_File_Name : constant String :=
                                      Binder_Exchange_File_Name
                                        (Main_Base_Name_Index,
                                         B_Data.Binder_Prefix).all;
               Binding_Not_Necessary : Boolean;

            begin
               if Is_Regular_File (Exchange_File_Name) then

                  Binder_Exchange_TS :=
                    File_Stamp
                      (Path_Name_Type'(Create_Name
                       (Exchange_File_Name)));

                  Open (Exchange_File, In_File, Exchange_File_Name);
                  Get_Line (Exchange_File, Line, Last);
                  Binding_Not_Necessary :=
                    Line (1 .. Last) = Binding_Label (Nothing_To_Bind);
                  Close (Exchange_File);

                  if Binding_Not_Necessary then
                     goto No_Binding;
                  end if;

                  if not Linker_Needs_To_Be_Called
                    and then
                      String (Binder_Exchange_TS) > String (Executable_TS)
                  then
                     Linker_Needs_To_Be_Called := True;

                     if Opt.Verbosity_Level > Opt.Low then
                        Put ("      -> binder exchange file """);
                        Put (Exchange_File_Name);
                        Put_Line (""" is more recent than executable");
                     end if;
                  end if;

                  Open (Exchange_File, In_File, Exchange_File_Name);

                  while not End_Of_File (Exchange_File) loop
                     Get_Line (Exchange_File, Line, Last);

                     if Last > 0 then
                        if Line (1) = '[' then
                           Section :=
                             Get_Binding_Section (Line (1 .. Last));

                        else
                           case Section is
                              when Generated_Object_File =>

                                 Binder_Object_TS :=
                                   File_Stamp
                                     (Path_Name_Type'
                                          (Create_Name
                                               (Line (1 .. Last))));

                                 Add_Argument
                                   (Line (1 .. Last), Opt.Verbose_Mode);

                              when Bound_Object_Files =>
                                 if Normalize_Pathname
                                   (Line (1 .. Last),
                                    Case_Sensitive => False) /=
                                   Normalize_Pathname
                                     (Get_Name_String
                                          (Main_Source.Object_Path),
                                      Case_Sensitive => False)
                                   and then
                                     not Is_In_Library_Project
                                       (Line (1 .. Last))
                                 then
                                    Add_Argument
                                      (Line (1 .. Last), Opt.Verbose_Mode);
                                 end if;

                              when Resulting_Options =>
                                 if Line (1 .. Last) /= "-static"
                                   and then Line (1 .. Last) /= "-shared"
                                 then
                                    Binding_Options.Append
                                      (new String'(Line (1 .. Last)));
                                 end if;

                              when Gprexch.Run_Path_Option =>
                                 if Opt.Run_Path_Option
                                   and then
                                     Main_Proj.Config.Run_Path_Option /=
                                       No_Name_List
                                 then
                                    Add_Rpath (Line (1 .. Last));
                                    Add_Rpath
                                      (Shared_Libgcc_Dir
                                         (Line (1 .. Last)));
                                 end if;

                              when others =>
                                 null;
                           end case;
                        end if;
                     end if;
                  end loop;

                  Close (Exchange_File);

                  if Binder_Object_TS = Empty_Time_Stamp then
                     if not Linker_Needs_To_Be_Called
                       and then Opt.Verbosity_Level > Opt.Low
                     then
                        Put_Line
                          ("      -> no binder generated object file");
                     end if;

                     Put ("no binder generated object file for ");
                     Put_Line (Get_Name_String (Main_File.File));
                     Record_Failure (Main_File);
                     return;

                  elsif not Linker_Needs_To_Be_Called
                    and then
                      String (Binder_Object_TS) > String (Executable_TS)
                  then
                     Linker_Needs_To_Be_Called := True;

                     if Opt.Verbosity_Level > Opt.Low then
                        Put_Line
                          ("      -> binder generated object is more " &
                             "recent than executable");
                     end if;
                  end if;

               else
                  Put ("binder exchange file ");
                  Put (Exchange_File_Name);
                  Put_Line (" does not exist");
                  Record_Failure (Main_File);
                  return;
               end if;
            end;

            <<No_Binding>>
            B_Data := B_Data.Next;
         end loop Binding_Loop;

         Last_Object_Index := Last_Argument;
      end if;

      --  Add the global archive, if there is one

      if Global_Archive_Exists then
         Global_Archive_TS :=
           File_Stamp
             (Path_Name_Type'
                  (Create_Name (Global_Archive_Name (Main_Proj))));

         if Global_Archive_TS = Empty_Time_Stamp then
            if not Linker_Needs_To_Be_Called
              and then Opt.Verbosity_Level > Opt.Low
            then
               Put_Line ("      -> global archive does not exist");
            end if;

            Put ("global archive for project file ");
            Put (Get_Name_String (Main_Proj.Name));
            Put_Line (" does not exist");
         end if;
      end if;

      if not Linker_Needs_To_Be_Called
        and then Global_Archive_Has_Been_Built
      then
         Linker_Needs_To_Be_Called := True;

         if Opt.Verbosity_Level > Opt.Low then
            Put_Line ("      -> global archive has just been built");
         end if;
      end if;

      if not Linker_Needs_To_Be_Called
        and then Global_Archive_Exists
        and then String (Global_Archive_TS) > String (Executable_TS)
      then
         Linker_Needs_To_Be_Called := True;

         if Opt.Verbosity_Level > Opt.Low then
            Put_Line ("      -> global archive is more recent than " &
                          "executable");
         end if;
      end if;

      --  Check if there are library files that are more recent than
      --  executable.

      declare
         List : Project_List := Main_Proj.All_Imported_Projects;
         Proj : Project_Id;

         Current_Dir : constant String := Get_Current_Dir;
      begin
         while List /= null loop
            Proj := List.Project;
            List := List.Next;

            if Proj.Extended_By = No_Project
              and then Proj.Library
              and then Proj.Object_Directory /= No_Path_Information
              and then (Is_Static (Proj)
                        or else Proj.Standalone_Library = No)
            then
               --  Put the full path name of the library file in Name_Buffer

               Get_Name_String (Proj.Library_Dir.Display_Name);

               if Is_Static (Proj) then
                  Add_Str_To_Name_Buffer ("lib");
                  Add_Str_To_Name_Buffer (Get_Name_String (Proj.Library_Name));

                  if Proj.Config.Archive_Suffix = No_File then
                     Add_Str_To_Name_Buffer (".a");
                  else
                     Add_Str_To_Name_Buffer
                       (Get_Name_String (Proj.Config.Archive_Suffix));
                  end if;

               else
                  --  Shared libraries

                  if Proj.Config.Shared_Lib_Prefix = No_File then
                     Add_Str_To_Name_Buffer ("lib");
                  else
                     Add_Str_To_Name_Buffer
                       (Get_Name_String (Proj.Config.Shared_Lib_Prefix));
                  end if;

                  Add_Str_To_Name_Buffer (Get_Name_String (Proj.Library_Name));

                  if Proj.Config.Shared_Lib_Suffix = No_File then
                     Add_Str_To_Name_Buffer (".so");
                  else
                     Add_Str_To_Name_Buffer
                       (Get_Name_String (Proj.Config.Shared_Lib_Suffix));
                  end if;
               end if;

               --  Check that library file exists and that it is not more
               --  recent than the executable.

               declare
                  Lib_TS : constant Time_Stamp_Type :=
                             File_Stamp (File_Name_Type'(Name_Find));

               begin
                  if Lib_TS = Empty_Time_Stamp then
                     Linker_Needs_To_Be_Called := True;

                     if Opt.Verbosity_Level > Opt.Low then
                        Put ("      -> library file """);
                        Put (Name_Buffer (1 .. Name_Len));
                        Put_Line (""" not found");
                     end if;

                     exit;

                  elsif String (Lib_TS) > String (Executable_TS) then
                     Linker_Needs_To_Be_Called := True;

                     if Opt.Verbosity_Level > Opt.Low then
                        Put ("      -> library file """);
                        Put (Name_Buffer (1 .. Name_Len));
                        Put_Line
                          (""" is more recent than executable");
                     end if;

                     exit;
                  end if;
               end;
            end if;
         end loop;

         Change_Dir (Current_Dir);
      end;

      if not Linker_Needs_To_Be_Called then
         if Opt.Verbosity_Level > Opt.Low then
            Put_Line ("      -> up to date");

         elsif not Opt.Quiet_Output then
            Inform (Exec_Name, "up to date");
         end if;

      else
         if Global_Archive_Exists then
            Add_Argument (Global_Archive_Name (Main_Proj), Opt.Verbose_Mode);
         end if;

         --  Add the library switches, if there are libraries

         Process_Imported_Libraries (Main_Proj, There_Are_SALs => Disregard);

         Library_Dirs.Reset;

         for J in reverse 1 .. Library_Projs.Last loop
            if not Library_Projs.Table (J).Is_Aggregated then
               if Is_Static (Library_Projs.Table (J).Proj) then
                  Add_Argument
                    (Get_Name_String
                       (Library_Projs.Table (J).Proj.Library_Dir.Display_Name)
                       & "lib"
                       & Get_Name_String
                       (Library_Projs.Table (J).Proj.Library_Name)
                       & Archive_Suffix (Library_Projs.Table (J).Proj),
                     Opt.Verbose_Mode);

               else
                  --  Do not issue several time the same -L switch if
                  --  several library projects share the same library
                  --  directory.

                  if not Library_Dirs.Get
                    (Library_Projs.Table (J).Proj.Library_Dir.Name)
                  then
                     Library_Dirs.Set
                       (Library_Projs.Table (J).Proj.Library_Dir.Name, True);

                     if Main_Proj.Config.Linker_Lib_Dir_Option = No_Name then
                        Add_Argument
                          ("-L" &
                             Get_Name_String
                             (Library_Projs.Table (J).
                                Proj.Library_Dir.Display_Name),
                           Opt.Verbose_Mode);

                     else
                        Add_Argument
                          (Get_Name_String
                             (Main_Proj.Config.Linker_Lib_Dir_Option) &
                             Get_Name_String
                             (Library_Projs.Table (J).
                                Proj.Library_Dir.Display_Name),
                           Opt.Verbose_Mode);
                     end if;

                     if Opt.Run_Path_Option
                       and then
                         Main_Proj.Config.Run_Path_Option /= No_Name_List
                     then
                        Add_Rpath
                          (Get_Name_String
                             (Library_Projs.Table
                                (J).Proj.Library_Dir.Display_Name));
                     end if;
                  end if;

                  if Main_Proj.Config.Linker_Lib_Name_Option = No_Name then
                     Add_Argument
                       ("-l" &
                          Get_Name_String
                          (Library_Projs.Table (J).Proj.Library_Name),
                        Opt.Verbose_Mode);

                  else
                     Add_Argument
                       (Get_Name_String
                          (Main_Proj.Config.Linker_Lib_Name_Option) &
                          Get_Name_String
                          (Library_Projs.Table (J).Proj.Library_Name),
                        Opt.Verbose_Mode);
                  end if;
               end if;
            end if;
         end loop;

         --  Put the options in the project file, if any

         declare
            The_Packages : constant Package_Id :=
                             Main_Proj.Decl.Packages;

            Linker_Package : constant GPR.Package_Id :=
                               GPR.Util.Value_Of
                                 (Name        => Name_Linker,
                                  In_Packages => The_Packages,
                                  Shared      => Main_File.Tree.Shared);

            Switches    : Variable_Value;
            Switch_List : String_List_Id;
            Element     : String_Element;

         begin
            if Linker_Package /= No_Package then
               declare
                  Defaults       : constant Array_Element_Id :=
                                     GPR.Util.Value_Of
                                       (Name      => Name_Default_Switches,
                                        In_Arrays =>
                                          Main_File.Tree.Shared.Packages.Table
                                            (Linker_Package).Decl.Arrays,
                                        Shared    => Main_File.Tree.Shared);
                  Switches_Array : constant Array_Element_Id :=
                                     GPR.Util.Value_Of
                                       (Name      => Name_Switches,
                                        In_Arrays =>
                                          Main_File.Tree.Shared.Packages.Table
                                            (Linker_Package).Decl.Arrays,
                                        Shared    => Main_File.Tree.Shared);
                  Option         : String_Access;

               begin
                  Switches :=
                    GPR.Util.Value_Of
                      (Index           => Name_Id (Main_Id),
                       Src_Index       => 0,
                       In_Array        => Switches_Array,
                       Shared          => Main_File.Tree.Shared,
                       Allow_Wildcards => True);

                  if Switches = Nil_Variable_Value then
                     Switches :=
                       GPR.Util.Value_Of
                         (Index                  =>
                              Main_Source.Language.Name,
                          Src_Index              => 0,
                          In_Array               => Switches_Array,
                          Shared                 => Main_File.Tree.Shared,
                          Force_Lower_Case_Index => True);
                  end if;

                  if Switches = Nil_Variable_Value then
                     Switches :=
                       GPR.Util.Value_Of
                         (Index                  => All_Other_Names,
                          Src_Index              => 0,
                          In_Array               => Switches_Array,
                          Shared                 => Main_File.Tree.Shared,
                          Force_Lower_Case_Index => True);
                  end if;

                  if Switches = Nil_Variable_Value then
                     Switches :=
                       GPR.Util.Value_Of
                         (Index     =>
                              Main_Source.Language.Name,
                          Src_Index => 0,
                          In_Array  => Defaults,
                          Shared    => Main_File.Tree.Shared);
                  end if;

                  case Switches.Kind is
                     when Undefined | Single =>
                        null;

                     when GPR.List =>
                        Switch_List := Switches.Values;

                        while Switch_List /= Nil_String loop
                           Element :=
                             Main_File.Tree.Shared.String_Elements.Table
                               (Switch_List);
                           Get_Name_String (Element.Value);

                           if Name_Len > 0 then
                              Option :=
                                new String'(Name_Buffer (1 .. Name_Len));

                              Test_If_Relative_Path
                                (Option,
                                 Get_Name_String (Main_Proj.Directory.Name),
                                 Dash_L);

                              Add_Argument (Option.all, True);
                           end if;

                           Switch_List := Element.Next;
                        end loop;
                  end case;
               end;
            end if;
         end;

         --  Get the Linker_Options, if any

         Get_Linker_Options (For_Project => Main_Proj);

         --  Add the linker switches specified on the command line

         for J in 1 .. Command_Line_Linker_Options.Last loop
            Add_Argument
              (Command_Line_Linker_Options.Table (J), Opt.Verbose_Mode);
         end loop;

         --  Then the binding options

         for J in 1 .. Binding_Options.Last loop
            Add_Argument (Binding_Options.Table (J), Opt.Verbose_Mode);
         end loop;

         --  Then the required switches, if any. These are put here because,
         --  if they include -L switches for example, the link may fail because
         --  the wrong objects or libraries are linked in.

         Min_Linker_Opts :=
           Main_Proj.Config.Trailing_Linker_Required_Switches;

         while Min_Linker_Opts /= No_Name_List loop
            Add_Argument
              (Get_Name_String
                 (Main_File.Tree.Shared.Name_Lists.Table
                    (Min_Linker_Opts).Name),
               Opt.Verbose_Mode);
            Min_Linker_Opts   := Main_File.Tree.Shared.Name_Lists.Table
              (Min_Linker_Opts).Next;
         end loop;

         --  Finally the Trailing_Switches if there are any in package Linker.
         --  They are put here so that it is possible to override the required
         --  switches from the configuration project file.

         declare
            The_Packages   : constant Package_Id :=
              Main_Proj.Decl.Packages;
            Linker_Package : constant GPR.Package_Id :=
              GPR.Util.Value_Of
                (Name        => Name_Linker,
                 In_Packages => The_Packages,
                 Shared      => Main_File.Tree.Shared);

            Switches    : Variable_Value;
            Switch_List : String_List_Id;
            Element     : String_Element;

         begin
            if Linker_Package /= No_Package then
               declare
                  Switches_Array : constant Array_Element_Id :=
                    GPR.Util.Value_Of
                      (Name      => Name_Trailing_Switches,
                       In_Arrays =>
                         Main_File.Tree.Shared.Packages.Table
                           (Linker_Package).Decl.Arrays,
                       Shared    => Main_File.Tree.Shared);
                  Option         : String_Access;

               begin
                  Switches :=
                    GPR.Util.Value_Of
                      (Index     => Name_Id (Main_Id),
                       Src_Index => 0,
                       In_Array  => Switches_Array,
                       Shared    => Main_File.Tree.Shared);

                  if Switches = Nil_Variable_Value then
                     Switches :=
                       GPR.Util.Value_Of
                         (Index                  =>
                              Main_Source.Language.Name,
                          Src_Index              => 0,
                          In_Array               => Switches_Array,
                          Shared                 => Main_File.Tree.Shared,
                          Force_Lower_Case_Index => True);
                  end if;

                  if Switches = Nil_Variable_Value then
                     Switches :=
                       GPR.Util.Value_Of
                         (Index                  => All_Other_Names,
                          Src_Index              => 0,
                          In_Array               => Switches_Array,
                          Shared                 => Main_File.Tree.Shared,
                          Force_Lower_Case_Index => True);
                  end if;

                  case Switches.Kind is
                  when Undefined | Single =>
                     null;

                  when GPR.List =>
                     Switch_List := Switches.Values;

                     while Switch_List /= Nil_String loop
                        Element :=
                          Main_File.Tree.Shared.String_Elements.Table
                            (Switch_List);
                        Get_Name_String (Element.Value);

                        if Name_Len > 0 then
                           Option :=
                             new String'(Name_Buffer (1 .. Name_Len));
                           Add_Argument (Option.all, True);
                        end if;

                        Switch_List := Element.Next;
                     end loop;
                  end case;
               end;
            end if;
         end;

         --  Remove duplicate stack size setting coming from pragmas
         --  Linker_Options or Link_With and linker switches ("-Xlinker
         --  --stack=R,C" or "-Wl,--stack=R"). Only the first stack size
         --  setting option should be taken into account, because the one in
         --  the project file or on the command line will always be the first
         --  one. And any subsequent stack setting option will overwrite the
         --  previous one.
         --  Also, if Opt.Maximum_Processes is greater than one, check for
         --  switches --lto or -flto and add =nn to the switch.

         Clean_Link_Option_Set : declare
            J        : Natural := Last_Object_Index + 1;
            Stack_Op : Boolean := False;

         begin
            while J <= Last_Argument loop

               --  Check for two switches "-Xlinker" followed by "--stack=..."

               if Arguments (J).all = "-Xlinker"
                 and then J < Last_Argument
                 and then Arguments (J + 1)'Length > 8
                 and then Arguments (J + 1) (1 .. 8) = "--stack="
               then
                  if Stack_Op then
                     Arguments (J .. Last_Argument - 2) :=
                       Arguments (J + 2 .. Last_Argument);
                     Last_Argument := Last_Argument - 2;

                  else
                     Stack_Op := True;
                  end if;
               end if;

               --  Check for single switch

               if (Arguments (J)'Length > 17
                   and then Arguments (J) (1 .. 17) = "-Xlinker --stack=")
                 or else
                  (Arguments (J)'Length > 12
                   and then Arguments (J) (1 .. 12) = "-Wl,--stack=")
               then
                  if Stack_Op then
                     Arguments (J .. Last_Argument - 1) :=
                       Arguments (J + 1 .. Last_Argument);
                     Last_Argument := Last_Argument - 1;

                  else
                     Stack_Op := True;
                  end if;
               end if;

               if Opt.Maximum_Processes > 1 then
                  if Arguments (J).all = "--lto" or else
                     Arguments (J).all = "-flto"
                  then
                     declare
                        Img : String := Opt.Maximum_Processes'Img;
                     begin
                        Img (1) := '=';
                        Arguments (J) := new String'(Arguments (J).all & Img);
                     end;
                  end if;
               end if;

               J := J + 1;
            end loop;
         end Clean_Link_Option_Set;

         --  Look for the last switch -shared-libgcc or -static-libgcc and
         --  remove all the others.

         declare
            Dash_Shared_Libgcc : Boolean := False;
            Dash_Static_Libgcc : Boolean := False;
            Arg : Natural;

            procedure Remove_Argument;
            --  Remove Arguments (Arg)

            procedure Remove_Argument is
            begin
               Arguments (Arg .. Last_Argument - 1) :=
                 Arguments (Arg + 1 .. Last_Argument);
               Last_Argument := Last_Argument - 1;
            end Remove_Argument;

         begin
            Arg := Last_Argument;
            loop
               if Arguments (Arg).all = "-shared-libgcc" then
                  if Dash_Shared_Libgcc or Dash_Static_Libgcc then
                     Remove_Argument;

                  else
                     Dash_Shared_Libgcc := True;
                  end if;

               elsif Arguments (Arg).all = "-static-libgcc" then
                  if Dash_Shared_Libgcc or Dash_Static_Libgcc then
                     Remove_Argument;
                  else
                     Dash_Static_Libgcc := True;
                  end if;
               end if;

               Arg := Arg - 1;
               exit when Arg = 0;
            end loop;

            --  If -shared-libgcc was the last switch, then put in the
            --  run path option the shared libgcc dir.

            if Dash_Shared_Libgcc
              and then Opt.Run_Path_Option
              and then Main_Proj.Config.Run_Path_Option /= No_Name_List
            then
               --  Look for the adalib directory in -L switches.
               --  If it is found, then add the shared libgcc
               --  directory to the run path option.

               for J in 1 .. Last_Argument loop
                  declare
                     Option : String (1 .. Arguments (J)'Length);
                     Last   : Natural := Option'Last;

                  begin
                     Option := Arguments (J).all;

                     if Last > 2 and then Option (1 .. 2) = "-L" then
                        if Option (Last) = '/'
                          or else Option (Last) = Directory_Separator
                        then
                           Last := Last - 1;
                        end if;

                        if Last > 10
                          and then Option (Last - 5 .. Last) = "adalib"
                        then
                           Add_Rpath
                             (Shared_Libgcc_Dir (Option (3 .. Last)));
                           exit;
                        end if;
                     end if;
                  end;
               end loop;
            end if;
         end;

         --  Add the run path option, if necessary

         if Opt.Run_Path_Option
           and then Main_Proj.Config.Run_Path_Option /= No_Name_List
           and then Rpaths.Last > 0
         then
            declare
               Nam_Nod : Name_Node :=
                           Main_File.Tree.Shared.Name_Lists.Table
                             (Main_Proj.Config.Run_Path_Option);
               Length  : Natural := 0;
               Arg     : String_Access := null;
            begin
               if Main_Proj.Config.Run_Path_Origin /= No_Name
                 and then
                   Get_Name_String (Main_Proj.Config.Run_Path_Origin) /= ""
               then
                  Rpaths_Relative_To
                    (Main_Proj.Exec_Directory.Display_Name,
                     Main_Proj.Config.Run_Path_Origin);
               end if;

               if Main_Proj.Config.Separate_Run_Path_Options then
                  for J in 1 .. Rpaths.Last loop
                     Nam_Nod := Main_File.Tree.Shared.Name_Lists.Table
                       (Main_Proj.Config.Run_Path_Option);
                     while Nam_Nod.Next /= No_Name_List loop
                        Add_Argument
                          (Get_Name_String (Nam_Nod.Name), True);
                        Nam_Nod := Main_File.Tree.Shared.Name_Lists.Table
                          (Nam_Nod.Next);
                     end loop;

                     Get_Name_String (Nam_Nod.Name);
                     Add_Str_To_Name_Buffer (Rpaths.Table (J).all);
                     Add_Argument
                       (Name_Buffer (1 .. Name_Len), Opt.Verbose_Mode);
                  end loop;

               else
                  while Nam_Nod.Next /= No_Name_List loop
                     Add_Argument (Get_Name_String (Nam_Nod.Name), True);
                     Nam_Nod := Main_File.Tree.Shared.Name_Lists.Table
                       (Nam_Nod.Next);
                  end loop;

                  --  Compute the length of the argument

                  Get_Name_String (Nam_Nod.Name);
                  Length := Name_Len;

                  for J in 1 .. Rpaths.Last loop
                     Length := Length + Rpaths.Table (J)'Length + 1;
                  end loop;

                  Length := Length - 1;

                  --  Create the argument

                  Arg := new String (1 .. Length);
                  Length := Name_Len;
                  Arg (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);

                  for J in 1 .. Rpaths.Last loop
                     if J /= 1 then
                        Length := Length + 1;
                        Arg (Length) := Path_Separator;
                     end if;

                     Arg (Length + 1 .. Length + Rpaths.Table (J)'Length)
                       := Rpaths.Table (J).all;
                     Length := Length + Rpaths.Table (J)'Length;
                  end loop;

                  Add_Argument (Arg, Opt.Verbose_Mode);
               end if;
            end;
         end if;

         --  Add the map file option, if supported and requested

         if Map_File /= null
           and then Main_Proj.Config.Map_File_Option /= No_Name
         then
            Get_Name_String (Main_Proj.Config.Map_File_Option);

            if Map_File'Length > 0 then
               Add_Str_To_Name_Buffer (Map_File.all);

            else
               Add_Str_To_Name_Buffer
                 (Get_Name_String (Main_Base_Name_Index));
               Add_Str_To_Name_Buffer (".map");
            end if;

            Add_Argument (Name_Buffer (1 .. Name_Len), Opt.Verbose_Mode);
         end if;

         --  Add the switch(es) to specify the name of the executable

         declare
            List : Name_List_Index :=
                     Main_Proj.Config.Linker_Executable_Option;
            Nam  : Name_Node;

            procedure Add_Executable_Name;
            --  Add the name of the executable to to current name buffer,
            --  then the content of the name buffer as the next argument.

            -------------------------
            -- Add_Executable_Name --
            -------------------------

            procedure Add_Executable_Name is
            begin
               Add_Str_To_Name_Buffer (Get_Name_String (Exec_Path_Name));
               Add_Argument
                 (Name_Buffer (1 .. Name_Len),
                  True,
                  Simple_Name => not Opt.Verbose_Mode);
            end Add_Executable_Name;

         begin
            if List /= No_Name_List then
               loop
                  Nam := Main_File.Tree.Shared.Name_Lists.Table (List);
                  Get_Name_String (Nam.Name);

                  if Nam.Next = No_Name_List then
                     Add_Executable_Name;
                     exit;

                  else
                     Add_Argument (Name_Buffer (1 .. Name_Len), True);
                  end if;

                  List := Nam.Next;
               end loop;

            else
               Add_Argument ("-o", True);
               Name_Len := 0;
               Add_Executable_Name;
            end if;
         end;

         --  If response files are supported, check the length of the
         --  command line and the number of object files, then create
         --  a response file if needed.

         if Main_Proj.Config.Max_Command_Line_Length > 0
           and then Main_Proj.Config.Resp_File_Format /= GPR.None
           and then First_Object_Index > 0
         then
            declare
               Arg_Length            : Natural := 0;
               Min_Number_Of_Objects : Natural := 0;
            begin
               for J in 1 .. Last_Argument loop
                  Arg_Length := Arg_Length + Arguments (J)'Length + 1;
               end loop;

               if Arg_Length > Main_Proj.Config.Max_Command_Line_Length then
                  if Main_Proj.Config.Resp_File_Options = No_Name_List then
                     Min_Number_Of_Objects := 0;
                  else
                     Min_Number_Of_Objects := 1;
                  end if;

                  --  Don't create a response file if there would not be
                  --  a smaller number of arguments.

                  if Last_Object_Index - First_Object_Index + 1 >
                    Min_Number_Of_Objects
                  then
                     declare
                        Resp_File_Options : String_List_Access :=
                                              new String_List (1 .. 0);
                        List              : Name_List_Index :=
                                              Main_Proj.Config.
                                                Resp_File_Options;
                        Nam_Nod           : Name_Node;

                     begin
                        while List /= No_Name_List loop
                           Nam_Nod :=
                             Main_File.Tree.Shared.Name_Lists.Table (List);
                           Resp_File_Options :=
                             new String_List'
                               (Resp_File_Options.all
                                & new String'
                                  (Get_Name_String (Nam_Nod.Name)));
                           List := Nam_Nod.Next;
                        end loop;

                        Create_Response_File
                          (Format            =>
                             Main_Proj.Config.Resp_File_Format,
                           Objects           => Arguments
                             (First_Object_Index .. Last_Object_Index),
                           Other_Arguments   =>
                             Arguments (Last_Object_Index + 1 ..
                                 Last_Argument),
                           Resp_File_Options => Resp_File_Options.all,
                           Name_1            => Response_File_Name,
                           Name_2            => Response_2);

                        Record_Temp_File
                          (Shared => Main_File.Tree.Shared,
                           Path   => Response_File_Name);

                        if Response_2 /= No_Path then
                           Record_Temp_File
                             (Shared => Main_File.Tree.Shared,
                              Path   => Response_2);
                        end if;

                        if Main_Proj.Config.Resp_File_Format = GCC
                          or else
                            Main_Proj.Config.Resp_File_Format = GCC_GNU
                          or else
                            Main_Proj.Config.Resp_File_Format = GCC_Object_List
                          or else
                            Main_Proj.Config.Resp_File_Format = GCC_Option_List
                        then
                           Arguments (First_Object_Index) :=
                             new String'("@" &
                                           Get_Name_String
                                           (Response_File_Name));
                           Last_Argument := First_Object_Index;

                        else
                           --  Replace the first object file arguments
                           --  with the argument(s) specifying the
                           --  response file. No need to update
                           --  Arguments_Displayed, as the values are
                           --  already correct (= Verbose_Mode).

                           if Resp_File_Options'Length = 0 then
                              Arguments (First_Object_Index) :=
                                new String'(Get_Name_String
                                            (Response_File_Name));
                              First_Object_Index := First_Object_Index + 1;

                           else
                              for J in Resp_File_Options'First ..
                                Resp_File_Options'Last - 1
                              loop
                                 Arguments (First_Object_Index) :=
                                   Resp_File_Options (J);
                                 First_Object_Index :=
                                   First_Object_Index + 1;
                              end loop;

                              Arguments (First_Object_Index) :=
                                new String'(Resp_File_Options
                                            (Resp_File_Options'Last).all
                                            &
                                              Get_Name_String
                                              (Response_File_Name));
                              First_Object_Index :=
                                First_Object_Index + 1;
                           end if;

                           --  And put the arguments following the object
                           --  files immediately after the response file
                           --  argument(s). Update Arguments_Displayed
                           --  too.

                           Arguments (First_Object_Index ..
                                        Last_Argument -
                                          Last_Object_Index +
                                            First_Object_Index -
                                              1) :=
                                     Arguments (Last_Object_Index + 1 ..
                                                          Last_Argument);
                           Arguments_Displayed
                             (First_Object_Index ..
                                Last_Argument -
                                  Last_Object_Index +
                                    First_Object_Index -
                                      1) :=
                                     Arguments_Displayed
                                       (Last_Object_Index + 1 ..
                                                    Last_Argument);
                           Last_Argument :=
                             Last_Argument - Last_Object_Index +
                               First_Object_Index - 1;
                        end if;
                     end;
                  end if;
               end if;
            end;
         end if;

         --  Delete an eventual executable, in case it is a symbolic
         --  link as we don't want to modify the target of the link.

         declare
            Dummy : Boolean;
            pragma Unreferenced (Dummy);

         begin
            Delete_File (Get_Name_String (Exec_Path_Name), Dummy);
         end;

         if not Opt.Quiet_Output then
            if Opt.Verbose_Mode then
               Display_Command (Linker_Path);
            else
               Display
                 (Section  => GPR.Link,
                  Command  => "link",
                  Argument => Main);
            end if;
         end if;

         declare
            Pid : Process_Id;
         begin
            Script_Write (Linker_Path.all,  Arguments (1 .. Last_Argument));
            Pid := Non_Blocking_Spawn
              (Linker_Path.all,  Arguments (1 .. Last_Argument));

            if Pid = Invalid_Pid then
               Record_Failure (Main_File);

            else
               Add_Process
                 (Pid,
                  (Linking, Pid, Main_File));
               Display_Processes ("link");
            end if;
         end;
      end if;
   end Link_Main;

   ---------
   -- Run --
   ---------

   procedure Run is

      Main : Main_Info;

      procedure Do_Link (Project : Project_Id; Tree : Project_Tree_Ref);

      procedure Await_Link;

      procedure Wait_For_Available_Slot;

      ----------------
      -- Await_Link --
      ----------------

      procedure Await_Link is
         Data : Process_Data;
         OK   : Boolean;
      begin
         loop
            Await_Process (Data, OK);

            if Data /= No_Process_Data then

               if not OK then
                  Record_Failure (Data.Main);
               end if;

               Display_Processes ("link");
               return;
            end if;
         end loop;
      end Await_Link;

      -------------
      -- Do_Link --
      -------------

      procedure Do_Link (Project : Project_Id; Tree : Project_Tree_Ref) is
         pragma Unreferenced (Project);
         Main_File : Main_Info;
      begin
         if Builder_Data (Tree).Need_Linking and then not Stop_Spawning then
            Mains.Reset;
            loop
               Main_File := Mains.Next_Main;
               exit when Main_File = No_Main_Info;

               if Main_File.Tree = Tree
                 and then not Project_Compilation_Failed (Main_File.Project)
               then
                  Wait_For_Available_Slot;
                  exit when Stop_Spawning;
                  Link_Main (Main_File);
                  exit when Stop_Spawning;
               end if;
            end loop;
         end if;
      end Do_Link;

      procedure Link_All is new For_Project_And_Aggregated (Do_Link);

      -----------------------------
      -- Wait_For_Available_Slot --
      -----------------------------

      procedure Wait_For_Available_Slot is
      begin
         while Outstanding_Processes >= Opt.Maximum_Processes loop
            Await_Link;
         end loop;
      end Wait_For_Available_Slot;

   begin
      Outstanding_Processes := 0;
      Stop_Spawning := False;
      Link_All (Main_Project, Project_Tree);

      while Outstanding_Processes > 0 loop
         Await_Link;
      end loop;

      if Bad_Processes.Last = 1 then
         Main := Bad_Processes.Table (1);
         Fail_Program
           (Main.Tree,
            "link of " & Get_Name_String (Main.File) & " failed");

      elsif Bad_Processes.Last > 1 then
         for J in 1 .. Bad_Processes.Last loop
            Main := Bad_Processes.Table (J);
            Put ("   link of ");
            Put (Get_Name_String (Main.File));
            Put_Line (" failed");
         end loop;

         Fail_Program (Main.Tree, "*** link phase failed");
      end if;
   end Run;

end Gprbuild.Link;
