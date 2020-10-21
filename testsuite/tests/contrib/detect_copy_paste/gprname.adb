------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--                      Copyright (C) 2001-2017, AdaCore                    --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Table;

with GPR.Attr.PM;
with GPR.Com;
with GPR.Env;
with GPR.Names;   use GPR.Names;
with GPR.Opt;
with GPR.Osint;   use GPR.Osint;
with GPR.Part;
with GPR.PP;
with GPR.Tree;    use GPR.Tree;
with GPR.Snames;  use GPR.Snames;
with GPR.Tempdir;
with GPR.Util;    use GPR.Util;

with GprConfig.Sdefault;
with Gpr_Util;    use Gpr_Util;

with System.Case_Util; use System.Case_Util;
with System.CRTL;
with System.HTable;
with System.Regexp;    use System.Regexp;

package body GPRName is

   use Gpr_Util.Project_Output;

   --  Packages of project files where unknown attributes are errors

   --  All the following need comments ??? All global variables and
   --  subprograms must be fully commented.

   Very_Verbose : Boolean := False;
   --  Set in call to Initialize to indicate very verbose output

   Tree : constant GPR.Project_Node_Tree_Ref := new Project_Node_Tree_Data;
   --  The project tree where the project file is parsed

   Root_Environment : GPR.Tree.Environment;

   Args : Argument_List_Access;
   --  The list of arguments for calls to the compiler to get the unit names
   --  and kinds (spec or body) in the Ada sources.

   Path_Name : String_Access;

   Path_Last : Natural;

   Directory_Last    : Natural := 0;

   Output_Name      : String_Access;
   Output_Name_Last : Natural;
   Output_Name_Id   : Name_Id;

   Project_Naming_File_Name : String_Access;
   --  String (1 .. Output_Name'Length +  Naming_File_Suffix'Length);

   Project_Naming_Last : Natural;
   Project_Naming_Id   : Name_Id := No_Name;

   Source_List_Path : String_Access;
   --  (1 .. Output_Name'Length + Source_List_File_Suffix'Length);
   Source_List_Last : Natural;

   Source_List_FD : File_Descriptor;

   Project_Node        : Project_Node_Id := Empty_Project_Node;
   Project_Declaration : Project_Node_Id := Empty_Project_Node;
   Source_Dirs_List    : Project_Node_Id := Empty_Project_Node;
   Languages_List      : Project_Node_Id := Empty_Project_Node;

   Project_Naming_Node     : Project_Node_Id := Empty_Project_Node;
   Project_Naming_Decl     : Project_Node_Id := Empty_Project_Node;
   Naming_Package          : Project_Node_Id := Empty_Project_Node;
   Naming_Package_Comments : Project_Node_Id := Empty_Project_Node;

   Source_Files_Comments     : Project_Node_Id := Empty_Project_Node;
   Source_Dirs_Comments      : Project_Node_Id := Empty_Project_Node;
   Source_List_File_Comments : Project_Node_Id := Empty_Project_Node;
   Languages_Comments        : Project_Node_Id := Empty_Project_Node;

   function Dup (Fd : File_Descriptor) return File_Descriptor;
   --  Create a copy of Fd and returns it

   procedure Dup2 (Old_Fd, New_Fd : File_Descriptor);
   --  Close New_Fd if necessary and copy Old_Fd into New_Fd

   Non_Empty_Node : constant Project_Node_Id := 1;
   --  Used for the With_Clause of the naming project

   type Matched_Type is (Match, No_Match, Excluded);

   Naming_File_Suffix      : constant String := "_naming";
   Source_List_File_Suffix : constant String := "_source_list.txt";

   package Processed_Directories is new GNAT.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  The list of already processed directories for each section, to avoid
   --  processing several times the same directory in the same section.

   package Source_Directories is new GNAT.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  The complete list of directories to be put in attribute Source_Dirs in
   --  the project file.

   type Source is record
      File_Name : Name_Id;
      Unit_Name : Name_Id;
      Index     : Int := 0;
      Spec      : Boolean;
   end record;

   package Sources is new GNAT.Table
     (Table_Component_Type => Source,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  The list of Ada sources found, with their unit name and kind, to be put
   --  in the source attribute and package Naming of the project file, or in
   --  the pragmas Source_File_Name in the configuration pragmas file.

   type Foreign_Source is record
      Language : Name_Id;
      File_Name : Name_Id;
   end record;

   package Foreign_Sources is new GNAT.Table
     (Table_Component_Type => Foreign_Source,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100);

   package Source_Files is new System.HTable.Simple_HTable
     (Header_Num => GPR.Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Name_Id,
      Hash       => GPR.Hash,
      Equal      => "=");
   --  Hash table to keep track of source file names, to avoid putting several
   --  times the same file name in case of multi-unit files.

   package Languages is new GNAT.Table
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100);

   procedure Add_Language (Lang : Name_Id);
   --  Add Lang to the list of languages

   ------------------
   -- Add_Language --
   ------------------

   procedure Add_Language (Lang : Name_Id) is
   begin
      for J in 1 .. Languages.Last loop
         if Languages.Table (J) = Lang then
            return;
         end if;
      end loop;

      Languages.Append (Lang);
   end Add_Language;

   ---------
   -- Dup --
   ---------

   function Dup  (Fd : File_Descriptor) return File_Descriptor is
   begin
      return File_Descriptor (System.CRTL.dup (Integer (Fd)));
   end Dup;

   ----------
   -- Dup2 --
   ----------

   procedure Dup2 (Old_Fd, New_Fd : File_Descriptor) is
      Fd : Integer;
      pragma Warnings (Off, Fd);
   begin
      Fd := System.CRTL.dup2 (Integer (Old_Fd), Integer (New_Fd));
   end Dup2;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      Discard : Boolean;
      pragma Warnings (Off, Discard);

      Current_Source_Dir : Project_Node_Id := Empty_Project_Node;

      Current_Language_Node : Project_Node_Id := Empty_Project_Node;

      Naming_Decl_Item  : constant Project_Node_Id :=
        Default_Project_Node
          (Of_Kind => N_Declarative_Item,
           In_Tree => Tree);

      Naming : constant Project_Node_Id :=
        Default_Project_Node
          (Of_Kind => N_Package_Declaration,
           In_Tree => Tree);

      procedure Check_Duplicates
        (Current_Source : Source;
         Source_Index   : Natural;
         Result         : out Natural);
      --  Check for duplicated source file+index in
      --  Source.Table (1 .. Source_Index - 1) and set Result to the relevant
      --  index if any. Set Result to 0 if not found.

      ----------------------
      -- Check_Duplicates --
      ----------------------

      procedure Check_Duplicates
        (Current_Source : Source;
         Source_Index   : Natural;
         Result         : out Natural) is
      begin
         Result := 0;

         for J in reverse 1 .. Source_Index - 1 loop
            declare
               S : Source renames Sources.Table (J);
            begin
               if S.File_Name = Current_Source.File_Name
                 and then S.Index = Current_Source.Index
               then
                  Result := J;
                  return;
               end if;
            end;
         end loop;
      end Check_Duplicates;

   begin
      --  If there were no already existing project file, or if the parsing was
      --  unsuccessful, create an empty project node with the correct name and
      --  its project declaration node.

      if No (Project_Node) then
         Project_Node :=
           Default_Project_Node (Of_Kind => N_Project, In_Tree => Tree);
         Set_Name_Of (Project_Node, Tree, To => Output_Name_Id);
         Set_Project_Declaration_Of
           (Project_Node, Tree,
            To => Default_Project_Node
              (Of_Kind => N_Project_Declaration, In_Tree => Tree));

      end if;

      --  Delete the file if it already exists

      Delete_File
        (Path_Name (Directory_Last + 1 .. Path_Last),
         Success => Discard);

      --  Create a new one

      if Opt.Verbose_Mode then
         Put ("Creating new file """);
         Put (Path_Name (Directory_Last + 1 .. Path_Last));
         Put_Line ("""");
      end if;

      Output_FD := Create_New_File
        (Path_Name (Directory_Last + 1 .. Path_Last),
         Fmode => Text);

      --  Fails if project file cannot be created

      if Output_FD = Invalid_FD then
         GPR.Com.Fail
           ("cannot create new """ & Path_Name (1 .. Path_Last) & """");
      end if;

      --  Delete the source list file, if it already exists

      declare
         Discard : Boolean;
         pragma Warnings (Off, Discard);
      begin
         Delete_File
           (Source_List_Path (1 .. Source_List_Last),
            Success => Discard);
      end;

      --  And create a new source list file, fail if file cannot be created

      Source_List_FD := Create_New_File
        (Name  => Source_List_Path (1 .. Source_List_Last),
         Fmode => Text);

      if Source_List_FD = Invalid_FD then
         GPR.Com.Fail
           ("cannot create file """
            & Source_List_Path (1 .. Source_List_Last)
            & """");
      end if;

      if Opt.Verbose_Mode then
         Put ("Naming project file name is """);
         Put
           (Project_Naming_File_Name (1 .. Project_Naming_Last));
         Put_Line ("""");
      end if;

      --  Create the naming project node

      Project_Naming_Node :=
        Default_Project_Node (Of_Kind => N_Project, In_Tree => Tree);
      Set_Name_Of (Project_Naming_Node, Tree, To => Project_Naming_Id);
      Project_Naming_Decl :=
        Default_Project_Node
          (Of_Kind => N_Project_Declaration, In_Tree => Tree);
      Set_Project_Declaration_Of
        (Project_Naming_Node, Tree, Project_Naming_Decl);
      Naming_Package :=
        Default_Project_Node
          (Of_Kind => N_Package_Declaration, In_Tree => Tree);
      Set_Name_Of (Naming_Package, Tree, To => Name_Naming);

      --  Add an attribute declaration for Source_Files as an empty list (to
      --  indicate there are no sources in the naming project) and a package
      --  Naming (that will be filled later).

      declare
         Decl_Item : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind => N_Declarative_Item, In_Tree => Tree);

         Attribute : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind       => N_Attribute_Declaration,
              In_Tree       => Tree,
              And_Expr_Kind => List);

         Expression : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind       => N_Expression,
              In_Tree       => Tree,
              And_Expr_Kind => List);

         Term      : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind       => N_Term,
              In_Tree       => Tree,
              And_Expr_Kind => List);

         Empty_List : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind => N_Literal_String_List,
              In_Tree => Tree);

      begin
         Set_First_Declarative_Item_Of
           (Project_Naming_Decl, Tree, To => Decl_Item);
         Set_Next_Declarative_Item (Decl_Item, Tree, Naming_Package);
         Set_Current_Item_Node (Decl_Item, Tree, To => Attribute);
         Set_Name_Of (Attribute, Tree, To => Name_Source_Files);
         Set_Expression_Of (Attribute, Tree, To => Expression);
         Set_First_Term (Expression, Tree, To => Term);
         Set_Current_Term (Term, Tree, To => Empty_List);
      end;

      --  Add a with clause on the naming project in the main project, if
      --  there is not already one.

      declare
         With_Clause : Project_Node_Id :=
           First_With_Clause_Of (Project_Node, Tree);

      begin
         while Present (With_Clause) loop
            exit when
              GPR.Tree.Name_Of (With_Clause, Tree) = Project_Naming_Id;
            With_Clause := Next_With_Clause_Of (With_Clause, Tree);
         end loop;

         if No (With_Clause) then
            With_Clause := Default_Project_Node
              (Of_Kind => N_With_Clause, In_Tree => Tree);
            Set_Next_With_Clause_Of
              (With_Clause, Tree,
               To => First_With_Clause_Of (Project_Node, Tree));
            Set_First_With_Clause_Of
              (Project_Node, Tree, To => With_Clause);
            Set_Name_Of (With_Clause, Tree, To => Project_Naming_Id);

            --  We set the project node to something different than Empty_Node,
            --  so that GPR.PP does not generate a limited with clause.

            Set_Project_Node_Of (With_Clause, Tree, Non_Empty_Node);

            Name_Len := Project_Naming_Last;
            Name_Buffer (1 .. Name_Len) :=
              Project_Naming_File_Name (1 .. Project_Naming_Last);
            Set_String_Value_Of (With_Clause, Tree, To => Name_Find);
         end if;
      end;

      Project_Declaration := Project_Declaration_Of (Project_Node, Tree);

      --  Add a package Naming in the main project

      declare
      begin
         Set_Next_Declarative_Item
           (Naming_Decl_Item, Tree,
            To => First_Declarative_Item_Of (Project_Declaration, Tree));
         Set_First_Declarative_Item_Of
           (Project_Declaration, Tree, To => Naming_Decl_Item);
         Set_Current_Item_Node (Naming_Decl_Item, Tree, To => Naming);
         Set_Name_Of (Naming, Tree, To => Name_Naming);

         --  Attach the comments, if any, that were saved for package
         --  Naming.

         Tree.Project_Nodes.Table (Naming).Comments :=
           Naming_Package_Comments;
      end;

      --  Package Naming is a renaming of package Naming in the naming project

      Set_Project_Of_Renamed_Package_Of
        (Naming, Tree, To => Project_Naming_Node);

      --  Add an attribute declaration for Languages, initialized as an
      --  empty list.

      declare
         Decl_Item  : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind => N_Declarative_Item,
              In_Tree => Tree);

         Attribute : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind       => N_Attribute_Declaration,
              In_Tree       => Tree,
              And_Expr_Kind => List);

         Expression : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind       => N_Expression,
              In_Tree       => Tree,
              And_Expr_Kind => List);

         Term  : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind       => N_Term, In_Tree => Tree,
              And_Expr_Kind => List);

      begin
         Set_Next_Declarative_Item
           (Decl_Item, Tree,
            To => First_Declarative_Item_Of (Project_Declaration, Tree));
         Set_First_Declarative_Item_Of
           (Project_Declaration, Tree, To => Decl_Item);
         Set_Current_Item_Node (Decl_Item, Tree, To => Attribute);
         Set_Name_Of (Attribute, Tree, To => Name_Languages);
         Set_Expression_Of (Attribute, Tree, To => Expression);
         Set_First_Term (Expression, Tree, To => Term);
         Languages_List :=
           Default_Project_Node
             (Of_Kind       => N_Literal_String_List,
              In_Tree       => Tree,
              And_Expr_Kind => List);
         Set_Current_Term (Term, Tree, To => Languages_List);

         --  Attach the comments, if any, that were saved for attribute
         --  Source_Dirs.

         Tree.Project_Nodes.Table (Attribute).Comments :=
           Languages_Comments;
      end;

      --  Put the languages in attribute Languages

      for Language_Index in 1 .. Languages.Last loop
         declare
            Expression : constant Project_Node_Id :=
              Default_Project_Node
                (Of_Kind       => N_Expression,
                 In_Tree       => Tree,
                 And_Expr_Kind => Single);

            Term       : constant Project_Node_Id :=
              Default_Project_Node
                (Of_Kind       => N_Term,
                 In_Tree       => Tree,
                 And_Expr_Kind => Single);

            Value      : constant Project_Node_Id :=
              Default_Project_Node
                (Of_Kind       => N_Literal_String,
                 In_Tree       => Tree,
                 And_Expr_Kind => Single);

         begin
            if No (Current_Language_Node) then
               Set_First_Expression_In_List
                 (Languages_List, Tree, To => Expression);
            else
               Set_Next_Expression_In_List
                 (Current_Language_Node, Tree, To => Expression);
            end if;

            Current_Language_Node := Expression;
            Set_First_Term (Expression, Tree, To => Term);
            Set_Current_Term (Term, Tree, To => Value);
            Set_String_Value_Of
              (Value, Tree, To => Languages.Table (Language_Index));
         end;
      end loop;

      --  Add an attribute declaration for Source_Dirs, initialized as an
      --  empty list.

      declare
         Decl_Item  : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind => N_Declarative_Item,
              In_Tree => Tree);

         Attribute : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind       => N_Attribute_Declaration,
              In_Tree       => Tree,
              And_Expr_Kind => List);

         Expression : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind       => N_Expression,
              In_Tree       => Tree,
              And_Expr_Kind => List);

         Term  : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind       => N_Term, In_Tree => Tree,
              And_Expr_Kind => List);

      begin
         Set_Next_Declarative_Item
           (Decl_Item, Tree,
            To => First_Declarative_Item_Of (Project_Declaration, Tree));
         Set_First_Declarative_Item_Of
           (Project_Declaration, Tree, To => Decl_Item);
         Set_Current_Item_Node (Decl_Item, Tree, To => Attribute);
         Set_Name_Of (Attribute, Tree, To => Name_Source_Dirs);
         Set_Expression_Of (Attribute, Tree, To => Expression);
         Set_First_Term (Expression, Tree, To => Term);
         Source_Dirs_List :=
           Default_Project_Node
             (Of_Kind       => N_Literal_String_List,
              In_Tree       => Tree,
              And_Expr_Kind => List);
         Set_Current_Term (Term, Tree, To => Source_Dirs_List);

         --  Attach the comments, if any, that were saved for attribute
         --  Source_Dirs.

         Tree.Project_Nodes.Table (Attribute).Comments :=
           Source_Dirs_Comments;
      end;

      --  Put the source directories in attribute Source_Dirs

      for Source_Dir_Index in 1 .. Source_Directories.Last loop
         declare
            Expression : constant Project_Node_Id :=
              Default_Project_Node
                (Of_Kind       => N_Expression,
                 In_Tree       => Tree,
                 And_Expr_Kind => Single);

            Term       : constant Project_Node_Id :=
              Default_Project_Node
                (Of_Kind       => N_Term,
                 In_Tree       => Tree,
                 And_Expr_Kind => Single);

            Value      : constant Project_Node_Id :=
              Default_Project_Node
                (Of_Kind       => N_Literal_String,
                 In_Tree       => Tree,
                 And_Expr_Kind => Single);

         begin
            if No (Current_Source_Dir) then
               Set_First_Expression_In_List
                 (Source_Dirs_List, Tree, To => Expression);
            else
               Set_Next_Expression_In_List
                 (Current_Source_Dir, Tree, To => Expression);
            end if;

            Current_Source_Dir := Expression;
            Set_First_Term (Expression, Tree, To => Term);
            Set_Current_Term (Term, Tree, To => Value);
            Name_Len := 0;
            Add_Str_To_Name_Buffer
              (Source_Directories.Table (Source_Dir_Index).all);
            Set_String_Value_Of (Value, Tree, To => Name_Find);
         end;
      end loop;

      --  Add an attribute declaration for Source_Files or Source_List_File
      --  with the source list file name that will be created.

      declare
         Decl_Item  : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind => N_Declarative_Item,
              In_Tree => Tree);

         Attribute  : constant Project_Node_Id :=
           Default_Project_Node
             (Of_Kind       => N_Attribute_Declaration,
              In_Tree       => Tree,
              And_Expr_Kind => Single);

         Expression : Project_Node_Id;

         Term : Project_Node_Id;

         Value : Project_Node_Id;

      begin
         Set_Next_Declarative_Item
           (Decl_Item, Tree,
            To => First_Declarative_Item_Of (Project_Declaration, Tree));
         Set_First_Declarative_Item_Of
           (Project_Declaration, Tree, To => Decl_Item);
         Set_Current_Item_Node (Decl_Item, Tree, To => Attribute);
         Set_Name_Of (Attribute, Tree, To => Name_Source_List_File);

         Expression :=
           Default_Project_Node
             (Of_Kind       => N_Expression,
              In_Tree       => Tree,
              And_Expr_Kind => Single);

         Set_Expression_Of (Attribute, Tree, To => Expression);

         Term :=
           Default_Project_Node
             (Of_Kind       => N_Term,
              In_Tree       => Tree,
              And_Expr_Kind => Single);

         Value :=
           Default_Project_Node
             (Of_Kind       => N_Literal_String,
              In_Tree       => Tree,
              And_Expr_Kind => Single);

         Set_First_Term (Expression, Tree, To => Term);
         Set_Current_Term (Term, Tree, To => Value);
         Name_Len := Source_List_Last;
         Name_Buffer (1 .. Name_Len) :=
           Source_List_Path (1 .. Source_List_Last);
         Set_String_Value_Of (Value, Tree, To => Name_Find);

         --  If there was no comments for attribute Source_List_File, put those
         --  for Source_Files, if they exist.

         if Present (Source_List_File_Comments) then
            Tree.Project_Nodes.Table (Attribute).Comments :=
              Source_List_File_Comments;
         else
            Tree.Project_Nodes.Table (Attribute).Comments :=
              Source_Files_Comments;
         end if;

         --  Put the foreign source file names in the source list file

         for Source_Index in 1 .. Foreign_Sources.Last loop
            Get_Name_String (Foreign_Sources.Table (Source_Index).File_Name);
            Add_Char_To_Name_Buffer (ASCII.LF);

            if Write (Source_List_FD,
                      Name_Buffer (1)'Address,
                      Name_Len) /= Name_Len
            then
               GPR.Com.Fail ("disk full");
            end if;
         end loop;

         --  Put the exception declarations in package Naming

         for J in 1 .. Languages.Last loop
            declare
               Lang : constant Name_Id := Languages.Table (J);

            begin
               if Lang /= Name_Ada then
                  --  Add an attribute declaration for
                  --  Implementation_Exceptions for the language.

                  declare
                     Decl_Item  : constant Project_Node_Id :=
                       Default_Project_Node
                         (Of_Kind => N_Declarative_Item,
                          In_Tree => Tree);

                     Attribute : constant Project_Node_Id :=
                       Default_Project_Node
                         (Of_Kind       => N_Attribute_Declaration,
                          In_Tree       => Tree,
                          And_Expr_Kind => List);

                     Expression : constant Project_Node_Id :=
                       Default_Project_Node
                         (Of_Kind       => N_Expression,
                          In_Tree       => Tree,
                          And_Expr_Kind => List);

                     Term : constant Project_Node_Id :=
                       Default_Project_Node
                         (Of_Kind       => N_Term,
                          In_Tree       => Tree,
                          And_Expr_Kind => List);

                     Source_List : Project_Node_Id;
                     Expr        : Project_Node_Id;
                     Prev_Expr   : Project_Node_Id;
                     Trm         : Project_Node_Id;
                     Value       : Project_Node_Id;

                  begin
                     Set_Next_Declarative_Item
                       (Decl_Item,
                        To      => First_Declarative_Item_Of
                                     (Naming_Package, Tree),
                        In_Tree => Tree);
                     Set_First_Declarative_Item_Of
                       (Naming_Package, Tree, To => Decl_Item);
                     Set_Current_Item_Node (Decl_Item, Tree, To => Attribute);
                     Set_Name_Of
                       (Attribute, Tree, To => Name_Implementation_Exceptions);
                     Set_Associative_Array_Index_Of
                       (Attribute, Tree, To => Lang);
                     Set_Expression_Of (Attribute, Tree, To => Expression);
                     Set_First_Term (Expression, Tree, To => Term);
                     Source_List :=
                       Default_Project_Node
                         (Of_Kind       => N_Literal_String_List,
                          In_Tree       => Tree,
                          And_Expr_Kind => List);
                     Set_Current_Term (Term, Tree, To => Source_List);
                     Prev_Expr := Empty_Project_Node;

                     --  Put all the sources for this language in the list

                     for J in 1 .. Foreign_Sources.Last loop
                        if Foreign_Sources.Table (J).Language = Lang then
                           Expr :=
                             Default_Project_Node
                               (Of_Kind       => N_Expression,
                                In_Tree       => Tree,
                                And_Expr_Kind => Single);
                           if Prev_Expr = Empty_Project_Node then
                              Set_First_Expression_In_List
                                (Node    => Source_List,
                                 In_Tree => Tree,
                                 To      => Expr);
                           else
                              Set_Next_Expression_In_List
                                (Node    => Prev_Expr,
                                 In_Tree => Tree,
                                 To      => Expr);
                           end if;

                           Prev_Expr := Expr;

                           Trm :=
                             Default_Project_Node
                               (Of_Kind       => N_Term,
                                In_Tree       => Tree,
                                And_Expr_Kind => Single);
                           Set_First_Term (Expr, Tree, To => Trm);

                           Value := Default_Project_Node
                             (Of_Kind       => N_Literal_String,
                              And_Expr_Kind => Single,
                              In_Tree       => Tree);
                           Set_String_Value_Of
                             (Node    => Value,
                              In_Tree => Tree,
                              To      => Foreign_Sources.Table (J).File_Name);
                           Set_Current_Term (Trm, Tree, To => Value);
                        end if;
                     end loop;
                  end;
               end if;
            end;
         end loop;

         --  Put the sources in the source list files (or attribute
         --  Source_Files) and in the naming project (or the Naming package).

         for Source_Index in 1 .. Sources.Last loop

            --  Add the corresponding attribute in the Naming package

            declare
               Current_Source : constant Source :=
                 Sources.Table (Source_Index);

               Decl_Item : constant Project_Node_Id :=
                 Default_Project_Node
                   (Of_Kind =>
                      N_Declarative_Item,
                    In_Tree => Tree);

               Attribute : constant Project_Node_Id :=
                 Default_Project_Node
                   (Of_Kind =>
                      N_Attribute_Declaration,
                    In_Tree => Tree);

               Expression : constant Project_Node_Id :=
                 Default_Project_Node
                   (Of_Kind       => N_Expression,
                    And_Expr_Kind => Single,
                    In_Tree       => Tree);

               Term      : constant Project_Node_Id :=
                 Default_Project_Node
                   (Of_Kind       => N_Term,
                    And_Expr_Kind => Single,
                    In_Tree       => Tree);

               Value     : constant Project_Node_Id :=
                 Default_Project_Node
                   (Of_Kind       => N_Literal_String,
                    And_Expr_Kind => Single,
                    In_Tree       => Tree);

               Process_File : Boolean := True;
               Index        : Natural;

            begin
               if Opt.Ignore_Predefined_Units
                 and then Current_Source.Unit_Name /= No_Name
               then
                  Get_Name_String (Current_Source.Unit_Name);

                  if Is_Ada_Predefined_Unit (Name_Buffer (1 .. Name_Len)) then
                     Process_File := False;
                  end if;
               end if;

               if Process_File then
                  --  Add source file name to the source list file (or the
                  --  attribute Source_Files) if it is not already there.
                  --  If already there, check for duplicate filenames+source
                  --  index and emit warnings accordingly.

                  if Source_Files.Get (Current_Source.File_Name) then
                     Check_Duplicates (Current_Source, Source_Index, Index);

                     if Index /= 0 then
                        if Opt.Ignore_Duplicate_Files then
                           Process_File := False;

                        elsif Sources.Table (Index).Unit_Name
                          = Current_Source.Unit_Name
                        then
                           Put_Line
                             ("warning: duplicate file " &
                              Get_Name_String (Current_Source.File_Name) &
                              " for unit " &
                              Get_Name_String (Current_Source.Unit_Name) &
                              " will be ignored");
                           Process_File := False;

                        else
                           Put_Line
                             ("warning: duplicate file " &
                              Get_Name_String (Current_Source.File_Name) &
                              " for units " &
                              Get_Name_String (Current_Source.Unit_Name) &
                              " and " &
                              Get_Name_String
                                (Sources.Table (Index).Unit_Name));
                              Put_Line
                                ("warning: generated Naming package needs " &
                                 "to be reviewed manually");
                        end if;
                     end if;
                  else
                     Source_Files.Set (Current_Source.File_Name, True);

                     Get_Name_String (Current_Source.File_Name);
                     Add_Char_To_Name_Buffer (ASCII.LF);

                     if Write (Source_List_FD,
                               Name_Buffer (1)'Address,
                               Name_Len) /= Name_Len
                     then
                        GPR.Com.Fail ("disk full");
                     end if;
                  end if;
               end if;

               if Process_File then
                  --  For an Ada source, add entry in package Naming

                  if Current_Source.Unit_Name /= No_Name then
                     Set_Next_Declarative_Item
                       (Decl_Item,
                        To      => First_Declarative_Item_Of
                          (Naming_Package, Tree),
                        In_Tree => Tree);
                     Set_First_Declarative_Item_Of
                       (Naming_Package,
                        To      => Decl_Item,
                        In_Tree => Tree);
                     Set_Current_Item_Node
                       (Decl_Item,
                        To      => Attribute,
                        In_Tree => Tree);

                     --  Is it a spec or a body?

                     if Current_Source.Spec then
                        Set_Name_Of
                          (Attribute, Tree,
                           To => Name_Spec);
                     else
                        Set_Name_Of
                          (Attribute, Tree,
                           To => Name_Body);
                     end if;

                     --  Get the name of the unit

                     Get_Name_String (Current_Source.Unit_Name);
                     To_Lower (Name_Buffer (1 .. Name_Len));
                     Set_Associative_Array_Index_Of
                       (Attribute, Tree, To => Name_Find);

                     Set_Expression_Of
                       (Attribute, Tree, To => Expression);
                     Set_First_Term
                       (Expression, Tree, To => Term);
                     Set_Current_Term
                       (Term, Tree, To => Value);

                     --  And set the name of the file

                     Set_String_Value_Of
                       (Value, Tree, To => Current_Source.File_Name);
                     Set_Source_Index_Of
                       (Value, Tree, To => Current_Source.Index);
                  end if;
               end if;
            end;
         end loop;
      end;

      --  Close the source list file

      Close (Source_List_FD);

      --  Output the project file

      GPR.PP.Pretty_Print
        (Project_Node, Tree,
         W_Char                 => Write_A_Char'Access,
         W_Eol                  => Write_Eol'Access,
         W_Str                  => Write_A_String'Access,
         Backward_Compatibility => False,
         Max_Line_Length        => 79);
      Close (Output_FD);

      --  Delete the naming project file if it already exists

      Delete_File
        (Project_Naming_File_Name (1 .. Project_Naming_Last),
         Success => Discard);

      --  Create a new one

      if Opt.Verbose_Mode then
         Put ("Creating new naming project file """);
         Put (Project_Naming_File_Name
                           (1 .. Project_Naming_Last));
         Put_Line ("""");
      end if;

      Output_FD := Create_New_File
        (Project_Naming_File_Name (1 .. Project_Naming_Last),
         Fmode => Text);

      --  Fails if naming project file cannot be created

      if Output_FD = Invalid_FD then
         GPR.Com.Fail
           ("cannot create new """
            & Project_Naming_File_Name (1 .. Project_Naming_Last)
            & """");
      end if;

      --  Output the naming project file

      GPR.PP.Pretty_Print
        (Project_Naming_Node, Tree,
         W_Char                 => Write_A_Char'Access,
         W_Eol                  => Write_Eol'Access,
         W_Str                  => Write_A_String'Access,
         Backward_Compatibility => False);
      Close (Output_FD);
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (File_Path         : String;
      Preproc_Switches  : Argument_List;
      Very_Verbose      : Boolean;
      Flags             : Processing_Flags)
   is
   begin
      GPRName.Very_Verbose := Initialize.Very_Verbose;

      --  Do some needed initializations

      Snames.Initialize;
      Set_Program_Name ("gprname");

      GPR.Initialize (No_Project_Tree);

      GPR.Tree.Initialize (Root_Environment, Flags);
      GPR.Env.Initialize_Default_Project_Path
        (Root_Environment.Project_Path,
         Target_Name => GprConfig.Sdefault.Hostname);

      GPR.Tree.Initialize (Tree);

      Sources.Set_Last (0);
      Source_Directories.Set_Last (0);
      Foreign_Sources.Set_Last (0);
      Languages.Set_Last (0);

      --  Initialize the compiler switches

      Args := new Argument_List (1 .. Preproc_Switches'Length + 6);
      Args (1) := new String'("-c");
      Args (2) := new String'("-gnats");
      Args (3) := new String'("-gnatu");
      Args (4 .. 3 + Preproc_Switches'Length) := Preproc_Switches;
      Args (4 + Preproc_Switches'Length) := new String'("-x");
      Args (5 + Preproc_Switches'Length) := new String'("ada");

      --  Get the path and file names

      Path_Name := new
        String (1 .. File_Path'Length + Project_File_Extension'Length);
      Path_Last := File_Path'Length;

      if File_Names_Case_Sensitive then
         Path_Name (1 .. Path_Last) := File_Path;
      else
         Path_Name (1 .. Path_Last) := To_Lower (File_Path);
      end if;

      Path_Name (Path_Last + 1 .. Path_Name'Last) :=
        Project_File_Extension;

      --  Get the end of directory information, if any

      for Index in reverse 1 .. Path_Last loop
         if Path_Name (Index) = Directory_Separator then
            Directory_Last := Index;
            exit;
         end if;
      end loop;

      if Path_Last < Project_File_Extension'Length + 1
        or else Path_Name
          (Path_Last - Project_File_Extension'Length + 1 .. Path_Last)
        /= Project_File_Extension
      then
         Path_Last := Path_Name'Last;
      end if;

      Output_Name := new String'(To_Lower (Path_Name (1 .. Path_Last)));
      Output_Name_Last := Output_Name'Last - 4;

      --  If there is already a project file with the specified name, parse
      --  it to get the components that are not automatically generated.

      if Is_Regular_File (Output_Name (1 .. Path_Last)) then
         if Opt.Verbose_Mode then
            Put ("Parsing already existing project file """);
            Put (Output_Name.all);
            Put_Line ("""");
         end if;

      else
         declare
            File : File_Type;
         begin
            Create (File, Out_File, Output_Name.all);
            Put (File, "project ");
            Put (File, Path_Name (Directory_Last + 1 .. Output_Name_Last));
            Put_Line (File, " is");
            Put (File, "end ");
            Put (File, Path_Name (Directory_Last + 1 .. Output_Name_Last));
            Put_Line (File, ";");
            Close (File);
            Opt.No_Backup := True;
         end;
      end if;

      GPR.Attr.PM.Remove_Unknown_Packages;

      Part.Parse
        (In_Tree                => Tree,
         Project                => Project_Node,
         Project_File_Name      => Output_Name.all,
         Errout_Handling        => Part.Finalize_If_Error,
         Store_Comments         => True,
         Is_Config_File         => False,
         Env                    => Root_Environment,
         Current_Directory      => Get_Current_Dir,
         Packages_To_Check      => Packages_To_Check_By_Gprname);

      --  Fail if parsing was not successful

      if No (Project_Node) then
         GPR.Com.Fail ("parsing of existing project file failed");

      elsif Project_Qualifier_Of (Project_Node, Tree) = Aggregate then
         GPR.Com.Fail ("aggregate projects are not supported");

      elsif Project_Qualifier_Of (Project_Node, Tree) =
        Aggregate_Library
      then
         GPR.Com.Fail ("aggregate library projects are not supported");

      else
         --  If parsing was successful, remove the components that are
         --  automatically generated, if any, so that they will be
         --  unconditionally added later.

         --  Remove the with clause for the naming project file

         declare
            With_Clause : Project_Node_Id :=
              First_With_Clause_Of (Project_Node, Tree);
            Previous    : Project_Node_Id := Empty_Project_Node;

         begin
            while Present (With_Clause) loop
               if GPR.Tree.Name_Of (With_Clause, Tree) =
                 Project_Naming_Id
               then
                  if No (Previous) then
                     Set_First_With_Clause_Of
                       (Project_Node, Tree,
                        To => Next_With_Clause_Of (With_Clause, Tree));
                  else
                     Set_Next_With_Clause_Of
                       (Previous, Tree,
                        To => Next_With_Clause_Of (With_Clause, Tree));
                  end if;

                  exit;
               end if;

               Previous := With_Clause;
               With_Clause := Next_With_Clause_Of (With_Clause, Tree);
            end loop;
         end;

         --  Remove attribute declarations of Source_Files,
         --  Source_List_File, Source_Dirs, Languages and the declaration of
         --  package Naming, if they exist, but preserve the comments
         --  attached to these nodes.

         declare
            Declaration  : Project_Node_Id :=
              First_Declarative_Item_Of
                (Project_Declaration_Of
                   (Project_Node, Tree),
                 Tree);
            Previous     : Project_Node_Id := Empty_Project_Node;
            Current_Node : Project_Node_Id := Empty_Project_Node;

            Name         : Name_Id;
            Kind_Of_Node : Project_Node_Kind;
            Comments     : Project_Node_Id;

         begin
            while Present (Declaration) loop
               Current_Node := Current_Item_Node (Declaration, Tree);

               Kind_Of_Node := Kind_Of (Current_Node, Tree);

               if Kind_Of_Node = N_Attribute_Declaration or else
                 Kind_Of_Node = N_Package_Declaration
               then
                  Name := GPR.Tree.Name_Of (Current_Node, Tree);

                  if Name = Name_Source_Files or else
                    Name = Name_Source_List_File or else
                    Name = Name_Source_Dirs or else
                    Name = Name_Languages or else
                    Name = Name_Naming
                  then
                     Comments :=
                       Tree.Project_Nodes.Table (Current_Node).Comments;

                     if Name = Name_Source_Files then
                        Source_Files_Comments := Comments;

                     elsif Name = Name_Source_List_File then
                        Source_List_File_Comments := Comments;

                     elsif Name = Name_Source_Dirs then
                        Source_Dirs_Comments := Comments;

                     elsif Name = Name_Languages then
                        Languages_Comments := Comments;

                     elsif Name = Name_Naming then
                        Naming_Package_Comments := Comments;
                     end if;

                     if No (Previous) then
                        Set_First_Declarative_Item_Of
                          (Project_Declaration_Of (Project_Node, Tree),
                           Tree,
                           To => Next_Declarative_Item
                             (Declaration, Tree));

                     else
                        Set_Next_Declarative_Item
                          (Previous, Tree,
                           To => Next_Declarative_Item
                             (Declaration, Tree));
                     end if;

                  else
                     Previous := Declaration;
                  end if;
               end if;

               Declaration := Next_Declarative_Item (Declaration, Tree);
            end loop;
         end;
      end if;

      if Directory_Last /= 0 then
         Output_Name (1 .. Output_Name_Last - Directory_Last) :=
           Output_Name (Directory_Last + 1 .. Output_Name_Last);
         Output_Name_Last := Output_Name_Last - Directory_Last;
      end if;

      --  Get the project name id

      Name_Len := Output_Name_Last;
      Name_Buffer (1 .. Name_Len) := Output_Name (1 .. Name_Len);
      Output_Name_Id := Name_Find;

      --  Create the project naming file name

      Project_Naming_Last := Output_Name_Last;
      Project_Naming_File_Name :=
        new String'(Output_Name (1 .. Output_Name_Last) &
                      Naming_File_Suffix &
                      Project_File_Extension);
      Project_Naming_Last :=
        Project_Naming_Last + Naming_File_Suffix'Length;

      --  Get the project naming id

      Name_Len := Project_Naming_Last;
      Name_Buffer (1 .. Name_Len) :=
        Project_Naming_File_Name (1 .. Name_Len);
      Project_Naming_Id := Name_Find;

      Project_Naming_Last :=
        Project_Naming_Last + Project_File_Extension'Length;

      --  Create the source list file name

      Source_List_Last := Output_Name_Last;
      Source_List_Path :=
        new String'(Output_Name (1 .. Output_Name_Last) &
                      Source_List_File_Suffix);
      Source_List_Last :=
        Output_Name_Last + Source_List_File_Suffix'Length;

      --  Add the project file extension to the project name

      Output_Name
        (Output_Name_Last + 1 ..
           Output_Name_Last + Project_File_Extension'Length) :=
          Project_File_Extension;
      Output_Name_Last := Output_Name_Last + Project_File_Extension'Length;

      --  Back up project file if it already exists

      if not Opt.No_Backup
        and then Is_Regular_File (Path_Name (1 .. Path_Last))
      then
         declare
            Discard    : Boolean;
            Saved_Path : constant String :=
              Path_Name (1 .. Path_Last) & ".saved_";
            Nmb        : Natural;

         begin
            Nmb := 0;
            loop
               declare
                  Img : constant String := Nmb'Img;

               begin
                  if not Is_Regular_File
                    (Saved_Path & Img (2 .. Img'Last))
                  then
                     Copy_File
                       (Name     => Path_Name (1 .. Path_Last),
                        Pathname => Saved_Path & Img (2 .. Img'Last),
                        Mode     => Overwrite,
                        Success  => Discard);
                     exit;
                  end if;

                  Nmb := Nmb + 1;
               end;
            end loop;
         end;
      end if;

      --  Change the current directory to the directory of the project file,
      --  if any directory information is specified.

      if Directory_Last /= 0 then
         begin
            Change_Dir (Path_Name (1 .. Directory_Last));
         exception
            when Directory_Error =>
               GPR.Com.Fail
                 ("unknown directory """
                  & Path_Name (1 .. Directory_Last)
                  & """");
         end;
      end if;
   end Initialize;

   -------------
   -- Process --
   -------------

   procedure Process
     (Directories       : Argument_List;
      Name_Patterns     : Regexp_List;
      Excluded_Patterns : Regexp_List;
      Foreign_Patterns  : Foreign_Regexp_List)
  is
      procedure Process_Directory (Dir_Name : String; Recursively : Boolean);
      --  Look for Ada and foreign sources in a directory, according to the
      --  patterns. When Recursively is True, after looking for sources in
      --  Dir_Name, look also in its subdirectories, if any.

      -----------------------
      -- Process_Directory --
      -----------------------

      procedure Process_Directory (Dir_Name : String; Recursively : Boolean) is
         Matched : Matched_Type := No_Match;
         Str     : String (1 .. 2_000);
         Canon   : String (1 .. 2_000);
         Last    : Natural;
         Dir     : Dir_Type;
         Do_Process : Boolean := True;

         Temp_File_Name         : String_Access := null;
         Save_Last_Source_Index : Natural := 0;
         File_Name_Id           : Name_Id := No_Name;

         Current_Source : Source;

         Current_Language : Name_Id;

      begin
         --  Avoid processing the same directory more than once

         for Index in 1 .. Processed_Directories.Last loop
            if Processed_Directories.Table (Index).all = Dir_Name then
               Do_Process := False;
               exit;
            end if;
         end loop;

         if Do_Process then
            if Opt.Verbose_Mode then
               Put ("Processing directory """);
               Put (Dir_Name);
               Put_Line ("""");
            end if;

            Processed_Directories. Increment_Last;
            Processed_Directories.Table (Processed_Directories.Last) :=
              new String'(Dir_Name);

            --  Get the source file names from the directory. Fails if the
            --  directory does not exist.

            begin
               Open (Dir, Dir_Name);
            exception
               when Directory_Error =>
                  GPR.Com.Fail ("cannot open directory """ & Dir_Name & """");
            end;

            --  Process each regular file in the directory

            File_Loop : loop
               Read (Dir, Str, Last);
               exit File_Loop when Last = 0;

               --  Copy the file name and put it in canonical case to match
               --  against the patterns that have themselves already been put
               --  in canonical case.

               Canon (1 .. Last) := Str (1 .. Last);
               Canonical_Case_File_Name (Canon (1 .. Last));

               if Is_Regular_File
                    (Dir_Name & Directory_Separator & Str (1 .. Last))
               then
                  Matched := Match;

                  Name_Len := Last;
                  Name_Buffer (1 .. Name_Len) := Str (1 .. Last);
                  File_Name_Id := Name_Find;

                  --  First, check if the file name matches at least one of
                  --  the excluded expressions;

                  for Index in Excluded_Patterns'Range loop
                     if
                       Match (Canon (1 .. Last), Excluded_Patterns (Index))
                     then
                        Matched := Excluded;
                        exit;
                     end if;
                  end loop;

                  --  If it does not match any of the excluded expressions,
                  --  check if the file name matches at least one of the
                  --  regular expressions.

                  if Matched = Match then
                     Matched := No_Match;

                     for Index in Name_Patterns'Range loop
                        if
                          Match
                            (Canon (1 .. Last), Name_Patterns (Index))
                        then
                           Matched := Match;
                           exit;
                        end if;
                     end loop;
                  end if;

                  if Very_Verbose
                    or else (Matched = Match and then Opt.Verbose_Mode)
                  then
                     Put ("   Checking """);
                     Put (Str (1 .. Last));
                     Put_Line (""": ");
                  end if;

                  --  If the file name matches one of the regular expressions,
                  --  parse it to get its unit name.

                  if Matched = Match then
                     declare
                        FD : File_Descriptor;
                        Success : Boolean;
                        Saved_Output : File_Descriptor;
                        Saved_Error  : File_Descriptor;
                        Tmp_File     : Path_Name_Type;

                     begin
                        --  If we don't have the path of the compiler yet,
                        --  get it now. The compiler name may have a prefix,
                        --  so we get the potentially prefixed name.

                        if Gcc_Path = null then
                           Gcc_Path := Locate_Exec_On_Path (Gcc);

                           if Gcc_Path = null then
                              GPR.Com.Fail ("could not locate " & Gcc);
                           end if;
                        end if;

                        --  Create the temporary file

                        Tempdir.Create_Temp_File (FD, Tmp_File);

                        if FD = Invalid_FD then
                           GPR.Com.Fail
                             ("could not create temporary file");

                        else
                           Temp_File_Name :=
                             new String'(Get_Name_String (Tmp_File));
                        end if;

                        Args (Args'Last) :=
                          new String'
                            (Dir_Name & Directory_Separator & Str (1 .. Last));

                        --  Save the standard output and error

                        Saved_Output := Dup (Standout);
                        Saved_Error  := Dup (Standerr);

                        --  Set standard output and error to the temporary file

                        Dup2 (FD, Standout);
                        Dup2 (FD, Standerr);

                        --  And spawn the compiler

                        if Very_Verbose then
                           Put (Gcc_Path.all);

                           for J in Args'Range loop
                              if Args (J)'Length > 0 then
                                 Put (" " & Args (J).all);
                              end if;
                           end loop;

                           New_Line;
                        end if;

                        Spawn (Gcc_Path.all, Args.all, Success);

                        --  Restore the standard output and error

                        Dup2 (Saved_Output, Standout);
                        Dup2 (Saved_Error, Standerr);

                        --  Close the temporary file

                        Close (FD);

                        --  And close the saved standard output and error to
                        --  avoid too many file descriptors.

                        Close (Saved_Output);
                        Close (Saved_Error);

                        --  Now that standard output is restored, check if
                        --  the compiler ran correctly.

                        --  Read the lines of the temporary file:
                        --  they should contain the kind and name of the unit.

                        declare
                           File      : Text_File;
                           Text_Line : String (1 .. 1_000);
                           Text_Last : Natural;

                        begin
                           Open (File, Temp_File_Name.all);

                           if not Is_Valid (File) then
                              GPR.Com.Fail
                                ("could not read temporary file " &
                                 Temp_File_Name.all);
                           end if;

                           Save_Last_Source_Index := Sources.Last;

                           if End_Of_File (File) then
                              if Opt.Verbose_Mode then
                                 if not Success then
                                    Put ("      (process died) ");
                                 end if;
                              end if;

                           else
                              Line_Loop : while not End_Of_File (File) loop
                                 Get_Line (File, Text_Line, Text_Last);

                                 if Very_Verbose then
                                    Put_Line (Text_Line (1 .. Text_Last));
                                 end if;

                                 --  Find the first closing parenthesis

                                 Char_Loop : for J in 1 .. Text_Last loop
                                    if Text_Line (J) = ')' then
                                       if J >= 13 and then
                                         Text_Line (1 .. 4) = "Unit"
                                       then
                                          --  Add entry to Sources table

                                          Name_Len := J - 12;
                                          Name_Buffer (1 .. Name_Len) :=
                                            Text_Line (6 .. J - 7);
                                          Current_Source :=
                                            (Unit_Name  => Name_Find,
                                             File_Name  => File_Name_Id,
                                             Index => 0,
                                             Spec  => Text_Line (J - 5 .. J) =
                                                        "(spec)");

                                          Sources.Append (Current_Source);
                                       end if;

                                       exit Char_Loop;
                                    end if;
                                 end loop Char_Loop;
                              end loop Line_Loop;
                           end if;

                           if Save_Last_Source_Index = Sources.Last then
                              if Opt.Verbose_Mode then
                                 Put_Line ("      not a unit");
                              end if;

                           else
                              Add_Language (Name_Ada);

                              if Sources.Last >
                                   Save_Last_Source_Index + 1
                              then
                                 for Index in Save_Last_Source_Index + 1 ..
                                                Sources.Last
                                 loop
                                    Sources.Table (Index).Index :=
                                      Int (Index - Save_Last_Source_Index);
                                 end loop;
                              end if;

                              for Index in Save_Last_Source_Index + 1 ..
                                             Sources.Last
                              loop
                                 Current_Source := Sources.Table (Index);

                                 if Opt.Verbose_Mode then
                                    if Current_Source.Spec then
                                       Put ("      spec of ");

                                    else
                                       Put ("      body of ");
                                    end if;

                                    Put_Line
                                      (Get_Name_String
                                         (Current_Source.Unit_Name));
                                 end if;
                              end loop;
                           end if;

                           Close (File);

                           Delete_File (Temp_File_Name.all, Success);
                        end;
                     end;

                  --  File name matches none of the regular expressions

                  else
                     --  If file is not excluded, see if this is foreign source

                     if Matched /= Excluded then
                        for Index in Foreign_Patterns'Range loop
                           if Match (Canon (1 .. Last),
                                     Foreign_Patterns (Index).Pattern)
                           then
                              Matched := Match;
                              Current_Language :=
                                Foreign_Patterns (Index).Language;
                              Add_Language (Current_Language);
                              exit;
                           end if;
                        end loop;
                     end if;

                     if Very_Verbose then
                        case Matched is
                           when No_Match =>
                              Put_Line ("no match");

                           when Excluded =>
                              Put_Line ("excluded");

                           when Match =>
                              Put_Line ("foreign source");
                        end case;
                     end if;

                     if Matched = Match then

                        --  Add foreign source file name

                        Name_Len := 0;
                        Add_Str_To_Name_Buffer (Canon (1 .. Last));
                        Foreign_Sources.Append
                          ((File_Name => Name_Find,
                            Language  => Current_Language));
                     end if;
                  end if;
               end if;
            end loop File_Loop;

            Close (Dir);
         end if;

         --  If Recursively is True, call itself for each subdirectory.
         --  We do that, even when this directory has already been processed,
         --  because all of its subdirectories may not have been processed.

         if Recursively then
            Open (Dir, Dir_Name);

            loop
               Read (Dir, Str, Last);
               exit when Last = 0;

               --  Do not call itself for "." or ".."

               if Is_Directory
                    (Dir_Name & Directory_Separator & Str (1 .. Last))
                 and then Str (1 .. Last) /= "."
                 and then Str (1 .. Last) /= ".."
               then
                  Process_Directory
                    (Dir_Name & Directory_Separator & Str (1 .. Last),
                     Recursively => True);
               end if;
            end loop;

            Close (Dir);
         end if;
      end Process_Directory;

   --  Start of processing for Process

   begin
      Processed_Directories.Set_Last (0);

      --  Process each directory

      for Index in Directories'Range  loop

         declare
            Dir_Name    : constant String := Directories (Index).all;
            Last        : Natural := Dir_Name'Last;
            Recursively : Boolean := False;
            Found       : Boolean;
            Canonical   : String (1 .. Dir_Name'Length) := Dir_Name;

         begin
            Canonical_Case_File_Name (Canonical);

            Found := False;
            for J in 1 .. Source_Directories.Last loop
               if Source_Directories.Table (J).all = Canonical then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Source_Directories.Append (new String'(Canonical));
            end if;

            if Dir_Name'Length >= 4
              and then (Dir_Name (Last - 2 .. Last) = "/**")
            then
               Last := Last - 3;
               Recursively := True;
            end if;

            Process_Directory (Dir_Name (Dir_Name'First .. Last), Recursively);
         end;

      end loop;
   end Process;

end GPRName;
