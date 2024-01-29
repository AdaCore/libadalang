with Ada.Characters.Handling;
with Ada.Containers.Generic_Sort;
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package body Xrefs is

   procedure Skip_Inst_Info (S : String; Cursor : in out Natural);

   ----------------
   -- File_Index --
   ----------------

   function File_Index
     (Files    : in out File_Table_Type;
      Filename : String) return File_Index_Type
   is
      Basename : constant Unbounded_String :=
        To_Unbounded_String (+Base_Name (Create (+Filename)));
      Cur      : Filename_Maps.Cursor;
      Inserted : Boolean;
   begin
      Files.Map.Insert (Basename, File_Index_Type'First, Cur, Inserted);
      if Inserted then
         Files.Table.Append (Basename);
         Files.Map.Replace_Element (Cur, Files.Table.Last_Index);
         Files.Sort_Indexes := Sort_Index_Vectors.Empty_Vector;
      end if;

      return Filename_Maps.Element (Cur);
   end File_Index;

   -----------------
   -- Comes_First --
   -----------------

   function Comes_First
     (Files : in out File_Table_Type; L, R : File_Index_Type) return Boolean
   is
      Table    : Filename_Vectors.Vector renames Files.Table;
      Sort_Idx : Sort_Index_Vectors.Vector renames Files.Sort_Indexes;
   begin
      --  The sort indexes are missing: compute them
      if Sort_Idx.Is_Empty then
         declare
            Sort_Build_Array : array (1 .. Sort_Index_Type (Table.Last_Index))
              of File_Index_Type;

            function Before (L, R : Sort_Index_Type) return Boolean;
            procedure Swap (L, R : Sort_Index_Type);

            ------------
            -- Before --
            ------------

            function Before (L, R : Sort_Index_Type) return Boolean is
               L_Name : constant String := +Table (Sort_Build_Array (L));
               R_Name : constant String := +Table (Sort_Build_Array (R));
            begin
               return L_Name < R_Name;
            end Before;

            ----------
            -- Swap --
            ----------

            procedure Swap (L, R : Sort_Index_Type) is
               Tmp : constant File_Index_Type := Sort_Build_Array (L);
            begin
               Sort_Build_Array (L) := Sort_Build_Array (R);
               Sort_Build_Array (R) := Tmp;
            end Swap;

            procedure Sort is new Ada.Containers.Generic_Sort
              (Sort_Index_Type, Before, Swap);

         begin
            for I in Sort_Build_Array'Range loop
               Sort_Build_Array (I) := File_Index_Type (I);
               Sort_Idx.Append (I);
            end loop;
            Sort (Sort_Build_Array'First, Sort_Build_Array'Last);

            for I in Sort_Build_Array'Range loop
               Sort_Idx.Replace_Element (Sort_Build_Array (I), I);
            end loop;
         end;
      end if;

      return Files.Sort_Indexes (L) < Files.Sort_Indexes (R);
   end Comes_First;

   package String_Maps is new Ada.Containers.Hashed_Maps
     (Unbounded_String,
      Natural,
      Ada.Strings.Unbounded.Hash,
      "=");

   type Dep_Index_Type is new Positive;

   package Deps_Vectors is new Ada.Containers.Vectors
     (Dep_Index_Type, File_Index_Type);

   --------------------
   -- Skip_Inst_Info --
   --------------------

   procedure Skip_Inst_Info (S : String; Cursor : in out Natural) is
      Level           : Natural := 0;
      Dummy_Natural   : Natural;
      Dummy_Character : Character;
   begin
      --  Skip generic instantiation information, if present

      while Cursor <= S'Last and then S (Cursor) = '[' loop
         Dummy_Character := Read_Character (S, Cursor);
         pragma Assert (Dummy_Character = '[');
         Dummy_Natural := Read_Natural (S, Cursor);
         if S (Cursor) = '|' then
            Cursor := Cursor + 1;
            Dummy_Natural := Read_Natural (S, Cursor);
         end if;
         Level := Level + 1;
      end loop;

      for I in 1 .. Level loop
         if Read_Character (S, Cursor) /= ']' then
            raise Program_Error;
         end if;
      end loop;
   end Skip_Inst_Info;

   -------------------
   -- Read_LI_Xrefs --
   -------------------

   procedure Read_LI_Xrefs
     (LI_Filename : String;
      Files       : in out File_Table_Type;
      Xrefs       : out Unit_Xrefs_Vectors.Vector)
   is
      File     : File_Type;
      Unit_Map : String_Maps.Map;
      Deps     : Deps_Vectors.Vector;

      In_Xref       : Boolean := False;
      Current_File  : File_Index_Type;
      Entity_File   : File_Index_Type;
      Entity_Sloc   : Source_Location;
      Ref_File      : File_Index_Type;
      Current_Xrefs : Unit_Xrefs_Access;

      procedure Set_Ref_File (Index : File_Index_Type);
      procedure Process_Xref (S : String);

      ------------------
      -- Set_Ref_File --
      ------------------

      procedure Set_Ref_File (Index : File_Index_Type) is
         use String_Maps;

         Name  : constant String := Filename (Files, Index);
         C     : constant Cursor := Unit_Map.Find (+Name);
      begin
         if Has_Element (C) then
            Ref_File := Index;
            Current_Xrefs := Xrefs (Element (C));
         else
            Current_Xrefs := null;
         end if;
      end Set_Ref_File;

      ------------------
      -- Process_Xref --
      ------------------

      procedure Process_Xref (S : String) is
         Cursor     : Natural := S'First;
         First_Num  : constant Natural := Read_Natural (S, Cursor);
         First_Char : constant Character := Read_Character (S, Cursor);

         Line, Column : Natural;
         Type_Char    : Character;
      begin
         if First_Char = '|' then
            Set_Ref_File (Deps (Dep_Index_Type (First_Num)));
            Line := Read_Natural (S, Cursor);
            Type_Char := Read_Character (S, Cursor);
         else
            Type_Char := First_Char;
            Line := First_Num;
         end if;

         if S (Cursor) = '<' then
            --  This reference belongs to a pragma Import/Export. Just discard
            --  the symbol information.
            Skip_Until (S, Cursor, '>');
            Cursor := Cursor + 1;
         end if;

         Column := Read_Natural (S, Cursor);

         --  Skip generic instantiation information, if present.
         --
         --  TODO: Extract it and enhance this Xref processing engine to take
         --  it into account.
         Skip_Inst_Info (S, Cursor);

         if Cursor <= S'Last then
            raise Program_Error;
         end if;

         --  Ignore "end of spec/body" xrefs, which point on the ending
         --  semicolon of an entity body. LAL won't support that.

         if Current_Xrefs /= null and then Type_Char not in 'e' | 't' then
            Current_Xrefs.Xrefs.Append
              (Xref_Type'(Ref_Sloc => (Line_Number (Line),
                                       Column_Number (Column)),
                          Ref_File => Ref_File,
                          Entity_Sloc => Entity_Sloc,
                          Entity_File => Entity_File,
                          Error => False));
         end if;
      end Process_Xref;

   begin
      Open (File, In_File, LI_Filename);
      loop
         begin
            declare
               Line        : constant String := Get_Line (File);
               Line_Letter : constant Character :=
                 (if Line'Length > 0
                  then Line (Line'First)
                  else ASCII.NUL);
            begin
               if Line'Length = 0 then
                  In_Xref := False;
               end if;

               case Line_Letter is
               when 'U' =>
                  declare
                     Chunks   : constant Slice_Array := Split (Line);
                     Filename : constant String := Get (Line, Chunks (3));
                  begin
                     if Unit_Map.Contains (+Filename) then
                        --  If a package specification is a simple generic
                        --  instantiation, the ALI file will contains two units
                        --  associated to the same file: a body and a spec
                        --  located in the ADS file. In that case, do not add
                        --  it twice.
                        null;
                     else
                        Xrefs.Append (new Unit_Xrefs_Type'
                                       (Unit  => File_Index (Files, Filename),
                                        Xrefs => <>));
                        Unit_Map.Insert (+Filename, Xrefs.Last_Index);
                     end if;
                  end;

               when 'D' =>
                  declare
                     Chunks   : constant Slice_Array := Split (Line);
                     Filename : constant String := Get (Line, Chunks (2));
                     Index    : constant File_Index_Type :=
                       File_Index (Files, Filename);
                  begin
                     Deps.Append (Index);
                  end;

               when 'X' =>
                  In_Xref := True;
                  declare
                     Chunks : constant Slice_Array := Split (Line);
                     Dep    : constant Dep_Index_Type :=
                       Dep_Index_Type'Value (Get (Line, Chunks (2)));
                  begin
                     Current_File := Deps (Dep);
                  end;

               when '.' =>
                  if In_Xref then
                     if Line (Line'First .. Line'First + 1) /= ". " then
                        raise Program_Error;
                     end if;

                     for Chunk of Split (Line (Line'First + 2 .. Line'Last))
                     loop
                        Process_Xref (Get (Line, Chunk));
                     end loop;
                  end if;

               when others =>
                  if In_Xref then
                     declare
                        Cursor     : Natural := Line'First;
                        Line_Nbr   : constant Natural :=
                          Read_Natural (Line, Cursor);
                        Type_Char  : constant Character :=
                          Read_Character (Line, Cursor)
                          with Unreferenced;
                        Column     : constant Natural :=
                          Read_Natural (Line, Cursor);
                        Level_Char : constant Character :=
                          Read_Character (Line, Cursor)
                          with Unreferenced;
                     begin
                        Set_Ref_File (Current_File);
                        Entity_File := Current_File;
                        Entity_Sloc :=
                          (Line_Number (Line_Nbr), Column_Number (Column));
                        --  First chunk is the entity name: skip it, the rest
                        --  is the actual xref data.

                        --  Skip the entity name (either regular identifier or
                        --  "XXX" or 'X')...
                        if Line (Cursor) = '"' then
                           Cursor := Cursor + 1;
                           Skip_Until (Line, Cursor, '"');
                           Cursor := Cursor + 1;
                        elsif Line (Cursor) = ''' then
                           Cursor := Cursor + 3;
                        else
                           while
                             Cursor <= Line'Last
                             and then
                               (Ada.Characters.Handling.Is_Alphanumeric
                                  (Line (Cursor))
                                or else Line (Cursor) = '_')
                           loop
                              Cursor := Cursor + 1;
                           end loop;
                        end if;

                        --  ... and the potential suffixes: renameref
                        --  (=line:col).
                        if Cursor <= Line'Last and then Line (Cursor) = '='
                        then
                           declare
                              Dummy_Char : constant Character :=
                                Read_Character (Line, Cursor);
                              pragma Assert (Dummy_Char = '=');

                              Dummy : Natural := Read_Natural (Line, Cursor);
                           begin
                              if Read_Character (Line, Cursor) /= ':' then
                                 raise Program_Error;
                              end if;
                              Dummy := Read_Natural (Line, Cursor);
                           end;
                        end if;

                        --  instref ([file|line])
                        Skip_Inst_Info (Line, Cursor);

                        --  typeref (various info enclosed in {}, () or <>)

                        loop
                           if Cursor > Line'Last then
                              exit;
                           elsif Line (Cursor) = '{' then
                              Skip_Until (Line, Cursor, '}');
                              Cursor := Cursor + 1;
                           elsif Line (Cursor) = '(' then
                              Skip_Until (Line, Cursor, ')');
                              Cursor := Cursor + 1;
                           elsif Line (Cursor) = '<' then
                              Skip_Until (Line, Cursor, '>');
                              Cursor := Cursor + 1;
                           else
                              exit;
                           end if;
                        end loop;

                        declare
                           Chunks     : constant Slice_Array :=
                             Split (Line (Cursor .. Line'Last));
                        begin
                           for Chunk of Chunks loop
                              Process_Xref (Get (Line, Chunk));
                           end loop;
                        end;
                     end;
                  end if;
               end case;
            end;
         exception
            when Ada.IO_Exceptions.End_Error =>
               exit;
         end;
      end loop;
      Close (File);
   end Read_LI_Xrefs;

   ---------
   -- Put --
   ---------

   procedure Put (Files : File_Table_Type; X : Xref_Type) is

      procedure Put_Ref (File : File_Index_Type; Sloc : Source_Location);

      -------------
      -- Put_Ref --
      -------------

      procedure Put_Ref (File : File_Index_Type; Sloc : Source_Location) is
      begin
         Put (Filename (Files, File) & ':' & Image (Sloc));
      end Put_Ref;

   begin
      Put_Ref (X.Ref_File, X.Ref_Sloc);
      Put (" => ");
      if X.Error then
         Put ("ERROR");
      else
         Put_Ref (X.Entity_File, X.Entity_Sloc);
      end if;
   end Put;

   ----------
   -- Sort --
   ----------

   procedure Sort
     (Files : in out File_Table_Type; Xrefs : in out Xref_Vectors.Vector)
   is

      function Before (L, R : Positive) return Boolean;
      procedure Swap (L, R : Positive);

      ------------
      -- Before --
      ------------

      function Before (L, R : Positive) return Boolean is
         L_Xref : constant Xref_Type := Xrefs (L);
         R_Xref : constant Xref_Type := Xrefs (R);
      begin
         if Comes_First (Files, L_Xref.Ref_File, R_Xref.Ref_File) then
            return True;
         elsif L_Xref.Ref_File /= R_Xref.Ref_File then
            return False;

         else
            return L_Xref.Ref_Sloc < R_Xref.Ref_Sloc;
         end if;
      end Before;

      ----------
      -- Swap --
      ----------

      procedure Swap (L, R : Positive) is
         Temp : constant Xref_Type := Xrefs (L);
      begin
         Xrefs (L) := Xrefs (R);
         Xrefs (R) := Temp;
      end Swap;

      procedure Sort is new Ada.Containers.Generic_Sort
        (Positive, Before, Swap);
   begin
      Sort (Xrefs.First_Index, Xrefs.Last_Index);
   end Sort;

   -----------------------
   -- Remove_Duplicates --
   -----------------------

   procedure Remove_Duplicates (Xrefs : in out Xref_Vectors.Vector)
   is
      Last_Xref : Xref_Type := (Error => True, others => <>);
      Out_Vec : Xref_Vectors.Vector;
   begin
      for Xref of Xrefs loop
         if Last_Xref.Error or else Xref.Ref_Sloc /= Last_Xref.Ref_Sloc then
            Out_Vec.Append (Xref);
         end if;
         Last_Xref := Xref;
      end loop;

      Xrefs := Out_Vec;
   end Remove_Duplicates;

   ----------
   -- Sort --
   ----------

   procedure Sort
     (Files : in out File_Table_Type; Xrefs : in out Unit_Xrefs_Vectors.Vector)
   is

      function Before (L, R : Positive) return Boolean;
      procedure Swap (L, R : Positive);

      ------------
      -- Before --
      ------------

      function Before (L, R : Positive) return Boolean is
         L_Unit : constant Unit_Xrefs_Access := Xrefs (L);
         R_Unit : constant Unit_Xrefs_Access := Xrefs (R);
      begin
         return Comes_First (Files, L_Unit.Unit, R_Unit.Unit);
      end Before;

      ----------
      -- Swap --
      ----------

      procedure Swap (L, R : Positive) is
         Temp : constant Unit_Xrefs_Access := Xrefs (L);
      begin
         Xrefs (L) := Xrefs (R);
         Xrefs (R) := Temp;
      end Swap;

      procedure Sort is new Ada.Containers.Generic_Sort
        (Positive, Before, Swap);
   begin
      Sort (Xrefs.First_Index, Xrefs.Last_Index);
   end Sort;

   ---------
   -- Put --
   ---------

   procedure Put (Files : File_Table_Type; Xrefs : Unit_Xrefs_Vectors.Vector)
   is
   begin
      for Unit_Xrefs of Xrefs loop
         Put_Line ("== " & Filename (Files, Unit_Xrefs.Unit) & " ==");
         for Xref of Unit_Xrefs.Xrefs loop
            Put (Files, Xref);
            New_Line;
         end loop;
      end loop;
   end Put;

end Xrefs;
