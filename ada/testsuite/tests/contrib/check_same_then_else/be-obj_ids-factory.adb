------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- The CodePeer technology was originally developed by SofCheck, Inc.       --
------------------------------------------------------------------------------

with Std_Output; use Std_Output;

with BE.Driver;

with BE.Expr_Ids.Factory;

with BE.Obj_Ids.Aliasing;
with BE.Obj_Ids.Region_Information;
with BE.Obj_Ids.Info;
with BE.Obj_Ids.SCIL_Extension;
with BE.Obj_Ids.SCIL_Extension.Update;
with BE.Obj_Ids.Tracked_Components;
with BE.Obj_Ids.Obj_Id_Sets;
with BE.Obj_Ids.Matching_Types;
with BE.Obj_Ids.Known_Value_Sets;
with BE.Obj_Ids.Debugging;
with BE.Obj_Ids.Value_Tracker;
with BE.Procedure_Dependence;

with BE.SCIL;
with BE.SCIL.Debugging;
with BE.SCIL.Persistent_Source_Info; use BE.SCIL.Persistent_Source_Info;
with BE.SCIL.Expressions;
with BE.SCIL.Names;
with BE.SCIL.SCIL_Types;
with BE.SCIL.Dictionary_Streaming;

with BE.Externs_Mapping;

with BE.Source_Information;

with Utils.Spellings; use Utils.Spellings;
pragma Elaborate_All (Utils.Spellings);

with Utils.Arithmetic.Saturating_Big_Ints;
with Utils.Dictionary.Forwarding; use Utils.Dictionary.Forwarding;
with Utils.Messages;
with Utils.Storage_Management.Varying_Pools.Debug;
with Utils.Strings;
with Utils.Paging; use Utils.Paging;
with Utils.Paging.Persistent_Objects;
with Utils.Paging.Segment_Id_Sets; use Utils.Paging.Segment_Id_Sets;
with Utils.Persistent_Spellings.Globalize;

with BE.Debugging;
pragma Elaborate (BE.Debugging);
with ST.Mains.Debug_Flags;
pragma Elaborate_All (ST.Mains.Debug_Flags);
package body BE.Obj_Ids.Factory is

   Debug_Tog : constant Boolean :=
      Slow and then ST.Mains.Debug_Flags.Flag_Is_On ("obj_id_factory");
   function Debug is new BE.Debugging.Check_Debug_Flag (Debug_Tog);

   Debug_Shadow_Tog : constant Boolean := --  Slow and then
     ST.Mains.Debug_Flags.Flag_Is_On ("obj_id_shadow_factory");
   function Debug_Shadow is new BE.Debugging.Check_Debug_Flag (
      Debug_Shadow_Tog);

   Debug_Calls_Tog : constant Boolean :=
      Slow and then ST.Mains.Debug_Flags.Flag_Is_On ("obj_id_calls");
   function Debug_Calls is new BE.Debugging.Check_Debug_Flag (
      Debug_Calls_Tog);

   Debug_Refed_Objs_Tog : constant Boolean := --  Slow and then
     ST.Mains.Debug_Flags.Flag_Is_On ("refed_objs");
   function Debug_Refed_Objs is new BE.Debugging.Check_Debug_Flag (
      Debug_Refed_Objs_Tog);

   Debug_SCIL_Listings : constant Boolean :=
      Slow and then (ST.Mains.Debug_Flags.Flag_Is_On ("scil_listings"));

   Test_Uplevel_Consts : constant Boolean :=
     ST.Mains.Debug_Flags.Flag_Is_On ("uplevel_consts", On_By_Default => True);
   --  If True, then we notice local constants that are uplevel
   --  referenced and save their value when processing the
   --  enclosing procedure for use later by a nested procedure.

   Test_Limit_Of_Dereferences : constant Boolean :=
     ST.Mains.Debug_Flags.Flag_Is_On ("limit_derefs", On_By_Default => True);
   --  If True, then we create a wildcard rather than a simple
   --  deref when we get beyond a chain of two derefs.

   ------------- Local declarations -------------

   package Call_Site_Source is
      --  Source info extension for call-site objects

      function New_Source_Info
        (Callee_Source : SCIL.Source_Info_DPtr;
         Identifier    : String := "";
         Description   : String := "")
         return          SCIL.Source_Info_DPtr;
      --  Return source info given callee source,
      --  plus identifier/description to be provided instead

      function Copy_Source_Info
        (From_Obj : Call_Site_Obj_Id) return SCIL.Source_Info_DPtr;
      --  Given a Call_Site_Obj_Id, makes a copy of its Source_Info that is
      --  local to the current module, including further copying any associated
      --  Callee_Source Source_Info when present.

      type Source_Info is new SCIL.Source_Info with record
         Callee_Source : SCIL.Source_Info_DPtr;
         Identifier    : Utils.Persistent_Spellings.Persistent_Spelling;
         Description   : Utils.Persistent_Spellings.Persistent_Spelling;
      end record;

      for Source_Info'External_Tag use "CSTS";
      --  We want 4-letter external tags so they can
      --  take the place of the tag itself on disk

      function File_Name (Source : Source_Info) return Spelling;
      function Get_Srcpos
        (Source : Source_Info)
         return   Messages.Source_Position;

      function Identifier (Source : Source_Info) return Spelling;
      function Source_Info_Kind (Source : Source_Info) return Integer;
      function Description (Source : Source_Info) return Spelling;
      function Callee_Source
        (Source : Source_Info) return SCIL.Source_Info_DPtr;
      procedure Globalize_Tagged_Object
        (Forwarding      : Relocated_File_Id_Mapping_Ptr;
         Dict_Forwarding : Dictionary_Forwarding_Array_Ptr;
         Data            : in out Source_Info);
      --  Globalize the fields of a Call_Site_Source Source_Info record.

      procedure Track_Dictionary_Refs_For_Tagged_Object (Data : Source_Info);
      --  Record references for any fields of Data that are dictionary items.

   end Call_Site_Source;

   package body Call_Site_Source is

      use Persistent_Spellings;

      procedure Globalize_Spelling
        (Dict_Forwarding : Dictionary_Forwarding_Array_Ptr;
         Data            : in out Persistent_Spellings.Persistent_Spelling)
      renames Utils.Persistent_Spellings.Globalize;
      --  Globalize a persistent spelling

      function New_Source_Info
        (Callee_Source : SCIL.Source_Info_DPtr;
         Identifier    : String := "";
         Description   : String := "")
         return          SCIL.Source_Info_DPtr
      is
         --  Return source info given callee source,
         --  plus identifier/description to be provided instead
         Within : Varying_Pools.Within_Pool
           (Driver.Cur_Obj_Ids_Cumulative_Pool);
         pragma Unreferenced (Within);

         function Persistent_Spelling_Or_Null
           (Str  : String)
            return Persistent_Spelling
         is
         --  Return No_Persistent_Spelling if Str is the empty string
         --  else return Intern(Str).
         begin
            if Str = "" then
               return No_Persistent_Spelling;
            else
               return Intern (Str);
            end if;
         end Persistent_Spelling_Or_Null;

      begin  --  New_Source_Info
         return
           To_Source_Info_DPtr
             (new Source_Info'(Callee_Source,
                               Identifier  =>
                                 Persistent_Spelling_Or_Null (Identifier),
                               Description =>
                                 Persistent_Spelling_Or_Null (Description)));
      end New_Source_Info;

      function Copy_Source_Info
        (From_Obj : Call_Site_Obj_Id) return SCIL.Source_Info_DPtr
      is

         CS_Source_Info   : constant SCIL.Source_Info_DPtr :=
                              Obj_Ids.Source_Info (From_Obj);
         CS_Callee_Source : SCIL.Source_Info_DPtr;

         use type SCIL.Source_Info_DPtr;

      begin
         --  When the call site obj_id's Source_Info itself designates a
         --  call-site Source_Info record, make a copy of both the call-site
         --  Source_Info and the designated Source_Info. (Might we need to
         --  recursively follow even more levels of call-site Source_Info???)

         if To_Source_Info_Ptr (CS_Source_Info).all
           in Call_Site_Source.Source_Info'Class
         then
            CS_Callee_Source :=
              Call_Site_Source.Callee_Source
                (Call_Site_Source.Source_Info
                   (To_Source_Info_Ptr (CS_Source_Info).all));

            return Call_Site_Source.New_Source_Info
                     (Callee_Source =>
                        (To_Source_Info_DPtr
                           (new SCIL.Source_Info'Class'
                              (To_Source_Info_Ptr (CS_Callee_Source).all))));

         --  Otherwise just make a copy of the associated Source_Info

         else
            return To_Source_Info_DPtr
                     (new SCIL.Source_Info'Class'
                            (To_Source_Info_CPtr (CS_Source_Info).all));
         end if;
      end Copy_Source_Info;

      function File_Name (Source : Source_Info) return Spelling is
      --  Just return File_Name of Callee_Source
      begin
         return SCIL.File_Name
                  (To_Source_Info_CPtr (Source.Callee_Source).all);
      end File_Name;

      function Get_Srcpos
        (Source : Source_Info)
         return   Messages.Source_Position
      is
      --  Just return Srcpos of Callee_Source
      begin
         return SCIL.Get_Srcpos
                  (To_Source_Info_CPtr (Source.Callee_Source).all);
      end Get_Srcpos;

      function Identifier (Source : Source_Info) return Spelling is
      --  Return call-site-obj identifier if non-null
      begin
         if Source.Identifier = No_Persistent_Spelling then
            return SCIL.Identifier
                     (To_Source_Info_CPtr (Source.Callee_Source).all);
         else
            return Persistent_Spelling_To_Spelling (Source.Identifier);
         end if;
      end Identifier;

      function Source_Info_Kind (Source : Source_Info) return Integer is
         pragma Unreferenced (Source);
      begin
         return 99;
      end Source_Info_Kind;

      function Description (Source : Source_Info) return Spelling is
      --  Return call-site-obj description if non-null
      begin
         if Source.Description = No_Persistent_Spelling then
            return SCIL.Description
                     (To_Source_Info_CPtr (Source.Callee_Source).all);
         else
            return Persistent_Spelling_To_Spelling (Source.Description);
         end if;
      end Description;

      function Callee_Source
        (Source : Source_Info) return SCIL.Source_Info_DPtr
      is
      --  Return call-site-obj associated Callee_Source Source_Info reference
      begin
         return Source.Callee_Source;
      end Callee_Source;

      procedure Globalize_Tagged_Object
        (Forwarding      : Relocated_File_Id_Mapping_Ptr;
         Dict_Forwarding : Dictionary_Forwarding_Array_Ptr;
         Data            : in out Source_Info)
      is
      --  Globalize the fields of a Call_Site_Source Source_Info record.
      begin
         --  NOTE: These nodes are not created by the front end,
         --       but might be created during the execution of the
         --       inspector_be and then written back, so we want
         --       globalize to work properly on them.

         SCIL.Globalize_Disk_Ptr (Forwarding, Data.Callee_Source);
         Globalize_Spelling (Dict_Forwarding, Data.Identifier);
         Globalize_Spelling (Dict_Forwarding, Data.Description);

         --  Should we further globalize the call-site Source_Info associated
         --  with the Callee_Source field's designated Source_Info record???

         if False then
            BE.SCIL.Globalize_Tagged_Object
              (Forwarding, Dict_Forwarding,
               To_Source_Info_Ptr (Data.Callee_Source).all);
         end if;
      end Globalize_Tagged_Object;

      procedure Track_Dictionary_Refs_For_Tagged_Object (Data : Source_Info) is
      --  Record references for any fields of Data that are dictionary items.
         use SCIL.Dictionary_Streaming;
      begin
         --  Add the segment associated with the callee Source_Info disk
         --  pointer to the seg-id set. (This call might not be needed, since
         --  we're now making local copies of call-site Source_Info, but it
         --  doesn't hurt to add the segment id. ???)

         Add_Segment_Id_To_Set
           (Persistent_Objects.Disk_Ptr (Data.Callee_Source));

         --  Do we need to pass the buck for Callee_Source and track its
         --  designated Source_Info object as well???

         if False then
            BE.SCIL.Track_Dictionary_Refs_For_Tagged_Object
              (To_Source_Info_Ptr (Data.Callee_Source).all);
         end if;

         Add_Persistent_Spelling_Ref (Data.Identifier);
         Add_Persistent_Spelling_Ref (Data.Description);
      end Track_Dictionary_Refs_For_Tagged_Object;

   end Call_Site_Source;

   procedure Track_New_Obj_Id
     (Current_Proc   : SCIL.Procedure_Body;
      New_Obj_Id     : Obj_Id;
      Is_Referenced  : Boolean := False);
   --  Add to set of interesting" obj-ids, if appropriate.
   --  If New_Obj_Id is elementary or Is_Referenced is True, add
   --  it to set of referenced obj-ids.
   --  If is a component, add to list of tracked components
   --  for enclosing object.

   procedure Get_Sub_Object_Obj_Id
     (Current_Proc       : SCIL.Procedure_Body;
      Encloser_Obj       : access Obj_Table_Entry'Class;
      New_Obj            : in out Obj_Table_Entry'Class;
      Sub_Obj            : out Obj_Id;
      Read_Only          : Boolean := False;
      Shadow_Creation_Ok : Boolean := False)
   is
      --  Add entry to obj table associated with Obj, unless already there.
      --  Initialize obj table if necessary.
      --  Return obj-id designating new or preexisting entry in table.
      --  If entry not already in table, new entry is allocated
      --  in an "appropriate" storage pool and returned obj-id
      --  designates this newly allocated obj-table entry.

      Table_Ref : Obj_Table_Ref;
      use type Expr_Ids.Expr_Id_Kind; --  for "="
      use type SCIL.Procedure_Body;
      use type Region_Information.Procedure_Body_Info_Ptr;
      use type Driver.BE_Phase_Enum_Or_None;
   begin

      if Encloser_Obj.Sub_Obj_Obj_Table = null then
         if Read_Only then
            --  If no table, then no sub-obj.
            Sub_Obj := No_Obj_Id;
            return;
         end if;
         --  This must be the first component, so allocate an obj table
         declare
            Within : Varying_Pools.Within_Pool
              (Driver.Cur_Obj_Ids_Cumulative_Pool);
            pragma Unreferenced (Within);
            use type SCIL.Module; --  for "/="
            Enc_Module : SCIL.Module renames Enclosing_Module
                                               (Obj_Id (Encloser_Obj));
            Enc_Module_Ptr : constant Region_Information.Ptr_To_Module :=
               Region_Information.Associated_Module_Acc
                 (Region_Information.Associated_Region_Info (Enc_Module));
         begin

            pragma Assert (Enc_Module = Driver.Cur_SCIL_Module);
            if not (Enc_Module = Driver.Cur_SCIL_Module) then
               raise Program_Error;
            end if;
            Table_Ref                                                  :=
              new Obj_Table (Enc_Module_Ptr);
         end;
         Encloser_Obj.Sub_Obj_Obj_Table := Table_Ref;
      else
         --  There is already an obj-table
         Table_Ref := Encloser_Obj.Sub_Obj_Obj_Table;
      end if;

      if Driver.Cur_Phase /= Driver.Persistent_Import_Phase then
         --  Compute a hash value based on content of New_Obj.
         --  Note computed_hash is set when reading obj_ids from .scilx file.
         Compute_Hash (New_Obj);
      end if;

      if Read_Only then
         Sub_Obj := Obj_Tables.Lookup (Table_Ref.Table, New_Obj);
         if Sub_Obj = No_Obj_Id
           and then New_Obj.Kind = Indexed_Component_Obj_Kind
           and then Expr_Ids.Expr_Kind
                       (Indexed_Component_Obj_Table_Entry (New_Obj).
              Index_Expr_Id) =
                    Expr_Ids.Others_Expr
         then
            declare
               New_Indexed_Obj : Indexed_Component_Obj_Table_Entry renames
                 Indexed_Component_Obj_Table_Entry (New_Obj);
               use type Expr_Ids.Expr_Id; --  for "/="
            begin
               --  Find corresponding "others"
               --  TBD: What if index set of others doesn't match?
               New_Indexed_Obj.Index_Expr_Id :=
                  Expr_Ids.Factory.Find_Others_Expr_Id
                    (Current_Proc,
                     New_Indexed_Obj.Enclosing_Obj_Id);

               --  And lookup again
               if New_Indexed_Obj.Index_Expr_Id /= Expr_Ids.No_Expr_Id then
                  Compute_Hash (New_Obj);  --  Recompute the hash value
                  Sub_Obj := Obj_Tables.Lookup (Table_Ref.Table, New_Obj);
               end if;
            end;
         end if;
         return;
      end if;

      --  Find/Create obj-table entry in sub-obj table equiv to New_Obj
      if Driver.Cur_Phase not in Driver.Obj_Id_Phase_Enum
        and then Driver.Cur_Phase /= Driver.Persistent_Import_Phase
        and then not Shadow_Creation_Ok
      then
         Sub_Obj := Obj_Tables.Lookup (Table_Ref.Table, New_Obj);
         --  if Sub_Obj = No_Obj_Id then
         --  pragma Assert( False ); null;
         --  end if;
         return;
      else
         declare
            Within : Varying_Pools.Within_Pool
              (Driver.Cur_Obj_Ids_Cumulative_Pool);
            pragma Unreferenced (Within);
         begin
            Obj_Tables.Include (Table_Ref.Table, New_Obj, Sub_Obj);
         end;
      end if;

      --  (mireille) this assertion does not work when running with dbg-on
      --  scil_listings, since everyting is done in the cumulative_pool
      --  Sometimes called within obj-id-cumulative mapping e.g.
      --  when called from Update_DMOD_Mapping
      pragma Assert
        (Debug_SCIL_Listings
        or else (Driver.Cur_Phase in Driver.Obj_Id_Phase_Enum
                and then Driver.Cur_Temp_Pool =
                         Varying_Pools.Debug.Get_Current_Pool)
        or else Driver.Cur_Phase not in Driver.Obj_Id_Phase_Enum);

      if Driver.Cur_Phase in Driver.Obj_Id_Phase_Enum
        and then Current_Proc /= SCIL.No_Procedure_Body
        and then Region_Information.Associated_Region_Info (Current_Proc) /=
                 null
      then
         Track_New_Obj_Id (Current_Proc, Sub_Obj);
      end if;

      if Debug
        and then Driver.Cur_Phase not in Driver.Persistent_Import_Phase
      then
         Put_Line
           ("Get_Sub_Object_Obj_Id -- returning Sub_Obj " &
            Obj_Ids.Debugging.Obj_Id_Image (Sub_Obj, Current_Proc));
         Obj_Ids.Debugging.Dump_Obj_Id (Sub_Obj);
      end if;
   end Get_Sub_Object_Obj_Id;

   procedure Track_New_Obj_Id
     (Current_Proc   : SCIL.Procedure_Body;
      New_Obj_Id     : Obj_Id;
      Is_Referenced  : Boolean := False)
   is
      --  Add to set of interesting" obj-ids, if appropriate.
      --  If New_Obj_Id is elementary or Is_Referenced is True, add
      --  it to set of referenced obj-ids.
      --  If is a component, add to list of tracked components
      --  for enclosing object.

      --  Region info should already be filled in:
      use type Region_Information.Procedure_Body_Info_Ptr; --  for "/="

      Proc_Region_Info : constant Region_Information.Procedure_Body_Info_Ptr
         :=
         Region_Information.Associated_Region_Info (Current_Proc);
      pragma Assert (Proc_Region_Info /= null);

      Proc_Value_Mapping : constant Known_Value_Sets.Obj_Id_Value_Mapping :=
         Region_Information.Proc_Obj_Id_Value_Mapping (Proc_Region_Info);

      Referenced_Obj_Ids      : Obj_Id_Sets.Obj_Id_Set renames
        Region_Information.Proc_Referenced_Obj_Ids (Proc_Region_Info).all;
      Obj_Ids_Already_Tracked : Obj_Id_Sets.Obj_Id_Set renames
        Region_Information.Obj_Ids_Already_Tracked (Proc_Region_Info).all;

      use SCIL;
      New_Obj_Id_Type : constant SCIL_Type := Type_Of_Obj_Id (New_Obj_Id);

      Already_Tracked : constant Boolean :=
         Obj_Id_Sets.Is_Member (New_Obj_Id, Set => Obj_Ids_Already_Tracked);

   begin  --  Track_New_Obj_Id

      if Already_Tracked
        and then (not Is_Referenced
                 or else Obj_Id_Sets.Is_Member
                            (New_Obj_Id,
                             Set => Referenced_Obj_Ids))
      then
         --  We've already done the work to track this object.
         --  We're doing this test to stop the recursion that
         --  might be created by tracking global constant initialization.

         return;  --  Already tracked this object

      end if;

      --  Remember that we have tracked this object
      if not Already_Tracked then
         declare
            Within : Varying_Pools.Within_Pool (Driver.Cur_Temp_Pool);
            pragma Unreferenced (Within);
         begin
            Obj_Id_Sets.Include
              (New_Obj_Id,
               Within_Set => Obj_Ids_Already_Tracked);
         end;
      end if;

      if Is_Referenced
        or else Kind (New_Obj_Id_Type) in Elementary_Type_Kind
        or else Kind (New_Obj_Id_Type) = Incomplete_Type_Kind
      then
         --  Is_Referenced or Elementary, add it to the set of "referenced"
         --  obj ids.
         if Debug_Refed_Objs then
            --  Report any addition to referenced obj ids set
            if not Obj_Id_Sets.Is_Member
                     (New_Obj_Id,
                      Referenced_Obj_Ids)
            then
               Put_Line ("Track_New_Obj_Id: Newly ref'ed obj: ");
               Obj_Ids.Debugging.Dump_Obj_Id (New_Obj_Id);
            end if;
         end if;

         if not Obj_Id_Sets.Is_Member (New_Obj_Id, Referenced_Obj_Ids) then
            --  Add it to the refed-obj-id set
            declare
               Within_Cumulative_Pool : Varying_Pools.Within_Pool
                 (Driver.Cur_Obj_Ids_Cumulative_Pool);
               pragma Unreferenced (Within_Cumulative_Pool);
               use type Obj_Ids.Level_Type;
            begin
               Obj_Id_Sets.Include
                 (New_Obj_Id,
                  Within_Set => Referenced_Obj_Ids);

               if Is_Caller_Relevant (New_Obj_Id, Current_Proc) then
                  --  Also add it to the caller-relevant set
                  --  NOTE: We do this as we go rather than only at the
                  --       end so recursive calls can use it.
                  --       We do it again at the end because
                  --       what is caller-relevant might grow
                  --  TBD:  Is that extra pass necessary?
                  declare
                     Caller_Relevant_Objs : Obj_Id_Sets.Obj_Id_Set renames
                       Region_Information.Caller_Relevant_Refed_Obj_Ids (
                       Proc_Region_Info).all;
                  begin
                     Obj_Id_Sets.Include (New_Obj_Id, Caller_Relevant_Objs);
                     if Debug_Refed_Objs then
                        --  Make sure we aren't adding a "foreign" others
                        --  to the set of referenced objects
                        if Kind (New_Obj_Id) =
                           Indexed_Component_Obj_Kind
                        then
                           declare
                              Indexed_Obj_Id : constant
                                Indexed_Component_Obj_Id                 :=
                                 Indexed_Component_Obj_Id (New_Obj_Id);
                              Index_Expr_Id  : constant Expr_Ids.Expr_Id :=
                                Indexed_Obj_Id.Index_Expr_Id;
                              use Expr_Ids;
                           begin
                              case Expr_Kind (Index_Expr_Id) is
                                 when Others_Expr | Dynamic_Expr |
                                   Sliding_Expr =>
                                    --  Make sure enclosing proc is correct
                                    declare
                                       Proc : constant SCIL.Procedure_Body :=
                                          Expr_Ids.Factory.Expr_Enclosing_Proc
                                            (Index_Expr_Id);
                                    begin
                                       if Proc /= Current_Proc then
                                          Put_Line
                                            ("Proc mismatch for expr-id:");
                                          Put_Line
                                            (" Expected: " &
                                             SCIL.Debugging.Proc_Id
                                                (Current_Proc) &
                                             ", Found: " &
                                             SCIL.Debugging.Proc_Id (Proc) &
                                             " in:");
                                          Obj_Ids.Debugging.Dump_Obj_Id
                                            (New_Obj_Id);
                                       end if;
                                    end;

                                 when Fetch_Expr | Int_Literal_Expr =>
                                    --  These are not specific to a proc
                                    null;
                              end case;
                           end;
                        end if;
                     end if;
                  end;
               end if;

               if Test_Uplevel_Consts
                 and then Info.Is_Uplevel_Obj (New_Obj_Id)
                 and then Level_Num (New_Obj_Id) > 0
               then
                  --  Mark obj as being up-level referenced
                  New_Obj_Id.Is_Uplevel_Referenced := True;
                  --  If const, also mark enclosing obj
                  if Info.Obj_Is_Const (New_Obj_Id) then
                     declare
                        New_Whole_Obj : constant Obj_Id :=
                           Enclosing_Whole_Obj (New_Obj_Id);
                     begin
                        if Kind (New_Whole_Obj) in Whole_Obj_Kind
                          and then Level_Num (New_Whole_Obj) > 0
                        then
                           --  Create dependence on proc that initializes
                           --  this constant
                           declare
                              New_Whole_Obj_Decl : constant SCIL.Object_Decl
                                 :=
                                 Denotes (Whole_Obj_Id (New_Whole_Obj));
                           begin
                              New_Whole_Obj.Is_Uplevel_Referenced := True;

                              if SCIL.Kind (New_Whole_Obj_Decl) =
                                 SCIL.Standalone_Object_Decl_Kind
                              then
                                 declare
                                    Proc_Declaring_Obj : constant
                                      SCIL.Procedure_Body :=
                                       SCIL.Down
                                         (SCIL.Object_Decls.Container
                                             (
                                             SCIL.To_Standalone_Object_Decl
                                               (New_Whole_Obj_Decl)));
                                    use Region_Information;
                                 begin
                                    --  Create mutual dependence unless on self
                                    if Proc_Declaring_Obj /=
                                       Current_Proc
                                    then
                                       Procedure_Dependence.Add_Dependence
                                         (Procedure_Dependence_Mapping
                                             (Associated_Region_Info
                                                 (Current_Proc)),
                                          Proc_Declaring_Obj);
                                       Procedure_Dependence.Add_Dependence
                                         (Procedure_Dependence_Mapping
                                             (Associated_Region_Info
                                                 (Proc_Declaring_Obj)),
                                          Current_Proc);
                                    end if;
                                 end;
                              end if;  --  Up-level ref to standalone local
                                       --  const
                           end;
                        end if;  --  Up-level ref to local const
                     end;
                  end if;  --  Up-level ref to const
               end if; --  Up-level ref

               if Kind (New_Obj_Id_Type) in Composite_Type_Kind
                 and then not Region_Information.Is_Part_Of_Loop
                                (Proc_Region_Info)
                 and then Procedure_Dependence.Has_Dependence
                             (
                              Region_Information.Procedure_Dependence_Mapping
                                (Proc_Region_Info),
                              Current_Proc)
               then
                  --  This procedure has a recursive call on itself.
                  --  Make sure we iterate obj-id if outside a loop.
                  --  E.g. see I603-008 and I715-003
                  Region_Information.Set_Iterate_Obj_Id
                    (Proc_Region_Info,
                     True);
                  if Debug_Refed_Objs then
                     Put
                       ("Track_New_Obj_Id: Newly ref'ed composite obj" &
                        " causing iteration due to recursion:");
                     Obj_Ids.Debugging.User_Dump_Obj_Id (New_Obj_Id);
                     New_Line;
                  end if;
               end if;
            end;
         end if;
         --  also make sure that if there is an associated unknown_result,
         --  it also gets added to the set
         if Known_Value_Sets.Has_Associated_Unknown_Result_Obj
              (Proc_Value_Mapping,
               New_Obj_Id)
         then
            Known_Value_Sets.Set_Associated_Unknown_Result_Obj
              (Current_Proc,
               Proc_Value_Mapping,
               New_Obj_Id,
               Known_Value_Sets.Associated_Unknown_Result_Obj
                  (Proc_Value_Mapping,
                   New_Obj_Id));
         end if;

         Value_Tracker.Add_Ref_To_Global_Mapping (New_Obj_Id);

         if Already_Tracked then

            if Kind (New_Obj_Id) in Sub_Obj_Kind
              and then SCIL.Kind (New_Obj_Id_Type) in SCIL.Composite_Type_Kind
            then
               --  Check to see whether we are adding a new tracked
               --  subcomponent to a composite obj assigned earlier
               if Tracked_Components.Is_Value_Tracked_Component
                     (Current_Proc,
                      New_Obj_Id)
                 and then Tracked_Components.Encloser_Is_Member
                             (New_Obj_Id,
                              Region_Information.
                    Composite_Objs_Outside_Of_Loop (Proc_Region_Info).all)
               then

                  --  Make sure we iterate obj-id
                  Region_Information.Set_Iterate_Obj_Id
                    (Proc_Region_Info,
                     True);
               end if;
            end if;

            return;  --  Now we are all done

         end if;
      end if;

      case Kind (New_Obj_Id) is
         when Sub_Obj_Kind =>
            --  If object is a component, (possibly) track it.

            --  NOTE: Track_New_Component decides how
            --       to track a component based on whether it
            --       is statically identified, etc.

            declare
               Enclosing_Obj : constant Obj_Id :=
                  Enclosing_Obj_Id (Sub_Obj_Id (New_Obj_Id));
            begin
               if Enclosing_Obj /= No_Obj_Id then
                  Tracked_Components.Track_New_Component
                    (Current_Proc,
                     Composite_Obj => Enclosing_Obj,
                     New_Component => New_Obj_Id);

                  --  Do additional kind-specific tracking

                  case Sub_Obj_Kind'(Kind (New_Obj_Id)) is
                     when Dereference_Obj_Kind =>
                        --  We keep track of all the derefs for pointer alias
                        --  analysis
                        declare
                           Pointer_Obj : constant Obj_Id :=
                              Pointer_Obj_Id
                                (Dereference_Obj_Id (New_Obj_Id));
                        begin
                           --  Make sure the pointer obj is tracked
                           Track_Obj_Id_And_Enclosers
                             (Current_Proc,
                              Pointer_Obj);

                           Matching_Types.Track_New_Deref
                             (Current_Proc,
                              New_Obj_Id);

                           --  If pointer is EAV then EAV and UPE is
                           --  established
                           if Known_Value_Sets.Is_Externally_Assignable
                                (Proc_Value_Mapping,
                                 Pointer_Obj)
                           then
                              Known_Value_Sets.Set_Is_Externally_Assignable
                                (Proc_Value_Mapping,
                                 New_Obj_Id);
                              Known_Value_Sets.
                                Set_Unknown_Pointer_Exists_And_IVAL
                                 (Proc_Value_Mapping, New_Obj_Id);
                           elsif Known_Value_Sets.Unknown_Pointer_Exists
                                    (Proc_Value_Mapping,
                                     Pointer_Obj)
                           then
                              --  If pointer is UPE then this obj-id is UPE too
                              Known_Value_Sets.
                                Set_Unknown_Pointer_Exists_And_IVAL
                                 (Proc_Value_Mapping, New_Obj_Id);
                           end if;

                        end;
                     when Call_Side_Effects_Obj_Kind =>
                        --  We keep track of all call-side-effects objects
                        Matching_Types.Track_New_Side_Effect_Obj
                          (Current_Proc,
                           Call_Side_Effects_Obj_Id (New_Obj_Id));

                     when Selected_Component_Obj_Kind   |
                          Indexed_Component_Obj_Kind    |
                          Call_Site_Heap_Slice_Obj_Kind |
                          Sliding_Obj_Kind              |
                          Unknown_Result_Obj_Kind       =>

                        --  We used to simply inherit EAV from the encloser,
                        --  but
                        --  we now support constant components of non-constant
                        --  enclosers.  We don't inherit the EAV flag if the
                        --  component
                        --  is an up-level constant; if this is true, then we
                        --*do* know
                        --  its initial value (whereas EAV implies that we do
                        --  not know
                        --  the initial value).

                        if Known_Value_Sets.Is_Externally_Assignable
                              (Proc_Value_Mapping,
                               Enclosing_Obj)
                          and then not Info.Is_Uplevel_Const (New_Obj_Id)
                        then
                           Known_Value_Sets.Set_Is_Externally_Assignable
                             (Proc_Value_Mapping,
                              New_Obj_Id);
                        end if;

                        --  We simply inherit UPE from the encloser, even
                        --  for constant components.  We notice they
                        --  are constant during alias analysis, rather
                        --  than here, to avoid interfering with the
                        --  general rule that if a pointer is UPE, then
                        --  so is everything it points at.
                        if Known_Value_Sets.Unknown_Pointer_Exists
                             (Proc_Value_Mapping,
                              Enclosing_Obj)
                        then
                           Known_Value_Sets.
                             Set_Unknown_Pointer_Exists_And_IVAL
                                (Proc_Value_Mapping, New_Obj_Id);
                        end if;

                  end case;
               end if;  --  whether enclosing obj is non-null
            end;
         when Simple_Obj_Kind =>
            declare
            --  Use these booleans to set initial value of EAV
            --  Rule: EAV initially On for Uplevels, and IN[OUT], and
            --  for a composite: corresponds to EAV of encloser.
               Is_Uplevel_Obj       : constant Boolean :=
                  Info.Is_Uplevel_Obj (New_Obj_Id);
               Is_Initialized_Param : constant Boolean :=
                  Info.Is_Initialized_Param (New_Obj_Id);
               Is_Const             : constant Boolean :=
                  Info.Obj_Is_Const (New_Obj_Id);
               Is_Uplevel_Const     : constant Boolean :=
                  Is_Const and then Is_Uplevel_Obj;

            begin

               --  Set the EAV, UPE flag for this simple name
               --  but not only for integer type, because integer components of
               --  complex objects inherit their EAV from the composite object.
               if Is_Uplevel_Obj then
                  if not Is_Uplevel_Const then
                     --  EAV is true for all uplevels except for uplevel
                     --  constants
                     Known_Value_Sets.Set_Is_Externally_Assignable
                       (Proc_Value_Mapping,
                        New_Obj_Id);
                  end if;
                  if not Is_Const then
                     --  UPE is true for non-const uplevels
                     Known_Value_Sets.Set_Unknown_Pointer_Exists_And_IVAL
                       (Proc_Value_Mapping,
                        New_Obj_Id);
                  end if;
               elsif Is_Initialized_Param
                 and then (Kind (New_Obj_Id_Type) /= Pointer_Type_Kind
                          or else not
                           Obj_Ids.Info.Is_In_Param_Pointing_To_Unaliased_Obj
                             (New_Obj_Id,
                              Current_Proc))
               then
                  --  EAV is true for IN[OUT] params, unless is known to
                  --  be pointer to uninit obj, as for "this" param of
                  --  constructor
                  Known_Value_Sets.Set_Is_Externally_Assignable
                    (Proc_Value_Mapping,
                     New_Obj_Id);
               else
                  --  Local variables fall here, as do in-params
                  --  pointing to uninit objects.
                  --  EAV, UPE remain false.
                  null;
               end if;
            end;
         when others =>
            null;
      end case;

      if Debug_Shadow
        and then Driver.Cur_Phase not in Driver.Obj_Id_Phase_Enum
      then
         Put_Line
           ("Track_new_Obj_Id -- obj created not in obj-id-ph:" &
            Obj_Ids.Debugging.Obj_Id_Image (New_Obj_Id, Current_Proc));
         Obj_Ids.Debugging.Dump_Obj_Id (New_Obj_Id);
      end if;

   end Track_New_Obj_Id;

   function Desig_Type_To_Use
     (Pointer_Obj     : Obj_Id;
      Designated_Type : SCIL.SCIL_Type)
      return            SCIL.SCIL_Type
   is
      --  Return Designated_Type if /= SCIL.No_SCIL_Type, else get
      --  designated type from pointer_obj
      use type SCIL.SCIL_Type; --  for "/="
   begin
      if Designated_Type /= SCIL.No_SCIL_Type then
         return Designated_Type;
      else
         return SCIL.SCIL_Types.Designated_Type
                  (SCIL.Down (Type_Of_Obj_Id (Pointer_Obj)));
      end if;
   end Desig_Type_To_Use;

   function Find_Matching_Deref_Prefix
     (Current_Proc    : SCIL.Procedure_Body;
      Pointer_Obj     : Obj_Id;
      Collection_Obj  : Obj_Id;
      Designated_Type : SCIL.SCIL_Type)
      return            Dereference_Obj_Id
   is
      --  Find prefix of Pointer_Obj, if any, that is a deref whose type
      --  matches the designated type of the pointer, and whose collection
      --  matches the specified collection.
      --  If Designated_Type is /= No_SCIL_Type, then use that type rather
      --  than the designated type of the pointer object.
      --  Return No_Obj_Id if no such prefix exists.

      use type SCIL.SCIL_Type; --  for "="

      Desig_Type : constant SCIL.SCIL_Type :=
         Desig_Type_To_Use (Pointer_Obj, Designated_Type);

      Prefix : Obj_Id := Enclosing_Whole_Obj (Pointer_Obj);
   begin
      --  Loop over each deref within prefix
      while Prefix.Kind in Dereference_Obj_Kind loop
         declare
            Deref_Prefix : constant Dereference_Obj_Id :=
               Dereference_Obj_Id (Prefix);
         --  Just get a type-conversion of it.

         begin
            if Deref_Prefix.Enclosing_Obj_Id = Collection_Obj
              and then Type_Of_Obj_Id (Prefix) = Desig_Type
            then

               pragma Warnings (Off);
               --  Found a matching deref
               if not Includes_All_Reachable (Deref_Prefix)
                  --  TBD: We had to eliminate this because GNAT complains
                  --      about always true:
                 and then
                   Simple_Deref_Obj_Id (Deref_Prefix).Matching_Prefix_Count >=
                     Max_Prefix_Count
               then
                  pragma Warnings (On);
                  --  Matching deref is not a wildcard, but it has reached
                  --  the maximum prefix count.
                  --  Try to find preexisting wildcard one
                  declare
                     Result_Obj_Id : constant Dereference_Obj_Id :=
                        Dereference_Obj_Id (Find_Wildcard_Deref_Obj_Id
                                               (Current_Proc        =>
                       Current_Proc,
                                                Pointer_Base_Obj_Id => Prefix,
                                                Designated_Type     =>
                                                   Desig_Type_To_Use
                                                     (Pointer_Obj,
                                                      Designated_Type),
                                                Collection_Obj_Id   =>
                       Collection_Obj));
                  begin
                     if Result_Obj_Id /= Dereference_Obj_Id (No_Obj_Id) then
                        --  Found a preexisting wildcard one
                        return Result_Obj_Id;
                     end if;
                  end;

               end if;

               --  Just return the prefix
               return Deref_Prefix;

            end if;

            --  Go to the whole object enclosing the pointer of this deref
            Prefix := Enclosing_Whole_Obj (Deref_Prefix.Pointer_Obj_Id);
         end;
      end loop;

      --  No matching prefix
      return Dereference_Obj_Id (No_Obj_Id);

   end Find_Matching_Deref_Prefix;

   function Proc_Obj_Identifier (Proc_Obj : Obj_Id) return String is
      --  Return a string that identifies proc being called.
      --  If proc_obj is a deref, use selected component's id.
      --  If proc_obj is a simple obj, use its id directly.
      --  Return "" if neither of the above
      use type SCIL.SCIL_Node; --  for "+"
   begin
      case Kind (Proc_Obj) is
         when Simple_Obj_Kind =>
            --  Return denoted obj-decl's identifier
            return SCIL.Debugging.Ada_Id
                     (+Denotes (Simple_Obj_Id (Proc_Obj)),
                      Show_Source_Positions => False);

         when Dereference_Obj_Kind =>
            --  Return identifier associated with pointer
            return Proc_Obj_Identifier
                     (Pointer_Obj_Id (Dereference_Obj_Id (Proc_Obj)));

         when Selected_Component_Obj_Kind =>
            --  Return identifier associated with selector
            return SCIL.Debugging.Ada_Id
                     (+Selector (Selected_Component_Obj_Id (Proc_Obj)),
                      Show_Source_Positions => False);

         when Call_Site_Obj_Kind            |
              Indexed_Component_Obj_Kind    |
              Call_Site_Heap_Slice_Obj_Kind |
              Sliding_Obj_Kind              |
              Unknown_Result_Obj_Kind       |
              Call_Side_Effects_Obj_Kind    =>
            --  Too complicated; just return the empty string
            return "";
      end case;
   end Proc_Obj_Identifier;

   function Source_Info_For_Callee_Obj
     (Callee_Obj   : Obj_Id;
      Proc_Obj     : Obj_Id;
      Calling_Proc : SCIL.Procedure_Body)
      return         SCIL.Source_Info_DPtr
   is
      --  At a call-site, return the source-info associated with
      --  the Callee_Obj, for use in creating a Call_Site_Obj_Id.
      --  Note that we return a COPY of Callee_Obj's source-info,
      --  except when there is a spec param from which we can get
      --  the source.  We believe that the spec param will stay in
      --  memory at least as long as any call site.  We don't believe
      --  that anything from the body will necessarily stay in memory
      --  as long as any call site, so we copy something that comes
      --  from the body.
      --  The "Proc_Obj" if non-null is used to create a less ambiguous
      --  identifier for the call-site obj by combining in the name
      --  of the procedure being called.

      Source : SCIL.Source_Info_DPtr;
      Within : Varying_Pools.Within_Pool (
        Driver.Cur_Obj_Ids_Cumulative_Pool);
      pragma Unreferenced (Within);
   begin
      case Kind (Callee_Obj) is
         when Simple_Obj_Kind =>
            declare
               Obj_Decl        : constant SCIL.Object_Decl :=
                  Denotes (Simple_Obj_Id (Callee_Obj));
               Original_Source : SCIL.Source_Info_DPtr;
               use type SCIL.SCIL_Node; --  for "+"
               Is_Heap_Obj     : Boolean                   := False;
            begin
               case SCIL.Kind (Obj_Decl) is
                  when SCIL.Spec_Param_Decl_Kind =>
                     --  Make local copy of source info from spec param

                     --  TBD: Not obvious what storage pool we are using
                     --  here...
                     --  TBD: Might be dangling references since we are doing
                     --      a "shallow" copy  -- need a "clone" operation?
                     Original_Source :=
                       To_Source_Info_DPtr
                         (new SCIL.Source_Info'Class'(SCIL.Source
                                                        (+Obj_Decl).all));

                  when SCIL.Body_Param_Decl_Kind =>
                     --  Make local copy of source info from spec param,
                     --  to avoid dangling ref.

                     --  TBD: Not obvious what storage pool we are using
                     --  here...
                     --  TBD: Might be dangling references since we are doing
                     --      a "shallow" copy  -- need a "clone" operation?
                     Original_Source :=
                       To_Source_Info_DPtr
                         (new SCIL.Source_Info'Class'(SCIL.Source
                                (+SCIL.Object_Decls.Corresponding_Spec_Param
                                    (SCIL.Down (Obj_Decl))).all));

                  when SCIL.Standalone_Object_Decl_Kind =>
                     --  This is a bit odd.  Perhaps we should complain
                     --  since we have an exported local that is not on the
                     --  heap

                     --  TBD: Not obvious what storage pool we are using
                     --  here...
                     --  TBD: Might be dangling references since we are doing
                     --      a "shallow" copy  -- need a "clone" operation?
                     Original_Source :=
                       To_Source_Info_DPtr
                         (new SCIL.Source_Info'Class'(SCIL.Source
                                                        (+Obj_Decl).all));

                  when SCIL.Heap_Object_Decl_Kind =>
                     --  TBD: Not obvious what storage pool we are using
                     --  here...
                     --  TBD: Might be dangling references since we are doing
                     --      a "shallow" copy  -- need a "clone" operation?
                     Original_Source :=
                       To_Source_Info_DPtr
                         (new SCIL.Source_Info'Class'(SCIL.Source
                                                        (+Obj_Decl).all));
                     Is_Heap_Obj     := True;

                  when SCIL.Component_Decl_Kind =>
                     --  Callee obj should not be a component
                     pragma Assert (False);
                     null;
                     Original_Source := SCIL.Null_Source_Info_DPtr;
               end case;

               if Is_Heap_Obj then
                  --  Don't want to add callee as prefix, but check
                  --  for recursion and add an indicator
                  declare
                     Calling_Proc_Name : constant String :=
                        SCIL.Debugging.Ada_Id
                          (+Calling_Proc,
                           Show_Source_Positions => False);
                     Param_Name        : constant String :=
                        To_String
                          (SCIL.Identifier
                              (To_Source_Info_CPtr (Original_Source).all));
                  begin
                     if Strings.Contains
                          (Param_Name,
                           Substring => Calling_Proc_Name & ':')
                     then
                        --  Add recursion indicator ("*")
                        Source :=
                           Call_Site_Source.New_Source_Info
                             (Original_Source,
                              Identifier => Param_Name & '*');
                     else
                        --  Use original source info as is
                        Source := Original_Source;
                     end if;
                  end;
               elsif Proc_Obj /= No_Obj_Id then
                  --  Add in the proc name
                  declare
                     Proc_Name : String renames Proc_Obj_Identifier (Proc_Obj);
                  begin
                     if Proc_Name /= "" then
                        declare
                           Param_Name : constant String :=
                             To_String
                               (SCIL.Identifier
                                  (To_Source_Info_CPtr (Original_Source).all));
                        begin
                           if Strings.Is_Suffix (Param_Name, "'Result") then
                              Source := Original_Source;
                           elsif Param_Name = "return_value" then
                              --  just add "(...)" to name of procedure
                              --  to represent returned value
                              Source :=
                                 Call_Site_Source.New_Source_Info
                                   (Original_Source,
                                    Identifier => Proc_Name & "(...)");
                           else
                              --  Add proc-name as prefix to param name
                              Source :=
                                 Call_Site_Source.New_Source_Info
                                   (Original_Source,
                                    Identifier => Proc_Name &
                                                  "." &
                                                  Param_Name);
                           end if;
                        end;
                     else
                        --  Use original source info as is
                        Source := Original_Source;
                     end if;
                  end;
               else
                  --  Use original source info as is
                  Source := Original_Source;
               end if;
            end;

         when Call_Site_Obj_Kind =>
            --  TBD: Not obvious what storage pool we are using here...
            --  TBD: Might be dangling references since we are doing
            --      a "shallow" copy  -- need a "clone" operation?
            Source :=
              Call_Site_Source.Copy_Source_Info
                (Call_Site_Obj_Id (Callee_Obj));

         when Sub_Obj_Kind =>
            --  Must be the "base" object.
            --  Use the enclosing whole object's Source, but
            --  with a different identifier/description
            declare
               Enclosing_Source : constant SCIL.Source_Info_DPtr :=
                  Source_Info_For_Callee_Obj
                    (Enclosing_Whole_Obj (Sub_Obj_Id (Callee_Obj)),
                     Proc_Obj     => No_Obj_Id,
                     Calling_Proc => Calling_Proc);
            --  NOTE: We are already inside the cumulative pool,
            --       no need for another "within"
            begin
               Source :=
                  Call_Site_Source.New_Source_Info
                    (Enclosing_Source,
                     Identifier  => "_Base_" &
                                    To_String
                                       (SCIL.Identifier
                                           (To_Source_Info_CPtr
                                               (Enclosing_Source).all)),
                     Description => "Temp used for base of call-site slice");
            end;
      end case;

      return Source;
   end Source_Info_For_Callee_Obj;

   function Get_Writable_Or_Localized_Obj_Id
     (Current_Proc       : SCIL.Procedure_Body;
      Current_Module     : SCIL.Module;
      Original_Obj_Id    : Obj_Id;
      Localize_Others    : Boolean;
      Read_Only          : Boolean := False;
      Shadow_Creation_Ok : Boolean := False)
      return               Obj_Id
   is
      --  Helper function for Get_Writable_Obj_Id and
      --  Get_Localized_Obj_Id.
      use type SCIL.Module; --  for "/="
      use type Level_Type; --  for "/="

      procedure Localize_Expr_Id
        (Array_Obj_Id : Obj_Id; Expr : in out Expr_Ids.Expr_Id);
      --  Get a localized expr-id

      procedure Localize_Expr_Id
        (Array_Obj_Id : Obj_Id; Expr : in out Expr_Ids.Expr_Id)
      is
         use type Expr_Ids.Expr_Id_Kind;
         use type Expr_Ids.Expr_Id;
      begin
         case Expr_Ids.Expr_Kind (Expr) is
            when Expr_Ids.Fetch_Expr =>
               if not Read_Only and then Shadow_Creation_Ok then
                  --  Create "localized" expr_id if necessary
                  Expr :=
                    Expr_Ids.Expr_Id
                      (Get_Writable_Or_Localized_Obj_Id
                        (Current_Proc,
                         Current_Module,
                         Obj_Id (Expr),
                         Localize_Others    => True,
                         Read_Only          => Read_Only,
                         Shadow_Creation_Ok => Shadow_Creation_Ok));
               end if;
            when --  Expr_Ids.Fetch_Expr |
              Expr_Ids.Dynamic_Expr | Expr_Ids.Sliding_Expr =>
               --  We should never be trying to
               --"localize" these
               --  but we do in some cases (e.g.
               --  race-condition checks)
               null;

            when Expr_Ids.Int_Literal_Expr =>
               --  These are fine as is
               null;

            when Expr_Ids.Others_Expr =>
               --  We want to get the "local" others for the new array.
               --  TBD: We are losing all index-set info in this process.
               --  Is that a problem?
               declare
                  New_Expr_Id : Expr_Ids.Expr_Id;
               begin
                  if Read_Only then
                     --  Use "localized" others only if
                     --  it already
                     --  exists
                     New_Expr_Id :=
                        Expr_Ids.Factory.Find_Others_Expr_Id
                          (Current_Proc,
                           Array_Obj_Id);
                  else
                     --  Create "localized" others if
                     --  necessary
                     New_Expr_Id :=
                        Expr_Ids.Factory.Get_Others_Expr_Id
                          (Current_Proc,
                           Array_Obj_Id);
                  end if;
                  if New_Expr_Id /= Expr_Ids.No_Expr_Id then
                     --  We now have a localized "others"
                     if not Read_Only
                       and then New_Expr_Id /= Expr
                     then
                        --  We have a new others-expr. We need to set its index
                        --  set appropriately. It should be a union of its
                        --  current set and that of the imported index set,
                        --  less those covered by literal indices.
                        declare
                           use Expr_Ids.Factory;
                           Orig_Proc              : constant
                             SCIL.Procedure_Body :=
                              Expr_Enclosing_Proc (Expr);
                           Orig_Others_Index_Set  : constant Int_Sets.Int_Set
                              :=
                              Others_Index_Set
                                (Expr, Orig_Proc);
                           Local_Others_Index_Set : Int_Sets.Int_Set :=
                              Int_Sets.Union
                                (Others_Index_Set
                                    (New_Expr_Id,
                                     Current_Proc),
                                 Orig_Others_Index_Set);
                           Local_Set_With_Lits : constant Int_Sets.Int_Set
                             := Local_Others_Index_Set;
                        begin
                           if not Others_Includes_Literal_Indices
                                    (New_Expr_Id)
                           then
                              Local_Others_Index_Set :=
                                 Aliasing.Remove_Literal_Indices
                                   (Array_Obj_Id,
                                    Current_Proc,
                                    Local_Others_Index_Set);
                           end if;
                           if not Int_Sets.Is_Subset (Orig_Others_Index_Set,
                                    Local_Others_Index_Set)
                             and then
                               Driver.Cur_Phase in Driver.Obj_Id_Phase_Enum
                           then
                              --  Oh dear, the new Others doesn't cover
                              --  some of the values that the original one did.
                              --  We need to untrack the literally indexed
                              --  components of the new Others.
                              --  TBD: We could re-use the others aliases
                              --       approach.
                              Tracked_Components.
                                Untrack_Literally_Indexed_Components
                                  (Current_Proc,
                                   Array_Obj_Id,
                                   New_Lit_Index => Expr_Ids.No_Expr_Id);

                              --  Put the literals back in
                              Local_Others_Index_Set := Local_Set_With_Lits;
                           end if;

                           --  Set new index set for local "others" expr
                           Set_Others_Index_Set
                             (New_Expr_Id,
                              Current_Proc,
                              Local_Others_Index_Set);
                        end;
                     end if;
                     Expr := New_Expr_Id;
                     if Debug_Shadow then
                        Put_Line
                          ("Get writable/local obj id getting " &
                           " localized others-expr (Read_Only = " &
                           Boolean'Image (Read_Only) &
                           ") for:");
                        Obj_Ids.Debugging.Dump_Obj_Id (Original_Obj_Id);
                        Put_Line (" new array obj id is:");
                        Obj_Ids.Debugging.Dump_Obj_Id (Array_Obj_Id);
                     end if;
                  else
                     if Debug_Shadow then
                        Put_Line
                          ("Get writable/local obj id unable to" &
                           " get localized others-expr (Read_Only = " &
                           Boolean'Image (Read_Only) &
                           ", Localize_Others = " &
                           Boolean'Image (Read_Only) &
                           ") for:");
                        Obj_Ids.Debugging.Dump_Obj_Id (Original_Obj_Id);
                     end if;
                  end if;
               end;
         end case;
      end Localize_Expr_Id;

   begin
      if Original_Obj_Id /= No_Obj_Id
        and then (Localize_Others
                 or else Enclosing_Module (Original_Obj_Id) /=
                         Current_Module)
      then
         declare
            Shadow_Obj_Id : Obj_Id           := No_Obj_Id;
            Modules_Match : constant Boolean :=
               Localize_Others
              and then Enclosing_Module (Original_Obj_Id) = Current_Module;
         begin
            case Kind (Original_Obj_Id) is
               when Whole_Obj_Kind =>
                  --  Note : the only way that we could see a call-site objId
                  --  here
                  --  would be if the Init_Procedure exports a call-site
                  --  objId, and
                  --  we make it look like a global by explicitly setting its
                  --  Level_Num to zero.

                  if Modules_Match then
                     --  Nothing to do, already the right module
                     return Original_Obj_Id;
                  end if;

                  --  We used to only create shadows for "globals" (i.e., not
                  --  formal params) and return the Original_Obj_Id for
                  --"locals".
                  --  but as of 2/13/2012 we always want to return a new
                  --  shadow_obj_id local
                  --  to the current module so we can have a self-contained
                  --  persistent representation of the module.
                  --  TBD : The reason for creating shadows was to make the
                  --  obj-id "writable" so you could add more component
                  --  obj-ids, etc.  If we are doing this to formal
                  --  parameters from other modules, then we will need to
                  --  figure out a way to make them writable as well.

                  --  If the original obj-id is a shadow, and the "real"
                  --  obj-id is in the current module, then return it.
                  if Is_Shadow (Whole_Obj_Id (Original_Obj_Id)) then
                     if Unique_Module_Identifier (Current_Module) =
                        Unique_Module_Identifier
                           (Non_Shadow_Obj_Id
                               (Whole_Obj_Id (Original_Obj_Id)))
                     then
                        return SCIL_Extension.Nth_Obj_Id
                                 (Module        => Current_Module,
                                  Obj_Id_Number =>
                                    Obj_Id_Number_Type (
                          Non_Shadow_Obj_Id (Whole_Obj_Id (Original_Obj_Id)).
                          Which_Obj));
                     end if;
                  end if;

                  if (Read_Only
                     or else Driver.Cur_Phase not in Driver.Obj_Id_Phase_Enum)
                    and then not Shadow_Creation_Ok
                  then
                     --  Don't create a new shadow; just look for one
                     --  that is already there
                     Shadow_Obj_Id :=
                        Obj_Tables.Lookup
                          (Region_Information.Shadow_Obj_Table
                              (Region_Information.Associated_Region_Info
                                  (Current_Module)).Table,
                           Original_Obj_Id.all);

                     if Debug_Shadow
                       and then not (Read_Only
                                    or else Shadow_Obj_Id /= No_Obj_Id)
                     then
                        Put_Line ("Get writable obj id fails for obj:");
                        Obj_Ids.Debugging.Dump_Obj_Id (Original_Obj_Id);
                     end if;
                     --  Make sure it is there if Read_Only not true
                     pragma Assert
                       (Read_Only
                       or else Shadow_Creation_Ok
                       or else Shadow_Obj_Id /= No_Obj_Id);
                  elsif Shadow_Creation_Ok then
                     --  We'll allow shadow creation for level0 const
                     --  Need to create a new shadow
                     declare
                     --  Following circumlocution works around an
                     --  apparent GNAT bug.  See PTR 5716.
                        Shadow_Obj_Temp : Obj_Table_Entry'Class :=
                           Original_Obj_Id.all;
                        Shadow_Obj      : Whole_Obj_Table_Entry'Class renames
                          Whole_Obj_Table_Entry'Class (Shadow_Obj_Temp);
                        Within          : Varying_Pools.Within_Pool
                          (Driver.Cur_Obj_Ids_Cumulative_Pool);
                        pragma Unreferenced (Within);

                        use type SCIL.Source_Info_DPtr;

                     begin
                        --  Reinitialize Sub-Obj table and obj-id number
                        Shadow_Obj.Sub_Obj_Obj_Table := null;
                        Shadow_Obj.Obj_Id_Number     := 0;

                        --  If this is a call_site_obj_id, we need to make a
                        --  local copy of the Source_Info so it will be present
                        --  in the current module (to avoid problematic cross
                        --  references to other modules).

                        if Kind (Original_Obj_Id) = Call_Site_Obj_Kind then
                           Call_Site_Obj_Table_Entry (Shadow_Obj).Source_Info
                             := Call_Site_Source.Copy_Source_Info
                                  (Call_Site_Obj_Id (Original_Obj_Id));
                        end if;

                        --  Indicate that it is a shadow
                        Shadow_Obj.Is_Shadow := True;
                        -------
                        Shadow_Obj.Non_Shadow_Obj_Id :=
                          Whole_Obj_Id (Original_Obj_Id).Non_Shadow_Obj_Id;
                        --? is this correct?
                        -------

                        --  Enter into shadow obj table if not already there
                        Obj_Tables.Include
                          (Region_Information.Shadow_Obj_Table
                              (Region_Information.Associated_Region_Info
                                  (Current_Module)).Table,
                           Shadow_Obj,
                           Shadow_Obj_Id);
                     end;
                  end if;

               when Sub_Obj_Kind =>
                  --  Make a copy of original,
                  --  but with writable/localized enclosing obj
                  declare
                     Shadow_Component      : Sub_Obj_Table_Entry'Class :=
                        Sub_Obj_Id (Original_Obj_Id).all;
                     Orig_Enclosing_Obj_Id : constant Obj_Id           :=
                       Sub_Obj_Id (Original_Obj_Id).Enclosing_Obj_Id;
                  begin
                     Shadow_Component.Obj_Id_Number    := 0;
                     Shadow_Component.Enclosing_Obj_Id :=
                        Get_Writable_Or_Localized_Obj_Id
                          (Current_Proc,
                           Current_Module,
                           Orig_Enclosing_Obj_Id,
                           Localize_Others    => Localize_Others,
                           Read_Only          => Read_Only,
                           Shadow_Creation_Ok => Shadow_Creation_Ok);

                     if Shadow_Component.Enclosing_Obj_Id = No_Obj_Id
                       or else (Shadow_Component.Enclosing_Obj_Id =
                                Orig_Enclosing_Obj_Id
                               and then Enclosing_Module
                                           (Orig_Enclosing_Obj_Id) /=
                                        Current_Module)
                     then
                        --  We don't have a writable enclosing obj-id,
                        --  so give up now.
                        return Original_Obj_Id;
                     end if;

                     --  Get writable/localized obj-ids for other nested
                     --  obj-ids.
                     case Sub_Obj_Kind'(Kind (Original_Obj_Id)) is
                        when Selected_Component_Obj_Kind =>
                           --  Nothing special to do
                           null;

                        when Dereference_Obj_Kind =>
                           --  Get a writable/localized Pointer_Obj_Id
                           declare
                              New_Dereference_Obj :
                                Dereference_Obj_Table_Entry'Class renames
                                Dereference_Obj_Table_Entry'Class (
                                Shadow_Component);
                           begin
                              New_Dereference_Obj.Pointer_Obj_Id :=
                                 Get_Writable_Or_Localized_Obj_Id
                                   (Current_Proc,
                                    Current_Module,
                                    Dereference_Obj_Id (Original_Obj_Id).
                                Pointer_Obj_Id,
                                    Localize_Others    => Localize_Others,
                                    Read_Only          => Read_Only,
                                    Shadow_Creation_Ok => Shadow_Creation_Ok);
                              if New_Dereference_Obj.Pointer_Obj_Id =
                                 null
                              then
                                 New_Dereference_Obj.Pointer_Obj_Id :=
                                   Dereference_Obj_Id (Original_Obj_Id).
                                   Pointer_Obj_Id;
                              end if;
                           end;

                        when Indexed_Component_Obj_Kind =>
                           declare
                              Indexed_Obj :
                                Indexed_Component_Obj_Table_Entry'Class renames
                                  Indexed_Component_Obj_Table_Entry'Class
                                    (Shadow_Component);
                           begin
                              Localize_Expr_Id
                                (Indexed_Obj.Enclosing_Obj_Id,
                                 Indexed_Obj.Index_Expr_Id);
                           end;

                        when Call_Site_Heap_Slice_Obj_Kind =>
                           --  Get a writable/localized version of the "base"
                           declare
                              New_Call_Site_Heap_Slice_Obj :
                                Call_Site_Heap_Slice_Obj_Table_Entry'Class
                                 renames Call_Site_Heap_Slice_Obj_Table_Entry'
                                Class (Shadow_Component);
                              New_Base                     : constant Obj_Id
                                 :=
                                 Get_Writable_Or_Localized_Obj_Id
                                   (Current_Proc,
                                    Current_Module,
                                    Obj_Id (New_Call_Site_Heap_Slice_Obj.Base),
                                    Localize_Others    => Localize_Others,
                                    Read_Only          => Read_Only,
                                    Shadow_Creation_Ok => Shadow_Creation_Ok);
                           begin
                              if New_Base /= No_Obj_Id then
                                 New_Call_Site_Heap_Slice_Obj.Base :=
                                   Call_Site_Obj_Id (New_Base);
                              end if;
                           end;

                        when Sliding_Obj_Kind =>
                           --  Get a writable/localized Sliding_Obj
                           declare
                              New_Sliding_Obj :
                                Sliding_Obj_Table_Entry'Class
                                 renames Sliding_Obj_Table_Entry'
                                Class (Shadow_Component);
                           begin
                              --  Localize bound values
                              --  TBD: Is this really necessary?
                              for I in 1 .. New_Sliding_Obj.Num_Bounds loop
                                 Localize_Expr_Id
                                   (New_Sliding_Obj.Enclosing_Obj_Id,
                                    New_Sliding_Obj.Old_Low_Bounds (I));
                                 Localize_Expr_Id
                                   (New_Sliding_Obj.Enclosing_Obj_Id,
                                    New_Sliding_Obj.New_Low_Bounds (I));
                              end loop;
                           end;

                        when Unknown_Result_Obj_Kind =>
                           --  Get a writable/localized Associated_Exported_Obj
                           declare
                              New_Unknown_Result_Obj      :
                                Unknown_Result_Obj_Table_Entry'Class renames
                                Unknown_Result_Obj_Table_Entry'Class (
                                Shadow_Component);
                              New_Associated_Exported_Obj : constant Obj_Id :=
                                 Get_Writable_Or_Localized_Obj_Id
                                   (Current_Proc,
                                    Current_Module,
                                    New_Unknown_Result_Obj.
                                Associated_Exported_Obj,
                                    Localize_Others    => Localize_Others,
                                    Read_Only          => Read_Only,
                                    Shadow_Creation_Ok => Shadow_Creation_Ok);
                           begin
                              if New_Associated_Exported_Obj /= No_Obj_Id then
                                 New_Unknown_Result_Obj.
                                   Associated_Exported_Obj :=
                                     New_Associated_Exported_Obj;
                              end if;
                           end;

                        when Call_Side_Effects_Obj_Kind =>
                           --  Get a writable/localized Proc_Obj
                           declare
                              New_Call_Side_Effects_Obj :
                                Call_Side_Effects_Obj_Table_Entry'Class renames
                                Call_Side_Effects_Obj_Table_Entry'Class (
                                Shadow_Component);
                              New_Proc_Obj              : constant Obj_Id :=
                                 Get_Writable_Or_Localized_Obj_Id
                                   (Current_Proc,
                                    Current_Module,
                                    New_Call_Side_Effects_Obj.Proc_Obj,
                                    Localize_Others    => Localize_Others,
                                    Read_Only          => Read_Only,
                                    Shadow_Creation_Ok => Shadow_Creation_Ok);
                           begin
                              if New_Proc_Obj /= No_Obj_Id then
                                 New_Call_Side_Effects_Obj.Proc_Obj :=
                                   New_Proc_Obj;
                              end if;
                           end;
                     end case;

                     if Shadow_Component.Enclosing_Obj_Id /= null then
                        --  If enclosing obj-id found, then look for
                        --  corresponding sub-obj
                        Shadow_Component.Sub_Obj_Obj_Table := null;

                        --  Look up in sub-obj-table of shadow of encloser
                        Get_Sub_Object_Obj_Id
                          (Current_Proc,
                           Shadow_Component.Enclosing_Obj_Id,
                           Shadow_Component,
                           Shadow_Obj_Id,
                           Read_Only          => Read_Only,
                           Shadow_Creation_Ok => Shadow_Creation_Ok);
                     end if;
                  end;
            end case;

            --  NOTE: Might be No_Obj_Id if Read_Only is True
            return Shadow_Obj_Id;
         end;
      else
         return Original_Obj_Id;
      end if;
   end Get_Writable_Or_Localized_Obj_Id;

   ------------- Visible subprograms -------------

   function Get_Writable_Obj_Id
     (Current_Proc       : SCIL.Procedure_Body;
      Current_Module     : SCIL.Module;
      Original_Obj_Id    : Obj_Id;
      Read_Only          : Boolean := False;
      Shadow_Creation_Ok : Boolean := False)
      return               Obj_Id
   is
   --  Return an obj-id that is "writable" meaning that
   --  it is defined in the current module under construction.
   --  This just returns Original_Obj_Id if it is in the
   --  current module.  If not, a shadow obj-id will be
   --  found/created to represent the obj-id (or if the
   --  the original obj-id is a shadow, and the "real" obj-id
   --  is in the current module, that will be returned).
   --  If the given obj-id is not a simple obj-id, and it
   --  is not already from the current module, then
   --  this routine will call itself recursively to retrieve/create
   --  a writable enclosing obj-id and retrieve/create the
   --  corresponding component obj-id.
   --  If Read_Only True, then No_Obj_Id is returned
   --  if shadow for obj-id not already in current module.
   --  If this is a string literal, we hash based on
   --  the sequence of characters of the literal, and will reuse
   --  a preexisting obj-id if it has the same sequence.
   --  NOTE: These might better be called "localized" obj-ids.

   begin
      return Get_Writable_Or_Localized_Obj_Id
               (Current_Proc,
                Current_Module,
                Original_Obj_Id,
                Localize_Others    => False,
                Read_Only          => Read_Only,
                Shadow_Creation_Ok => Shadow_Creation_Ok);
   end Get_Writable_Obj_Id;

   function Get_Localized_Obj_Id
     (Current_Proc       : SCIL.Procedure_Body;
      Current_Module     : SCIL.Module;
      Original_Obj_Id    : Obj_Id;
      Read_Only          : Boolean := False;
      Shadow_Creation_Ok : Boolean := False)
      return               Obj_Id
   is
   --  Return an obj-id that is "localized" meaning that
   --  it is defined in the current module under construction,
   --  and is meaningful in the current procedure.
   --  This passes the buck to Get_Writable_Obj_Id in most cases
   --  except when it has an Indexed_Component_Obj_Id with an "others",
   --  in which case it checks to see whether the the associated
   --  procedure for the "others" is the current one.  If not,
   --  it will get a "localized" others (if possible).
   --  This routine will call itself recursively to get
   --  a localized enclosing obj-id.
   --  If Read_Only True, then No_Obj_Id is returned
   --  if localized equivalent for obj-id not already in current module.
   begin
      return Get_Writable_Or_Localized_Obj_Id
               (Current_Proc,
                Current_Module,
                Original_Obj_Id,
                Localize_Others    => True,
                Read_Only          => Read_Only,
                Shadow_Creation_Ok => Shadow_Creation_Ok);
   end Get_Localized_Obj_Id;

   procedure Init_New_Obj_Id
     (Obj              : Obj_Id;
      Enclosing_Module : SCIL.Module)
   is
      --  Initialize the obj-id number and module for the given obj.
      Within : Varying_Pools.Within_Pool (
        Driver.Cur_Obj_Ids_Cumulative_Pool);
      pragma Unreferenced (Within);
   begin
      --  Just pass the buck to the scil-extension routine
      --  to assign obj-id number and fill in enclosing-module field.
      SCIL_Extension.Update.Assign_Obj_Id_Number (Enclosing_Module, Obj);
   end Init_New_Obj_Id;

   procedure Track_Obj_Id_And_Enclosers
     (Current_Proc   : SCIL.Procedure_Body;
      Obj            : Obj_Id;
      Is_Referenced  : Boolean := False)
   is
   --  If obj-id is elementary or Is_Referenced is True,
   --  add it to set of referened obj-ids.
   --  If is a component, track encloser and add this obj to
   --  list of tracked components for enclosing object.
   begin
      if Obj.Kind in Sub_Obj_Kind then
         --  Make sure enclosers are tracked components of *their* encloser
         Track_Obj_Id_And_Enclosers
           (Current_Proc,
            Sub_Obj_Id (Obj).Enclosing_Obj_Id);
      end if;
      --  Add to tracked components/refed-obj-id list
      Track_New_Obj_Id (Current_Proc, Obj, Is_Referenced => Is_Referenced);
   end Track_Obj_Id_And_Enclosers;

   procedure Track_Remaining_Refed_Objs
     (Current_Proc : SCIL.Procedure_Body)
   is
      --  Call Track_Obj_Id_And_Enclosers on all referenced obj-ids
      --  that have not been tracked in the current obj-id pass.
      use Region_Information;
      Proc_Info : constant Procedure_Body_Info_Ptr :=
         Associated_Region_Info (Current_Proc);

      Already_Tracked : Obj_Id_Sets.Obj_Id_Set renames
        Obj_Ids_Already_Tracked (Proc_Info).all;
      --  Set of refed objs already tracked

      procedure Track_One (Obj : Obj_Id) is
      --  track one referenced obj-id if not already tracked,
      --  and track its enclosers
      begin
         if not Obj_Id_Sets.Is_Member (Obj, Already_Tracked) then
            Track_Obj_Id_And_Enclosers
              (Current_Proc,
               Obj,
               Is_Referenced => True);
         end if;
      end Track_One;
      procedure Track_Remaining is new Obj_Id_Sets.Iterate (Track_One);
   begin
      --  Track the remaining obj-ids
      Track_Remaining (Proc_Referenced_Obj_Ids (Proc_Info).all);
   end Track_Remaining_Refed_Objs;

   procedure Init_Object_Decl_Obj_Id
     (Denotes        : SCIL.Object_Decl;
      Current_Module : SCIL.Module)
   is
      --  Create a (simple) obj-id for the given object decl.
      --  Requires: The object_decl is from the current module
      --  and does *not* yet have an obj-id created for it.
      --  Also enter obj-id into the "Shadow_Obj_Table" if it
      --  is a string literal, as string literals with the same
      --  sequence of characters, even if in different modules
      --  are supposed to be the "same" object (at least in Java).

      use type SCIL.Module; --  for "="
      use type SCIL.Node_Kind; --  for "/="
      pragma Assert
        (SCIL.Decl_Regions.Containing_Module
            (SCIL.Object_Decls.Containing_Region (Denotes)) =
         Current_Module);

      New_Obj_Id : Obj_Id := SCIL_Extension.Object_Decl_Obj_Id (Denotes);
      --  Should normally be No_Obj_Id.

      Module_Info : constant Region_Information.Module_Info_Ptr :=
         Region_Information.Associated_Region_Info (Current_Module);

      Within : Varying_Pools.Within_Pool (
        Driver.Cur_Obj_Ids_Cumulative_Pool);
      pragma Unreferenced (Within);
      use type SCIL.SCIL_Node; --  for "+"
      use type SCIL.Object_Decl;

      Is_External_Obj : constant Boolean :=
         SCIL.Kind (Denotes) = SCIL.Standalone_Object_Decl_Kind
        and then SCIL.Object_Decls.Is_Externally_Defined
                    (SCIL.To_Standalone_Object_Decl (Denotes));
   begin
      if Is_External_Obj
        and then Externs_Mapping.Fully_Declared_Object (Denotes) /= Denotes
      then
         --  the real obj_id will be created on the actual declaration,
         --  we should not use this object_decl
         return;
      end if;

      if New_Obj_Id /= No_Obj_Id then
         --  We must be reprocessing the obj-decl because
         --  when it was originally processed the enclosing module
         --  was being treated as an imported module.  Now presumably
         --  it is *not* marked as imported.
         pragma Assert
           (not Region_Information.Module_Is_Imported (Module_Info));

         if not Source_Information.Is_String_Literal (Denotes) then
            --  Recompute Is_Imported flag

            --  This is no longer true, it could have been set while reading
            --  a persistent module.
            --  pragma Assert (Simple_Obj_Id (New_Obj_Id).Is_Imported);

            Simple_Obj_Id (New_Obj_Id).Is_Imported :=
               Source_Information.Is_Imported (SCIL.Source (+Denotes));
         end if;

         return;       ----- All done now
      end if;

      if Is_External_Obj
        or else Source_Information.Is_String_Literal (Denotes)
      then
         --  External obj or String literal, so enter it into Shadow_Obj_Table
         --  which will hash based on chars of global unique id or
         --  string literal to make sure it is unique.
         declare
            New_Obj : Simple_Obj_Table_Entry :=
               Simple_Obj_Table_Entry'
              (Obj_Table_Entry with
               Level             => SCIL.Object_Decls.Object_Level (Denotes),
               Denotes           => Denotes,
               Is_Shadow         => False,
               Is_String_Literal => Source_Information.Is_String_Literal
                                      (Denotes),
               Is_Imported       => False,
               Non_Shadow_Obj_Id => No_Non_Shadow);
         begin
            --  Initialize the other relevant fields
            New_Obj.Kind := Simple_Obj_Kind;
            Compute_Hash (New_Obj);

            --  Add to shadow-obj/string-lit table which will
            --  assign an obj-id (by calling Init_New_Obj_Id) if it
            --  represents a new string literal.
            Obj_Tables.Include
              (Region_Information.Shadow_Obj_Table (Module_Info).Table,
               New_Obj,
               New_Obj_Id);
         end;
      else
         --  Not a string literal; it always gets its own unique obj-id
         --  so no need to put it immediately into the "shadow" table.
         New_Obj_Id :=
           new Simple_Obj_Table_Entry'
           (Obj_Table_Entry with
         --  Note : this gets reset, below!!
            Level             => Invalid_Level,
            Denotes           => Denotes,
            Is_Shadow         => False,
            Is_String_Literal => False,
            Is_Imported       =>
              Region_Information.Module_Is_Imported (Module_Info)
              or else Source_Information.Is_Imported (SCIL.Source (+Denotes)),
            Non_Shadow_Obj_Id => No_Non_Shadow);
         --  Initialize the other relevant fields
         New_Obj_Id.Kind := Simple_Obj_Kind;

         --  Set the "Level" of the New_Obj_Id.  Because Object_Level
         --  "asserts" when handed a Spec_Param_Decl or Component_Decl,
         --  we could not call Object_Level above, at New_Obj_Id's
         --  declaration.
         if SCIL.Kind (Denotes) /= SCIL.Spec_Param_Decl_Kind
           and then SCIL.Kind (Denotes) /= SCIL.Component_Decl_Kind
         then
            Simple_Obj_Id (New_Obj_Id).Level :=
               SCIL.Object_Decls.Object_Level (Denotes);
         end if;
         Compute_Hash (New_Obj_Id.all);
         Factory.Init_New_Obj_Id (New_Obj_Id, Current_Module);
      end if;

      if Simple_Obj_Id (New_Obj_Id).Non_Shadow_Obj_Id.Which_Obj =
         No_Unique_Obj_Number
      then
         --  This must be a new object, so fill in the
         --  non-shadow-obj-id pair.
         Simple_Obj_Id (New_Obj_Id).Non_Shadow_Obj_Id :=
           (Which_Module => Unique_Module_Identifier (M => Current_Module),
            Which_Obj    => Unique_Obj_Number_Type (New_Obj_Id.Obj_Id_Number));
      end if;

      --  Set it as the Obj_Id extension of object_decl
      SCIL_Extension.Update.Set_Object_Decl_Obj_Id (Denotes, New_Obj_Id);
   end Init_Object_Decl_Obj_Id;

   function Get_Simple_Obj_Id
     (Current_Proc   : SCIL.Procedure_Body;
      Current_Module : SCIL.Module;
      Denotes        : SCIL.Object_Decl)
      return           Simple_Obj_Id
   is
      --  Get an Obj-id for the given object decl.
      --  Requires: The object-decl has an obj-id stored on it
      --  (by Init_Object_Decl_Obj_Id, presumably).

      --  If the object-decl is in the current module,
      --  this just returns the stored obj-id.
      --  If the object-decl is level 0 and from some other module, then
      --  this will find/create a shadow obj-id.
      --  If this is a string literal, we hash based on
      --  the sequence of characters of the literal, and will reuse
      --  a preexisting obj-id if it has the same sequence.

      Result : constant Obj_Id :=
         Get_Writable_Obj_Id
           (Current_Proc,
            Current_Module,
            SCIL_Extension.Object_Decl_Obj_Id (Denotes),
            Shadow_Creation_Ok => True);
      use type SCIL.Procedure_Body;
      use type Region_Information.Procedure_Body_Info_Ptr;
   begin
      --  Add to set of tracked referenced objects
      if Driver.Cur_Phase in Driver.Obj_Id_Phase_Enum
        and then Current_Proc /= SCIL.No_Procedure_Body
        and then Region_Information.Associated_Region_Info (Current_Proc) /=
                 null
      then
         Track_New_Obj_Id (Current_Proc, Result);
      end if;

      return Simple_Obj_Id (Result);
   end Get_Simple_Obj_Id;

   function Find_Simple_Obj_Id
     (Current_Proc   : SCIL.Procedure_Body;
      Current_Module : SCIL.Module;
      Denotes        : SCIL.Object_Decl)
      return           Simple_Obj_Id
   is
      --  Find an Obj-id for the given object decl.
      --  Requires: The object-decl has an obj-id stored on it
      --  (by Set_Object_Decl_Obj_Id, presumably).

      --  If the object-decl is in the current module,
      --  this just returns the stored obj-id.
      --  If the object-decl is level 0 and from some other module, then
      --  this will find a shadow obj-id, if already created.
      --  If this is a string literal, we hash based on
      --  the sequence of characters of the literal, and will reuse
      --  a preexisting obj-id if it has the same sequence.

      --  Returns No_Obj_Id if there is no Obj-Id corresponding
      --  to the given object-decl in the shadow table.
      Result : constant Obj_Id :=
         Get_Writable_Obj_Id
           (Current_Proc,
            Current_Module,
            SCIL_Extension.Object_Decl_Obj_Id (Denotes),
            Read_Only => True);
   begin
      return Simple_Obj_Id (Result);
   end Find_Simple_Obj_Id;

   function Get_Call_Site_Obj_Id
     (Calling_Proc      : SCIL.Procedure_Body;
      Call_Site_Mapping : Obj_Id_Mappings.Obj_Id_Mapping_Ptr;
      Callee_Obj        : Obj_Id;
      Denoted_Decl      : SCIL.Object_Decl;
      Proc_Obj          : Obj_Id := No_Obj_Id)
      return              Call_Site_Obj_Id
   is
      --  Find/create a call site object id which roughly represents the
      --  temporary used at the site of a procedure call to create the
      --  actual/formal correspondence, or which represents an object
      --  allocated within the called procedure, and returned to the caller.
      --  Call_Site_Mapping is used to find obj-id if one already exists.
      --  If new obj-id is created, it is added to the mapping.
      --  The "Proc_Obj" if provided is used to create a less ambiguous
      --  identifier for the call-site obj by combining in the name
      --  of the procedure being called.

      Call_Site_Obj : Obj_Id :=
         Obj_Id_Mappings.Lookup_Obj_Id (Call_Site_Mapping.all, Callee_Obj);

   begin
      if Call_Site_Obj = No_Obj_Id or else Call_Site_Obj = Callee_Obj then
         --  Not already in mapping, or present with
         --  identity mapping, so create call-site obj and
         --  add to/replace in mapping.
         --  NOTE: identity mapping can be created due
         --       to directly recursive call.
         declare
            use type SCIL.Decl_Region; --  for "+"
            use BE.SCIL;
            Calling_Module : constant SCIL.Module :=
               SCIL.Decl_Regions.Containing_Module (+Calling_Proc);
            Level          : constant Level_Type  :=
               SCIL.Decl_Regions.Region_Level (+Calling_Proc);

            New_Obj : Call_Site_Obj_Table_Entry :=
              (Obj_Table_Entry with
               Level                => Level,
               Is_Shadow            => False,
               Is_String_Literal    => False,
               Is_Imported          => False,
               Denotes              => Denoted_Decl,
               Type_Of_Object       => Type_Of_Obj_Id (Callee_Obj),
               Collection_Of_Object => Collection_Of_Obj_Id (Callee_Obj),
               Source_Info          => Null_Source_Info_DPtr,
               Non_Shadow_Obj_Id    => No_Non_Shadow);

         begin
            New_Obj.Kind        := Call_Site_Obj_Kind;
            New_Obj.Source_Info :=
               Source_Info_For_Callee_Obj
                 (Callee_Obj,
                  Proc_Obj     => Proc_Obj,
                  Calling_Proc => Calling_Proc);

            if Kind (Callee_Obj) in Sub_Obj_Kind then
               --  Under "normal" circumstances, we should not be creating
               --  a call-site-obj-id for a sub-obj.  We do allow this in
               --  Create_Call_Site_Obj_Id, which is used for the "Base"
               --  and enclosing heap-array of a call-site-heap-slice.
               pragma Assert (False);
               null;
            end if;

            --  Now actually allocate obj-id in heap
            declare
               Within : Varying_Pools.Within_Pool
                 (Driver.Cur_Obj_Ids_Cumulative_Pool);
               pragma Unreferenced (Within);
            begin
               Call_Site_Obj := new Call_Site_Obj_Table_Entry'(New_Obj);
            end;

            --  Finish initialization of obj-id
            --  Calling_module should always be current_module
            Factory.Init_New_Obj_Id (Call_Site_Obj, Calling_Module);
            Call_Site_Obj_Id (Call_Site_Obj).Non_Shadow_Obj_Id :=
              (Which_Module => Unique_Module_Identifier (Calling_Module),
               Which_Obj    =>
              Unique_Obj_Number_Type (Call_Site_Obj.Obj_Id_Number));
            Compute_Hash (Call_Site_Obj.all);

            --  Add to call-site mapping
            declare
               Within : Varying_Pools.Within_Pool (Driver.Cur_Batch_Pool);
               pragma Unreferenced (Within);
            begin
               Obj_Id_Mappings.Insert_Pair
                 (Call_Site_Mapping.all,
                  From => Callee_Obj,
                  To   => Call_Site_Obj);
            end;
            if Debug_Calls then
               Put_Line
                 ("Get_Call_Site_Obj_Id adding to call-site mapping: " &
                  Obj_Ids.Debugging.Obj_Id_Image (Callee_Obj, Calling_Proc));
               Obj_Ids.Debugging.Dump_Obj_Id (Callee_Obj);
               Put_Line
                 ("Get_Call_Site_Obj_Id Call_Site_Obj: " &
                  Obj_Ids.Debugging.Obj_Id_Image
                     (Call_Site_Obj,
                      Calling_Proc));
               Obj_Ids.Debugging.Dump_Obj_Id (Call_Site_Obj);
            end if;

         end;

      end if;

      --  Add to appropriate mappings, sets, etc.
      Track_New_Obj_Id (Calling_Proc, Call_Site_Obj);

      return Call_Site_Obj_Id (Call_Site_Obj);
   end Get_Call_Site_Obj_Id;

   function Create_Call_Site_Obj_Id
     (Calling_Proc : SCIL.Procedure_Body;
      Callee_Obj   : Obj_Id;
      Denoted_Decl : SCIL.Object_Decl;
      Proc_Obj     : Obj_Id := No_Obj_Id)
      return         Call_Site_Obj_Id
   is
      --  Create and return a call-site obj-id to represent "Callee_Obj".
      --  This function differs from Get_Call_Site_Obj_Id, in that it
      --  doesn't check the Call_Site_Mapping to see if a call_site_obj_Id
      --  for Callee_Obj has already been created.  Nor does it enter the
      --  created objId into the Call_Site_Mapping.
      --
      --  This function is used to create the "Base" of a
      --  Call_Site_Heap_Slice_Obj_Id, and also when creating the
      --  slice's enclosing Call_Site_Heap_Array.
      --  The "Proc_Obj" if provided is used to create a less ambiguous
      --  identifier for the call-site obj by combining in the name
      --  of the procedure being called.
      --
      --  Note : even though we don't use the Call_Site_Mapping,
      --  there are other mechanisms in place to make sure we don't
      --  re-create the call-site-obj-ids (on a second pass of objId
      --  creation, for example).  A Call_Site_Heap_Slice_Obj_Id *is*
      --  entered into the Call_Site_Mapping, so if a slice already exists
      --  for a Callee_Heap_Array, then we will not re-create the objId
      --  for the Base.  The Call_Site_Heap_Array is in a different
      --  mapping (from SCIL_heap_array to Call_Site_Heap_Array), and this
      --  will prevent us from creating duplicate Call_Site_Heap_Arrays.

      use type SCIL.Decl_Region; --  for "+"
      use BE.SCIL;
      Calling_Module : constant SCIL.Module      :=
         SCIL.Decl_Regions.Containing_Module (+Calling_Proc);
      Level          : constant Level_Type       :=
         SCIL.Decl_Regions.Region_Level (+Calling_Proc);
      Call_Site_Obj  : Obj_Id;
      New_Obj        : Call_Site_Obj_Table_Entry :=
        (Obj_Table_Entry with
         Level                => Level,
         Denotes              => Denoted_Decl,
         Is_Shadow            => False,
         Is_String_Literal    => False,
         Is_Imported          => False,
         Type_Of_Object       => Type_Of_Obj_Id (Callee_Obj),
         Collection_Of_Object => Collection_Of_Obj_Id (Callee_Obj),
         Source_Info          => Null_Source_Info_DPtr,
         Non_Shadow_Obj_Id    => No_Non_Shadow);

   begin
      New_Obj.Kind        := Call_Site_Obj_Kind;
      New_Obj.Source_Info :=
         Source_Info_For_Callee_Obj
           (Callee_Obj,
            Proc_Obj     => Proc_Obj,
            Calling_Proc => Calling_Proc);

      --  Now actually allocate obj-id in heap
      declare
         Within : Varying_Pools.Within_Pool
           (Driver.Cur_Obj_Ids_Cumulative_Pool);
         pragma Unreferenced (Within);
      begin
         Call_Site_Obj := new Call_Site_Obj_Table_Entry'(New_Obj);
      end;

      --  Finish initialization of obj-id
      Factory.Init_New_Obj_Id (Call_Site_Obj, Calling_Module);
      Call_Site_Obj_Id (Call_Site_Obj).Non_Shadow_Obj_Id :=
        (Which_Module => Unique_Module_Identifier (Calling_Module),
         Which_Obj    => Unique_Obj_Number_Type (Call_Site_Obj.Obj_Id_Number));
      Compute_Hash (Call_Site_Obj.all);

      --  Add to appropriate mappings, sets, etc.
      Track_New_Obj_Id (Calling_Proc, Call_Site_Obj);

      return Call_Site_Obj_Id (Call_Site_Obj);

   end Create_Call_Site_Obj_Id;

   function Get_Selected_Component_Obj_Id
     (Current_Proc  : SCIL.Procedure_Body;
      Record_Obj_Id : Obj_Id;
      Selector      : SCIL.Component_Decl)
      return          Selected_Component_Obj_Id
   is
      --  Find/Create an Obj-Id for the given selected component
      --  Requires: Record_Obj_Id is "writable"
      New_Obj       : Selected_Component_Obj_Table_Entry :=
        (Obj_Table_Entry with
         Enclosing_Obj_Id => Record_Obj_Id,
         Selector         => Selector);
      Result_Obj_Id : Selected_Component_Obj_Id;
   begin
      New_Obj.Kind := Selected_Component_Obj_Kind;

      --  Find/Create obj-id for new object in obj table associated with record
      Get_Sub_Object_Obj_Id
        (Current_Proc,
         Record_Obj_Id,
         New_Obj,
         Obj_Id (Result_Obj_Id));

      --  And return it
      return Result_Obj_Id;
   end Get_Selected_Component_Obj_Id;

   function Get_Indexed_Component_Obj_Id
     (Current_Proc  : SCIL.Procedure_Body;
      Array_Obj_Id  : Obj_Id;
      Index_Expr_Id : Expr_Ids.Expr_Id;
      Read_Only     : Boolean := False)
      return          Indexed_Component_Obj_Id
   is
      --  Find/create an obj-id for the given indexed component
      --  If Read_Only is True, returns No_Obj_Id if not found.
      --  Requires: Array_Obj_Id is "writable" if Read_Only is False.
      --  NOTE: If Index_Expr_Id is a literal expr id, we might nevertheless
      --       return a component indexed by "others" if we have seen too
      --       many literally-indexed components.
      --  NOTE2: If Array_Obj_Id is a Sliding_Obj_Id or a partial indexing of
      --       a multi-dimensional Sliding_Obj_Id, then we might return
      --       a component indexed by a "Sliding_Expr_Id" with the Array_Obj_Id
      --       being the underlying non-sliding array obj-id.

      use type Expr_Ids.Expr_Id_Kind;
      use type Expr_Ids.Expr_Id;

      Others_Expr : Expr_Ids.Expr_Id :=
         Expr_Ids.Factory.Find_Others_Expr_Id (Current_Proc, Array_Obj_Id);

      function Index_Expr_Id_To_Use return  Expr_Ids.Expr_Id is
         --  Return Index_Expr_Id unless it is a literal expr id
         --  and array others includes literal indices.
      begin
         if Expr_Ids.Expr_Kind (Index_Expr_Id) = Expr_Ids.Int_Literal_Expr
           and then Others_Expr /= Expr_Ids.No_Expr_Id
           and then Expr_Ids.Factory.Others_Includes_Literal_Indices
                       (Others_Expr)
         then
            --  Return others expr-id to avoid creating yet another obj-id
            --  for a literally-indexed array element.
            --  First make sure literal value included in Others index set.
            Expr_Ids.Factory.Set_Others_Index_Set
              (Others_Expr,
               Current_Proc,
               Int_Sets.Insert
                  (Expr_Ids.Factory.Others_Index_Set
                      (Others_Expr,
                       Current_Proc),
                   Expr_Ids.Int_Literal_Value (Index_Expr_Id)));
            return Others_Expr;
         else
            return Index_Expr_Id;
         end if;
      end Index_Expr_Id_To_Use;

      function Sliding_Is_Dynamic
        (Sliding_Obj : Sliding_Obj_Id;
         Which_Dim   : SCIL.Expression_Index)
        return Boolean is
         --  Return True if either of the Low_Bounds for the given
         --  dimension of the Sliding are not int_literal expr_ids.
      begin
         return Expr_Ids.Expr_Kind
                  (Sliding_Old_Low_Bounds (Sliding_Obj)(Which_Dim)) /=
                    Expr_Ids.Int_Literal_Expr
           or else
                Expr_Ids.Expr_Kind
                  (Sliding_New_Low_Bounds (Sliding_Obj)(Which_Dim)) /=
                    Expr_Ids.Int_Literal_Expr;
      end Sliding_Is_Dynamic;

      function Slide_Literal_Index
        (Index_Expr_Id : Expr_Ids.Expr_Id;
         Sliding_Obj   : Sliding_Obj_Id;
         Which_Dim     : SCIL.Expression_Index)
        return Expr_Ids.Expr_Id is
         --  Return an Int_Literal_Expr_Id computed as:
         --    Index_Expr_Id - New_Low_Bound + Old_Low_Bound
         --  Requires: Index_Expr_Id, New_Low_Bound, and Old_Low_Bound
         --            are all Int_Literal_Expr_Id's

         pragma Assert (Expr_Ids.Expr_Kind (Index_Expr_Id) =
           Expr_Ids.Int_Literal_Expr);

         Old_Low_Bound : constant Expr_Ids.Expr_Id :=
           Sliding_Old_Low_Bounds (Sliding_Obj)(Which_Dim);
         New_Low_Bound : constant Expr_Ids.Expr_Id :=
           Sliding_New_Low_Bounds (Sliding_Obj)(Which_Dim);

         pragma Assert (Expr_Ids.Expr_Kind (Old_Low_Bound) =
           Expr_Ids.Int_Literal_Expr);
         pragma Assert (Expr_Ids.Expr_Kind (New_Low_Bound) =
           Expr_Ids.Int_Literal_Expr);
         use type Saturating_Big_Ints.Big_Int;
      begin
         return Expr_Ids.Factory.Get_Int_Literal_Expr_Id (Current_Proc,
           Expr_Ids.Int_Literal_Value (Index_Expr_Id) -
             Expr_Ids.Int_Literal_Value (New_Low_Bound) +
             Expr_Ids.Int_Literal_Value (Old_Low_Bound));
      end Slide_Literal_Index;

      New_Obj       : Indexed_Component_Obj_Table_Entry;
      Result_Obj_Id : Indexed_Component_Obj_Id;

      use type SCIL.Expression_Index;
   begin  --  Get_Indexed_Component_Obj_Id

      if Kind (Array_Obj_Id) = Sliding_Obj_Kind then
         --  Adjust index; create sliding expr id if adjustment is dynamic
         --  or this is not the last index of a multi-dim sliding.

         if Expr_Ids.Expr_Kind (Index_Expr_Id) /= Expr_Ids.Int_Literal_Expr
           or else
             Sliding_Obj_Id (Array_Obj_Id).Num_Bounds > 1
           or else
             Sliding_Is_Dynamic (Sliding_Obj_Id (Array_Obj_Id), Which_Dim => 1)
         then
            --  Recurse to create an indexing using a Sliding_Expr_Id
            return Get_Indexed_Component_Obj_Id
              (Current_Proc  => Current_Proc,
               Array_Obj_Id  => Sliding_Array_Obj_Id
                                  (Sliding_Obj_Id (Array_Obj_Id)),
               Index_Expr_Id =>
                 Expr_Ids.Factory.Get_Sliding_Expr_Id
                   (Current_Proc  => Current_Proc,
                    Sliding_Index => Index_Expr_Id,
                    Sliding_Obj   => Sliding_Obj_Id (Array_Obj_Id),
                    Sliding_Dim   => 1),
               Read_Only     => Read_Only);
         else
            --  Sliding and index are both static, so can eliminate
            --  the sliding object completely.
            return Get_Indexed_Component_Obj_Id
              (Current_Proc  => Current_Proc,
               Array_Obj_Id  => Sliding_Array_Obj_Id
                                  (Sliding_Obj_Id (Array_Obj_Id)),
               Index_Expr_Id =>
                 Slide_Literal_Index
                   (Index_Expr_Id, Sliding_Obj_Id (Array_Obj_Id),
                    Which_Dim => 1),
               Read_Only     => Read_Only);
         end if;
      elsif Kind (Array_Obj_Id) = Indexed_Component_Obj_Kind
        and then Expr_Ids.Expr_Kind (Index_Expr_Id) /= Expr_Ids.Sliding_Expr
      then
         --  Check for multi-dimensional sliding
         declare
            Inner_Indexing : constant Indexed_Component_Obj_Id :=
              Indexed_Component_Obj_Id (Array_Obj_Id);
            Inner_Index : constant Expr_Ids.Expr_Id :=
              Obj_Ids.Index_Expr_Id (Inner_Indexing);
            Sliding_Dim : constant SCIL.Expression_Index :=
              Expr_Ids.Factory.Sliding_Expr_Dim (Inner_Index);
         begin
            if Expr_Ids.Expr_Kind (Inner_Index) = Expr_Ids.Sliding_Expr
              and then
                Expr_Ids.Factory.Sliding_Expr_Obj (Inner_Index).Num_Bounds >
                  Sliding_Dim
            then
               --  Multidimensional sliding
               if Expr_Ids.Expr_Kind (Index_Expr_Id) /=
                   Expr_Ids.Int_Literal_Expr
                 or else
                   Expr_Ids.Factory.Sliding_Expr_Obj (Inner_Index).Num_Bounds >
                     Sliding_Dim + 1
                 or else
                   Sliding_Is_Dynamic
                     (Expr_Ids.Factory.Sliding_Expr_Obj (Inner_Index),
                      Which_Dim => Sliding_Dim + 1)
               then
                  --  Recurse to create an indexing using a Sliding_Expr_Id
                  return Get_Indexed_Component_Obj_Id
                    (Current_Proc  => Current_Proc,
                     Array_Obj_Id  => Array_Obj_Id,
                     Index_Expr_Id =>
                       Expr_Ids.Factory.Get_Sliding_Expr_Id
                         (Current_Proc  => Current_Proc,
                          Sliding_Index => Index_Expr_Id,
                          Sliding_Obj   =>
                            Expr_Ids.Factory.Sliding_Expr_Obj (Inner_Index),
                          Sliding_Dim   => Sliding_Dim + 1),
                     Read_Only     => Read_Only);
               else
                  --  Sliding and index are both static, so can eliminate
                  --  the sliding object completely.
                  return Get_Indexed_Component_Obj_Id
                    (Current_Proc  => Current_Proc,
                     Array_Obj_Id  => Array_Obj_Id,
                                          --  TBD: Might be able to
                                          --       simplify if earlier
                                          --       indices are also static
                     Index_Expr_Id =>
                       Slide_Literal_Index
                         (Index_Expr_Id,
                          Expr_Ids.Factory.Sliding_Expr_Obj (Inner_Index),
                          Which_Dim => Sliding_Dim + 1),
                     Read_Only     => Read_Only);
               end if;
            end if;
         end;
      end if;

      --  Does not involve sliding, so just create the Indexing Obj_Id
      --  using the Index provided.

      New_Obj :=
        (Obj_Table_Entry with
         Enclosing_Obj_Id => Array_Obj_Id,
         Index_Expr_Id    => Index_Expr_Id_To_Use);

      New_Obj.Kind := Indexed_Component_Obj_Kind;

      --  Find/Create obj-id for new object in obj table associated with array
      Get_Sub_Object_Obj_Id
        (Current_Proc,
         Array_Obj_Id,
         New_Obj,
         Obj_Id (Result_Obj_Id),
         Read_Only => Read_Only);

      --  Find/Create "others" element if this is dynamically indexed
      case Expr_Ids.Expr_Kind (Index_Expr_Id) is
         when Expr_Ids.Int_Literal_Expr | Expr_Ids.Others_Expr =>
            --  These are separately tracked
            null;
         when Expr_Ids.Fetch_Expr | Expr_Ids.Dynamic_Expr |
           Expr_Ids.Sliding_Expr =>
            --  These are not tracked; create an array "others" element
            --  which will be tracked instead.
            if Others_Expr = Expr_Ids.No_Expr_Id then
               --  Get an "others" expr-id now.
               Others_Expr :=
                  Expr_Ids.Factory.Get_Others_Expr_Id
                    (Current_Proc,
                     Array_Obj_Id);
            end if;
            if Expr_Ids.Expr_Kind (Index_Expr_Id) not in
                 Expr_Ids.Sliding_Expr
              or else
                Expr_Ids.Expr_Kind
                  (Expr_Ids.Factory.Sliding_Expr_Index (Index_Expr_Id))
                     not in Expr_Ids.Others_Expr
                     --  Avoid recursion if already have a sliding of an others
            then
               declare
                  Array_Others : constant Indexed_Component_Obj_Id :=
                     Get_Indexed_Component_Obj_Id
                       (Current_Proc,
                        Array_Obj_Id,
                        Others_Expr,
                        Read_Only => Read_Only);
                  pragma Unreferenced (Array_Others);
               begin
                  --  Creating others is all we need to do at this point
                  --  It will almost certainly end up aliased with the
                  --  dynamically indexed components.
                  null;
               end;
            end if;
      end case;
      --  And return it
      return Result_Obj_Id;
   end Get_Indexed_Component_Obj_Id;

   function Get_Call_Site_Heap_Slice_Obj_Id
     (Current_Proc      : SCIL.Procedure_Body;
      Call_Site_Mapping : Obj_Id_Mappings.Obj_Id_Mapping_Ptr;
      Callee_Obj        : Obj_Id;
      Heap_Array_Obj_Id : Obj_Id;
      Callee_Obj_Len    : Obj_Id)
      return              Call_Site_Heap_Slice_Obj_Id
   is
      --  Find/Create an Obj-Id for the call-site_heap_slice that
      --  is based on the callee-heap-array "Callee_Obj", with
      --  the enclosing call_site_heap_array "Heap_Array_Obj_Id".

      --  Call_Site_Mapping is used to find obj-id if one already exists.
      --  If a new obj-id is created, we also create an objId for the slice's
      --  "Base" index, which is "based upon" Callee_Obj_Len.
      --  We add the new obj-id to the Call_Site_Mapping.

      Call_Site_Heap_Slice_Obj : Obj_Id :=
         Obj_Id_Mappings.Lookup_Obj_Id (Call_Site_Mapping.all, Callee_Obj);
   begin

      if Call_Site_Heap_Slice_Obj = No_Obj_Id then
         --  not already in mapping, so create it.
         declare
            New_Obj : Call_Site_Heap_Slice_Obj_Table_Entry :=
              (Obj_Table_Entry with
               Enclosing_Obj_Id => Heap_Array_Obj_Id,
            --  this will be filled in for real, below
               Base             => Call_Site_Obj_Id (No_Obj_Id));
         begin
            New_Obj.Base :=
               Create_Call_Site_Obj_Id
                 (Calling_Proc => Current_Proc,
                  Callee_Obj   => Callee_Obj_Len,
                  Denoted_Decl => SCIL.No_Object_Decl);

            New_Obj.Kind := Call_Site_Heap_Slice_Obj_Kind;

            --  Find/Create obj-id for new object in obj table
            --  associated with heap-array
            Get_Sub_Object_Obj_Id
              (Current_Proc,
               Heap_Array_Obj_Id,
               New_Obj,
               Call_Site_Heap_Slice_Obj);

            --  Add to call-site mapping
            declare
               Within : Varying_Pools.Within_Pool (Driver.Cur_Batch_Pool);
               pragma Unreferenced (Within);
            begin
               Obj_Id_Mappings.Insert_Pair
                 (Call_Site_Mapping.all,
                  From => Callee_Obj,
                  To   => Call_Site_Heap_Slice_Obj);
            end;
            if Debug_Calls then
               Put_Line
                 ("Get_Call_Site_Heap_Slice Obj_Id adding to call-site " &
                  "mapping: " &
                  Obj_Ids.Debugging.Obj_Id_Image (Callee_Obj, Current_Proc));
               Obj_Ids.Debugging.Dump_Obj_Id (Callee_Obj);
               Put_Line
                 ("Get_Call_Site_Heap_Slice_Obj_Id Call_Site_Obj: " &
                  Obj_Ids.Debugging.Obj_Id_Image
                     (Call_Site_Heap_Slice_Obj,
                      Current_Proc));
               Obj_Ids.Debugging.Dump_Obj_Id (Call_Site_Heap_Slice_Obj);
            end if;
         end;

      else

         --  Add obj and obj base to tracked-component list, etc.
         Track_New_Obj_Id (Current_Proc, Call_Site_Heap_Slice_Obj);
         Track_New_Obj_Id
           (Current_Proc,
            Obj_Id (
           Call_Site_Heap_Slice_Obj_Id (Call_Site_Heap_Slice_Obj).Base));

      end if;

      --  And return it
      return Call_Site_Heap_Slice_Obj_Id (Call_Site_Heap_Slice_Obj);
   end Get_Call_Site_Heap_Slice_Obj_Id;

   function Get_Sliding_Obj_Id
     (Current_Proc         : SCIL.Procedure_Body;
      Sliding_Array_Obj_Id : Obj_Id;
      Old_Low_Bounds       : Expr_Ids.Expr_Id_Array;
      New_Low_Bounds       : Expr_Ids.Expr_Id_Array;
      Read_Only            : Boolean := False)
      return                 Sliding_Obj_Id is
   --  Find/Create a Sliding_Obj_Id given the original array Obj-Id,
   --  and the old and new low bounds for the array and the sliding.
   --  If Read_Only is True, the return No_Obj_Id if the Sliding doesn't
   --  already exist.
      New_Obj       : Sliding_Obj_Table_Entry :=
        (Obj_Table_Entry with
         Num_Bounds       => Old_Low_Bounds'Length,
         Enclosing_Obj_Id => Sliding_Array_Obj_Id,
         Old_Low_Bounds   => Old_Low_Bounds,
         New_Low_Bounds   => New_Low_Bounds);
      Result_Obj_Id : Sliding_Obj_Id;
   begin
      New_Obj.Kind := Sliding_Obj_Kind;

      --  Find/Create obj-id for new object in obj table associated with array
      Get_Sub_Object_Obj_Id
        (Current_Proc,
         Sliding_Array_Obj_Id,
         New_Obj,
         Obj_Id (Result_Obj_Id),
         Read_Only => Read_Only);

      --  And return it
      return Result_Obj_Id;

   end Get_Sliding_Obj_Id;

   function Find_Selected_Component_Obj_Id
     (Current_Proc  : SCIL.Procedure_Body;
      Record_Obj_Id : Obj_Id;
      Selector      : SCIL.Component_Decl)
      return          Selected_Component_Obj_Id
   is
      --  Find an Obj-Id for the given selected component
      --  Requires: Record_Obj_Id is "writable"
      New_Obj       : Selected_Component_Obj_Table_Entry :=
        (Obj_Table_Entry with
         Enclosing_Obj_Id => Record_Obj_Id,
         Selector         => Selector);
      Result_Obj_Id : Selected_Component_Obj_Id;
   begin
      New_Obj.Kind := Selected_Component_Obj_Kind;

      --  Find obj-id for new object in obj table associated with record
      Get_Sub_Object_Obj_Id
        (Current_Proc,
         Record_Obj_Id,
         New_Obj,
         Obj_Id (Result_Obj_Id),
         Read_Only => True);

      --  And return it
      return Result_Obj_Id;
   end Find_Selected_Component_Obj_Id;

   function Find_Indexed_Component_Obj_Id
     (Current_Proc  : SCIL.Procedure_Body;
      Array_Obj_Id  : Obj_Id;
      Index_Expr_Id : Expr_Ids.Expr_Id)
      return          Indexed_Component_Obj_Id
   is
      --  Find an Obj-Id for the given Indexed component
      --  returns No_Obj_Id if not found.

   begin
      return Get_Indexed_Component_Obj_Id
        (Current_Proc, Array_Obj_Id, Index_Expr_Id, Read_Only => True);
   end Find_Indexed_Component_Obj_Id;

   function Find_Dereference_Obj_Id
     (Current_Proc        : SCIL.Procedure_Body;
      Pointer_Obj_Id      : Obj_Id;
      Collection_Obj_Id   : Obj_Id;
      Find_Wildcard_Deref : Boolean        := False;
      Designated_Type     : SCIL.SCIL_Type := SCIL.No_SCIL_Type)
      return                Dereference_Obj_Id
   is
      --  Find an obj-id for the given dereference
      --  returns No_Obj_Id if not found.
      --  If Find_Wildcard_Deref is True and Designated_Type is non-null, then
      --  result should be a deref for which Includes_All_Reachable() returns
      --  true.  In this case, this is equivalent to a call on
      --  Find_Wildcard_Deref_Obj_Id with Pointer_Base_Obj_Id = Pointer_Obj_Id.
      --  If Find_Wildcard_Deref is false, then Designated_Type is
      --  ignored (it should be the same as the designated type of pointer).
      --  If Find_Wildcard_Deref is True, but Designated_Type is null, then
      --  we look for a "simple" deref, but if one can't be found, then we
      --  return a "wildcard" deref using the encloser of the simple deref
      --  as the Pointer "base" object.

      Matching_Prefix_Count : Natural := 0;

      --  Determine whether some "prefix" of the pointer-obj's name
      --  is a deref of the same type and collection as this one
      Matching_Prefix : constant Obj_Id :=
         Obj_Id (Find_Matching_Deref_Prefix
                    (Current_Proc    => Current_Proc,
                     Pointer_Obj     => Pointer_Obj_Id,
                     Collection_Obj  => Collection_Obj_Id,
                     Designated_Type => Designated_Type));

      use type SCIL.SCIL_Type; --  for "/="
   begin
      if Matching_Prefix /= No_Obj_Id then
         if Includes_All_Reachable
              (Dereference_Obj_Id (Matching_Prefix))
         then
            --  Reuse matching prefix if already a wildcard
            return Dereference_Obj_Id (Matching_Prefix);
         end if;

         --  New deref has one more matching prefix
         Matching_Prefix_Count :=
           Simple_Deref_Obj_Id (Matching_Prefix).Matching_Prefix_Count + 1;
         if Matching_Prefix_Count > Max_Prefix_Count then
            --  We should return a wildcard, but
            --  "Find_Matching_Deref_Prefix" already looked for one,
            --  so no need to look again
            return Dereference_Obj_Id (No_Obj_Id);
         end if;
      end if;

      if Collection_Obj_Id = No_Obj_Id then
         --  no collection, so can't be any deref
         return Dereference_Obj_Id (No_Obj_Id);
      end if;

      if not Find_Wildcard_Deref
        or else Designated_Type = SCIL.No_SCIL_Type
      then
         --  Try to find a simple (non-wildcard) deref
         declare
            New_Obj       : Simple_Deref_Obj_Table_Entry :=
              (Obj_Table_Entry with
               Enclosing_Obj_Id      => Collection_Obj_Id,
               Pointer_Obj_Id        => Pointer_Obj_Id,
               Matching_Prefix_Count => Matching_Prefix_Count);
            Result_Obj_Id : Dereference_Obj_Id;

         begin

            New_Obj.Kind := Simple_Deref_Obj_Kind;

            --  Find obj-id for object in
            --  obj table associated with collection
            Get_Sub_Object_Obj_Id
              (Current_Proc,
               Collection_Obj_Id,
               New_Obj,
               Obj_Id (Result_Obj_Id),
               Read_Only => True);

            --  Return preexisting deref or no_obj_id
            if Result_Obj_Id /= Dereference_Obj_Id (No_Obj_Id)
              or else not Find_Wildcard_Deref
            then
               return Result_Obj_Id;
            end if;
         end;
      end if;

      if Find_Wildcard_Deref then
         --  Caller expects result to be a wildcard, but there was
         --  no matching prefix.  This implies must be looking for
         --  wildcard directly, unless designated type is null.
         --  If designated type is null, then we want a wildcard deref
         --  only if we can't find a regular one.
         declare
            function Pointer_Base_To_Use return Obj_Id is
            --  Return Pointer_Obj_Id if Designated_Type non-null,
            --  else return Enclosing_Whole_Obj(Pointer_Obj_Id).
            --  This is because in a wildcard, "Pointer_Base_Obj_Id"
            --  identifies a composite object containing one or more
            --  pointers, not the pointer obj itself.
            begin
               if Designated_Type /= SCIL.No_SCIL_Type then
                  return Pointer_Obj_Id;
               else
                  return Enclosing_Whole_Obj (Pointer_Obj_Id);
               end if;
            end Pointer_Base_To_Use;

         begin
            return Dereference_Obj_Id (Find_Wildcard_Deref_Obj_Id
                                          (Current_Proc        =>
                                             Current_Proc,
                                           Pointer_Base_Obj_Id =>
                                              Pointer_Base_To_Use,
                                           Designated_Type     =>
                                              Desig_Type_To_Use
                                                (Pointer_Obj_Id,
                                                 Designated_Type),
                                           Collection_Obj_Id   =>
              Collection_Obj_Id));
         end;
      end if;

      return Dereference_Obj_Id (No_Obj_Id);

   end Find_Dereference_Obj_Id;

   function Get_Dereference_Obj_Id
     (Current_Proc       : SCIL.Procedure_Body;
      Pointer_Obj_Id     : Obj_Id;
      Collection_Obj_Id  : Obj_Id;
      Get_Wildcard_Deref : Boolean        := False;
      Designated_Type    : SCIL.SCIL_Type := SCIL.No_SCIL_Type)
      return               Dereference_Obj_Id
   is
      --  Find/Create an obj-id for the given dereference
      --  If Get_Wildcard_Deref is True and Designated_Type is non-null, then
      --  result should be a deref for which Includes_All_Reachable() returns
      --  true.  In this case, this is equivalent to a call on
      --  Get_Wildcard_Deref_Obj_Id with Pointer_Base_Obj_Id = Pointer_Obj_Id.
      --  If Get_Wildcard_Deref is false, then Designated_Type is
      --  ignored (it should be the same as the designated type of pointer).
      --  If Get_Wildcard_Deref is True, but Designated_Type is null, then
      --  we create a simple deref unless the "limit_derefs" flag is specified,
      --  in which case we might create a wildcard deref instead if we have
      --  too many nested derefs.
      --  Requires: Collection_Obj_Id is "writable"

      Desig_Type : constant SCIL.SCIL_Type :=
         Desig_Type_To_Use (Pointer_Obj_Id, Designated_Type);

      Matching_Prefix_Count : Natural := 0;

      --  Determine whether some "prefix" of the pointer-obj's name
      --  is a deref of the same type and collection as this one
      Matching_Prefix : Obj_Id :=
         Obj_Id (Find_Matching_Deref_Prefix
                    (Current_Proc    => Current_Proc,
                     Pointer_Obj     => Pointer_Obj_Id,
                     Collection_Obj  => Collection_Obj_Id,
                     Designated_Type => Desig_Type));

      Create_Wildcard : Boolean := Get_Wildcard_Deref;

      use type SCIL.SCIL_Type; --  for "/="
   begin
      if Matching_Prefix /= No_Obj_Id then
         if Includes_All_Reachable
              (Dereference_Obj_Id (Matching_Prefix))
         then
            --  Reuse matching prefix if already a wildcard
            return Dereference_Obj_Id (Matching_Prefix);
         end if;

         --  New deref has one more matching prefix
         Matching_Prefix_Count :=
           Simple_Deref_Obj_Id (Matching_Prefix).Matching_Prefix_Count + 1;
      elsif Get_Wildcard_Deref then
         --  Caller expects result to be a wildcard, but there was
         --  no matching prefix.  This implies must be creating
         --  wildcard directly, and so designated type should be provided
         if Designated_Type = SCIL.No_SCIL_Type then
            --  See if there is a pre-existing simple deref
            declare
               Result : constant Dereference_Obj_Id :=
                  Find_Dereference_Obj_Id
                    (Current_Proc      => Current_Proc,
                     Pointer_Obj_Id    => Pointer_Obj_Id,
                     Collection_Obj_Id => Collection_Obj_Id);
            begin
               if Result /= Dereference_Obj_Id (No_Obj_Id) then
                  --  Found a preexisting simple deref -- use that
                  if Driver.Cur_Phase in Driver.Obj_Id_Phase_Enum then
                     --  But first, make sure to track it
                     Track_New_Obj_Id (Current_Proc, Obj_Id (Result));
                  end if;
                  return Result;
               end if;

               --  See if this is a deref in a deref
               Matching_Prefix := Enclosing_Whole_Obj (Pointer_Obj_Id);
               if not Test_Limit_Of_Dereferences
                 or else SCIL.Kind (Desig_Type) in SCIL.Elementary_Type_Kind
                 or else Kind (Matching_Prefix) not in Dereference_Obj_Kind
                 or else
                   Kind
                     (Enclosing_Whole_Obj
                       (Obj_Ids.Pointer_Obj_Id
                         (Dereference_Obj_Id (Matching_Prefix))))
                     not in Dereference_Obj_Kind
               then
                  --  This is too simple to force a wildcard
                  Create_Wildcard := False;
                  Matching_Prefix := No_Obj_Id;
               end if;
            end;
         else
            --  Caller specified a Designated_Type
            --  Presume ptr obj is the base
            Matching_Prefix := Pointer_Obj_Id;
         end if;
      end if;

      if Create_Wildcard
        or else Matching_Prefix_Count > Max_Prefix_Count
      then
         --  Need to create a wildcard (we already looked for an existing one)
         return Dereference_Obj_Id (Get_Wildcard_Deref_Obj_Id
                                       (Current_Proc        => Current_Proc,
                                        Pointer_Base_Obj_Id =>
                                          Matching_Prefix,
                                        Designated_Type     => Desig_Type,
                                        Collection_Obj_Id   =>
           Collection_Obj_Id));
      else
         --  No need for a wildcard yet.  Create a simple deref
         declare
            New_Obj       : Simple_Deref_Obj_Table_Entry :=
              (Obj_Table_Entry with
               Enclosing_Obj_Id      => Collection_Obj_Id,
               Pointer_Obj_Id        => Pointer_Obj_Id,
               Matching_Prefix_Count => Matching_Prefix_Count);
            Result_Obj_Id : Dereference_Obj_Id;

         begin

            New_Obj.Kind := Simple_Deref_Obj_Kind;

            --  Find or create obj-id for object in
            --  obj table associated with collection
            Get_Sub_Object_Obj_Id
              (Current_Proc,
               Collection_Obj_Id,
               New_Obj,
               Obj_Id (Result_Obj_Id));

            --  Return new or preexisting deref
            return Result_Obj_Id;
         end;
      end if;
   end Get_Dereference_Obj_Id;

   function Find_Wildcard_Deref_Obj_Id
     (Current_Proc        : SCIL.Procedure_Body;
      Pointer_Base_Obj_Id : Obj_Id;
      Designated_Type     : SCIL.SCIL_Type;
      Collection_Obj_Id   : Obj_Id)
      return                Wildcard_Deref_Obj_Id
   is
      --  Find an obj-id for the given wildcard deref
      --  returns No_Obj_Id if not found.
      New_Obj       : Wildcard_Deref_Obj_Table_Entry :=
        (Obj_Table_Entry with
         Enclosing_Obj_Id => Collection_Obj_Id,
         Pointer_Obj_Id   => Pointer_Base_Obj_Id,
         Designated_Type  => Designated_Type);
      Result_Obj_Id : Wildcard_Deref_Obj_Id;
   begin
      New_Obj.Kind := Wildcard_Deref_Obj_Kind;

      --  Find obj-id for new object in obj table associated with collection
      Get_Sub_Object_Obj_Id
        (Current_Proc,
         Collection_Obj_Id,
         New_Obj,
         Obj_Id (Result_Obj_Id),
         Read_Only => True);
      --  And return it
      return Result_Obj_Id;
   end Find_Wildcard_Deref_Obj_Id;

   function Get_Wildcard_Deref_Obj_Id
     (Current_Proc        : SCIL.Procedure_Body;
      Pointer_Base_Obj_Id : Obj_Id;
      Designated_Type     : SCIL.SCIL_Type;
      Collection_Obj_Id   : Obj_Id)
      return                Wildcard_Deref_Obj_Id
   is
      --  Find/Create an obj-id for the given wildcard deref
      --  Requires: Collection_Obj_Id is "writable"
      New_Obj       : Wildcard_Deref_Obj_Table_Entry :=
        (Obj_Table_Entry with
         Enclosing_Obj_Id => Collection_Obj_Id,
         Pointer_Obj_Id   => Pointer_Base_Obj_Id,
         Designated_Type  => Designated_Type);
      Result_Obj_Id : Wildcard_Deref_Obj_Id;
   begin
      New_Obj.Kind := Wildcard_Deref_Obj_Kind;

      --  Find/create obj-id for new object in
      --  obj table associated with collection
      Get_Sub_Object_Obj_Id
        (Current_Proc,
         Collection_Obj_Id,
         New_Obj,
         Obj_Id (Result_Obj_Id));
      --  And return it
      return Result_Obj_Id;
   end Get_Wildcard_Deref_Obj_Id;

   function Find_Collection_Obj_Id
     (Current_Proc   : SCIL.Procedure_Body;
      Pointer_Obj_Id : Obj_Id)
      return           Obj_Id
   is
      --  Return a "writable" collection obj for the
      --  given pointer obj (based on pointer type),
      --  or return No_Obj_Id if no such collection is already
      --  present in the shadow table of the current module.
      use type SCIL.Decl_Region; --  for "+"
      use type SCIL.Object_Decl; --  for "+"

      Current_Module : constant SCIL.Module :=
         SCIL.Decl_Regions.Containing_Module (+Current_Proc);
   begin
      return Obj_Id (Find_Simple_Obj_Id
                        (Current_Proc,
                         Current_Module,
                         +SCIL.SCIL_Types.Collection_Of_Pointer_Type
                             (SCIL.Down (Type_Of_Obj_Id (Pointer_Obj_Id)))));
   end Find_Collection_Obj_Id;

   function Get_Collection_Obj_Id
     (Current_Proc   : SCIL.Procedure_Body;
      Pointer_Obj_Id : Obj_Id)
      return           Obj_Id
   is
      --  Return a "writable" collection obj for the
      --  given pointer obj (based on pointer type)
      use type SCIL.Decl_Region; --  for "+"
      use type SCIL.Object_Decl; --  for "+"

      Current_Module : constant SCIL.Module :=
         SCIL.Decl_Regions.Containing_Module (+Current_Proc);
   begin
      return Obj_Id (Get_Simple_Obj_Id
                        (Current_Proc,
                         Current_Module,
                         +SCIL.SCIL_Types.Collection_Of_Pointer_Type
                             (SCIL.Down (Type_Of_Obj_Id (Pointer_Obj_Id)))));
   end Get_Collection_Obj_Id;

   function Get_Call_Side_Effects_Obj_Id
     (Current_Proc      : SCIL.Procedure_Body;
      Collection_Obj_Id : Obj_Id;
      Proc_Obj          : Obj_Id;
      Proc_Level        : Level_Type;
      Proc_Type         : SCIL.SCIL_Type;
      Not_Yet_Analyzed  : Boolean)
      return              Call_Side_Effects_Obj_Id
   is
      --  Find/Create an obj-id for the given dereference
      --  Requires: Collection_Obj_Id is "writable"
      New_Obj       : Call_Side_Effects_Obj_Table_Entry :=
        (Obj_Table_Entry with
         Enclosing_Obj_Id => Collection_Obj_Id,
         Proc_Obj         => Proc_Obj,
         Proc_Level       => Proc_Level,
         Proc_Type        => Proc_Type,
         Not_Yet_Analyzed => Not_Yet_Analyzed);
      Result_Obj_Id : Call_Side_Effects_Obj_Id;

   begin

      New_Obj.Kind := Call_Side_Effects_Obj_Kind;

      --  Find/Create obj-id for new object in
      --  obj table associated with collection
      Get_Sub_Object_Obj_Id
        (Current_Proc,
         Collection_Obj_Id,
         New_Obj,
         Obj_Id (Result_Obj_Id));

      --  And return it
      return Result_Obj_Id;
   end Get_Call_Side_Effects_Obj_Id;

   function Find_Call_Side_Effects_Obj_Id
     ( --  TBD is Find needed??
      Current_Proc      : SCIL.Procedure_Body;
      Collection_Obj_Id : Obj_Id;
      Proc_Obj          : Obj_Id;
      Proc_Level        : Level_Type;
      Proc_Type         : SCIL.SCIL_Type) return Call_Side_Effects_Obj_Id
   is
      --  Find an obj-id for the given call_side_effects obj
      --  returns No_Obj_Id if not found.
      New_Obj       : Call_Side_Effects_Obj_Table_Entry :=
        (Obj_Table_Entry with
         Enclosing_Obj_Id => Collection_Obj_Id,
         Proc_Obj         => Proc_Obj,
         Proc_Level       => Proc_Level,
         Proc_Type        => Proc_Type,
         Not_Yet_Analyzed => False);
      Result_Obj_Id : Call_Side_Effects_Obj_Id;
   begin
      New_Obj.Kind := Call_Side_Effects_Obj_Kind;

      --  Find obj-id for new object in
      --  obj table associated with collection
      Get_Sub_Object_Obj_Id
        (Current_Proc,
         Collection_Obj_Id,
         New_Obj,
         Obj_Id (Result_Obj_Id),
         Read_Only => True);

      --  And return it
      return Result_Obj_Id;
   end Find_Call_Side_Effects_Obj_Id;

   function Get_Unknown_Result_Obj_Id
     (Current_Proc            : SCIL.Procedure_Body;
      Associated_Call_Stm     : SCIL.Call_Stm;
      Side_Effect_Obj_Id      : Call_Side_Effects_Obj_Id;
      Associated_Param        : SCIL.Spec_Param_Decl :=
        SCIL.No_Spec_Param_Decl;
      Associated_Exported_Obj : Obj_Id               := No_Obj_Id)
      return                    Unknown_Result_Obj_Id
   is
      --  Find/Create an Obj-Id for an unknown result corresponding
      --  to the given param or exported obj associated with the call
      --  on the unknown proc implied by the side-effect-obj-id.
      --  Requires: Side_Effect_Obj_Id is "writable"

      New_Obj       : Unknown_Result_Obj_Table_Entry :=
        (Obj_Table_Entry with
         Enclosing_Obj_Id        => Obj_Id (Side_Effect_Obj_Id),
         Associated_Call_Stm     => Associated_Call_Stm,
         Associated_Param        => Associated_Param,
         Associated_Exported_Obj => Associated_Exported_Obj);
      Result_Obj_Id : Unknown_Result_Obj_Id;
   begin
      New_Obj.Kind := Unknown_Result_Obj_Kind;

      --  Find/Create obj-id for new object in obj table
      --  associated with side-effect obj
      Get_Sub_Object_Obj_Id
        (Current_Proc,
         Side_Effect_Obj_Id,
         New_Obj,
         Obj_Id (Result_Obj_Id));

      --  And return it
      return Result_Obj_Id;
   end Get_Unknown_Result_Obj_Id;

   procedure Init_Name_Obj_Id
     (Associated_Name : SCIL.Name;
      Current_Proc    : SCIL.Procedure_Body)
   is
      --  Find/Create obj-id for the given Name found in given
      --  procedure body, and store on (extension of) name.
      --  If referenced, add obj-id to set of referenced obj-ids for procedure.
      use SCIL_Extension;
      use type SCIL.Decl_Region; --  for "+"

      Obj_Id_For_Name : Obj_Id;
      Current_Module  : constant SCIL.Module :=
         SCIL.Decl_Regions.Containing_Module (+Current_Proc);

      use SCIL;
   begin
      --  find/create appropriate kind of obj-id for name
      case SCIL.Kind (Associated_Name) is
         when SCIL.Simple_Name_Kind =>
            declare
               Denoted_Obj : constant SCIL.Object_Decl :=
                  SCIL.Names.Denotes (SCIL.Down (Associated_Name));
            begin
               Obj_Id_For_Name :=
                 Obj_Id (Get_Simple_Obj_Id
                            (Current_Proc,
                             Current_Module => Current_Module,
                             Denotes        => Denoted_Obj));
            end;
         when SCIL.Obj_Id_Name_Kind =>
            --  This cannot occur, obj_id name kind will never be in input.
            pragma Assert (False);
            null;

         when SCIL.Selected_Component_Name_Kind =>
            declare
               Encloser_Obj_Id : constant Obj_Id :=
                  Name_Obj_Id
                    (SCIL.Names.Record_Obj_Subn
                        (SCIL.Down (Associated_Name)));
            begin
               Obj_Id_For_Name :=
                 Obj_Id (Get_Selected_Component_Obj_Id
                            (Current_Proc,
                             Record_Obj_Id => Encloser_Obj_Id,
                             Selector      =>
                                SCIL.Names.Selector
                                  (SCIL.Down (Associated_Name))));

            end;
         when SCIL.Indexed_Component_Name_Kind =>
            declare
               Encloser_Obj_Id : constant Obj_Id :=
                  Name_Obj_Id
                    (SCIL.Names.Array_Obj_Subn
                        (SCIL.To_Indexed_Component_Name (Associated_Name)));
            begin
               Obj_Id_For_Name :=
                 Obj_Id (Get_Indexed_Component_Obj_Id
                            (Current_Proc,
                             Array_Obj_Id  => Encloser_Obj_Id,
                             Index_Expr_Id =>
                                Expr_Ids.Factory.Get_Expr_Id
                                  (Current_Proc,
                                   Array_Obj => Encloser_Obj_Id,
                                   Expr      =>
                                      SCIL.Names.Index_Subn
                                        (SCIL.Down (Associated_Name)))));

            end;

         when SCIL.Dereference_Name_Kind =>
            --  TBD what about setting EAV for dereference?
            declare
               Pointer_Obj : constant SCIL.Name :=
                  SCIL.Names.Pointer_Obj_Subn (SCIL.Down (Associated_Name));
            begin
               Obj_Id_For_Name :=
                 Obj_Id (Get_Dereference_Obj_Id
                            (Current_Proc,
                             Pointer_Obj_Id    => Name_Obj_Id (Pointer_Obj),
                             Collection_Obj_Id =>
                                Get_Collection_Obj_Id
                                  (Current_Proc,
                                   Name_Obj_Id (Pointer_Obj))));
            end;
         when SCIL.Array_Slice_Name_Kind =>
            --  for now give it the same obj_id as the array
            Obj_Id_For_Name :=
               Name_Obj_Id
                 (SCIL.Names.Array_Obj_Subn
                     (SCIL.To_Array_Slice_Name (Associated_Name)));

         when SCIL.Array_Sliding_Name_Kind =>
            --  Create a Sliding_Obj_Id, using low bounds of sliding
            --  and underlying slice.
            declare
               Array_To_Slide : constant Obj_Id :=
                   Name_Obj_Id
                     (SCIL.Names.Array_Obj_Subn
                        (SCIL.To_Array_Sliding_Name (Associated_Name)));
            begin
               Obj_Id_For_Name := Obj_Id (Get_Sliding_Obj_Id
                  (Current_Proc,
                   Sliding_Array_Obj_Id => Array_To_Slide,
                   Old_Low_Bounds => Expr_Ids.Factory.Get_Expr_Id_Array
                     (Current_Proc,
                      Array_To_Slide,
                      SCIL.Names.Low_Bound_Expr_Array
                       (SCIL.To_Array_Slice_Name
                         (SCIL.Names.Array_Obj_Subn
                           (SCIL.To_Array_Sliding_Name (Associated_Name))))),
                              --  Always a Slice below a Sliding
                   New_Low_Bounds => Expr_Ids.Factory.Get_Expr_Id_Array
                     (Current_Proc,
                      Array_To_Slide,
                      SCIL.Names.Low_Bound_Expr_Array
                       (SCIL.To_Array_Sliding_Name (Associated_Name)))));
            end;
      end case;

      --  Save obj-id on (an extension of) the Name
      Update.Set_Name_Obj_Id (Associated_Name, Obj_Id_For_Name);

   end Init_Name_Obj_Id;

   function Get_Name_Obj_Id
     (Associated_Name : SCIL.Name;
      Current_Proc    : SCIL.Procedure_Body)
      return            Obj_Id
   is
      --  Find/Create obj-id for the given Name found in given
      --  procedure body, and return it.
      use SCIL_Extension;
      use type SCIL.Decl_Region; --  for "+"

      Obj_Id_For_Name : Obj_Id;
      Current_Module  : constant SCIL.Module :=
         SCIL.Decl_Regions.Containing_Module (+Current_Proc);

      use SCIL;
   begin
      --  find/create appropriate kind of obj-id for name
      case SCIL.Kind (Associated_Name) is
         when SCIL.Simple_Name_Kind =>
            declare
               use type Driver.BE_Phase_Enum_Or_None;
               pragma Assert
                 (Driver.Cur_Phase = Driver.Persistent_Import_Phase);
               Denoted_Obj : constant SCIL.Object_Decl :=
                  SCIL.Names.Direct_Denotes (SCIL.Down (Associated_Name));
            begin
               Obj_Id_For_Name :=
                 Obj_Id (Get_Simple_Obj_Id
                            (SCIL.No_Procedure_Body,
                             Current_Module => Current_Module,
                             Denotes        => Denoted_Obj));
            end;
         when SCIL.Obj_Id_Name_Kind =>
            --  This cannot occur, obj_id name kind will never be in input.
            pragma Assert (False);
            null;

         when SCIL.Selected_Component_Name_Kind =>
            declare
               Encloser_Obj_Id : constant Obj_Id :=
                  Get_Name_Obj_Id
                    (SCIL.Names.Record_Obj_Subn
                        (SCIL.Down (Associated_Name)),
                     Current_Proc);
            begin
               Obj_Id_For_Name :=
                 Obj_Id (Get_Selected_Component_Obj_Id
                            (Current_Proc,
                             Record_Obj_Id => Encloser_Obj_Id,
                             Selector      =>
                                SCIL.Names.Selector
                                  (SCIL.Down (Associated_Name))));

            end;
         when SCIL.Indexed_Component_Name_Kind =>
            declare
               Encloser_Obj_Id : constant Obj_Id :=
                  Get_Name_Obj_Id
                    (SCIL.Names.Array_Obj_Subn
                        (SCIL.To_Indexed_Component_Name (Associated_Name)),
                     Current_Proc);
            begin
               Obj_Id_For_Name :=
                 Obj_Id (Get_Indexed_Component_Obj_Id
                            (Current_Proc,
                             Array_Obj_Id  => Encloser_Obj_Id,
                             Index_Expr_Id =>
                                Expr_Ids.Factory.Get_Expr_Id
                                  (Current_Proc,
                                   Array_Obj => Encloser_Obj_Id,
                                   Expr      =>
                                      SCIL.Names.Index_Subn
                                        (SCIL.Down (Associated_Name)))));

            end;

         when SCIL.Dereference_Name_Kind =>
            --  TBD what about setting EAV for dereference?
            declare
               Pointer_Obj : constant SCIL.Name :=
                  SCIL.Names.Pointer_Obj_Subn (SCIL.Down (Associated_Name));
            begin
               Obj_Id_For_Name :=
                 Obj_Id (Get_Dereference_Obj_Id
                            (SCIL.No_Procedure_Body,
                             Pointer_Obj_Id    =>
                                Get_Name_Obj_Id (Pointer_Obj, Current_Proc),
                             Collection_Obj_Id =>
                                Get_Collection_Obj_Id
                                  (Current_Proc,
                                   Get_Name_Obj_Id
                                      (Pointer_Obj,
                                       Current_Proc))));
            end;
         when SCIL.Array_Slice_Name_Kind =>
            --  for now give it the same obj_id as the array
            Obj_Id_For_Name :=
               Get_Name_Obj_Id
                 (SCIL.Names.Array_Obj_Subn
                     (SCIL.To_Array_Slice_Name (Associated_Name)),
                  Current_Proc);
         when SCIL.Array_Sliding_Name_Kind =>
            --  for now give it the same obj_id as the array
            Obj_Id_For_Name :=
               Get_Name_Obj_Id
                 (SCIL.Names.Array_Obj_Subn
                     (SCIL.To_Array_Sliding_Name (Associated_Name)),
                  Current_Proc);
      end case;
      return Obj_Id_For_Name;
   end Get_Name_Obj_Id;

   procedure Init_Name_Obj_Ids
     (Expr : SCIL.Expression;
      Current_Proc    : SCIL.Procedure_Body) is
   --  Walk Expr and find obj-ids associated with each SCIL.Name
   --  and store on (extension of) name.
      use SCIL, SCIL.Expressions;
      Operands : constant Expression_Seq_Length := Num_Operands (Expr);

      procedure Init_Name (Name_To_Decorate : SCIL.Name) is
         Obj : constant Obj_Id := Get_Name_Obj_Id
                                   (Name_To_Decorate, Current_Proc);
      begin
         --  Set Name_Obj_Id extension
         SCIL_Extension.Update.Set_Name_Obj_Id (Name_To_Decorate, Obj);
      end Init_Name;

   begin
      case Kind (Expr) is
      when Fetch_Exp_Kind =>
         declare
            Name_To_Decorate : constant SCIL.Name :=
              Object_Name_Subn (To_Fetch_Exp (Expr));
         begin
            Init_Name (Name_To_Decorate);
         end;
      when  others =>
         --  recurse on operands
         for I in 1 .. Operands loop
            Init_Name_Obj_Ids (Nth_Operand_Subn (Expr, I), Current_Proc);
         end loop;
      end case;
   end Init_Name_Obj_Ids;

   function Get_Corresponding_Part
     (Current_Proc  : SCIL.Procedure_Body;
      Container     : Obj_Id;
      Existing_Part : Obj_Id;
      Read_Only     : Boolean := False)
      return          Obj_Id
   is
   --  Return an obj-id representing a part of Container
   --  presuming that Existing_Part is an indexed or selected
   --  component and container is an array or record, or a
   --  deref of Container, presuming Existing_Part is a deref
   --  and Container is a pointer obj.
   begin
      if Container = No_Obj_Id then
         --  Survive case where Container is null
         return No_Obj_Id;
      end if;

      case Kind (Existing_Part) is
         when Selected_Component_Obj_Kind =>
            if Read_Only then
               --  Prevent object creation in that case by calling Find...
               return Obj_Id (Find_Selected_Component_Obj_Id
                                 (Current_Proc  => Current_Proc,
                                  Record_Obj_Id => Container,
                                  Selector      =>
                                     Selector
                                       (Selected_Component_Obj_Id (
                 Existing_Part))));
            else
               return Obj_Id (Get_Selected_Component_Obj_Id
                                 (Current_Proc  => Current_Proc,
                                  Record_Obj_Id => Container,
                                  Selector      =>
                                     Selector
                                       (Selected_Component_Obj_Id (
                 Existing_Part))));
            end if;

         when Indexed_Component_Obj_Kind =>
            if Read_Only then
               --  Prevent object creation in that case by returning No_Obj_Id
               return No_Obj_Id;
            end if;

            --  Create a corresponding indexed component,
            --  reusing the index expr unless it is an "others,"
            --  in which case we need to create a new one for the
            --  new array.
            declare
               use Expr_Ids;
               Index_Expr : Expr_Id :=
                  Index_Expr_Id (Indexed_Component_Obj_Id (Existing_Part));
            begin
               case Expr_Kind (Index_Expr) is
                  when Int_Literal_Expr =>
                     --  Use special routine to ensure that literal index
                     --  gets properly handled if others includes literal
                     --  indices.
                     return Aliasing.Get_Literally_Indexed_Component
                              (Current_Proc,
                               Array_Obj     => Container,
                               Index_Expr_Id => Index_Expr);

                  when Others_Expr =>
                     --  Get others expr-id for Container, rather than
                     --  reusing the one we have from Existing_Part.
                     --  NOTE: Using the wrong others expr-id can create
                     --  significant
                     --       problems.
                     --  Expand others of Container to cover same values
                     --(subtracting
                     --  out those covered by literal indices).
                     Aliasing.Expand_Others_To_Cover_Index_Set
                       (Current_Proc,
                        Container,
                        Expr_Ids.Factory.Others_Index_Set
                           (Index_Expr,
                            Current_Proc));

                     --  Now get expanded others expr-id
                     Index_Expr :=
                        Expr_Ids.Factory.Get_Others_Expr_Id
                          (Current_Proc,
                           Container);

                  when others =>
                     --  Fall through to use normal
                     --  Get_Indexed_Component_Obj_Id.
                     null;

               end case;

               --  Now get the corresponding indexed component
               return Obj_Id (Get_Indexed_Component_Obj_Id
                                 (Current_Proc  => Current_Proc,
                                  Array_Obj_Id  => Container,
                                  Index_Expr_Id => Index_Expr));
            end;

         when Dereference_Obj_Kind =>
            if Read_Only then
               --  Prevent object creation in that case by calling Find...
               return Obj_Id (Find_Dereference_Obj_Id
                                 (Current_Proc        => Current_Proc,
                                  Pointer_Obj_Id      => Container,
                                  Collection_Obj_Id   =>
                                     Collection_Obj_Id
                                       (Dereference_Obj_Id (Existing_Part)),
                                  Find_Wildcard_Deref => Kind (Existing_Part) =
                 Wildcard_Deref_Obj_Kind,
                                  Designated_Type     =>
                                     Type_Of_Obj_Id (Existing_Part)));
            else
               return Obj_Id (Get_Dereference_Obj_Id
                                 (Current_Proc       => Current_Proc,
                                  Pointer_Obj_Id     => Container,
                                  Collection_Obj_Id  =>
                                     Collection_Obj_Id
                                       (Dereference_Obj_Id (Existing_Part)),
                                  Get_Wildcard_Deref => Kind (Existing_Part) =
                 Wildcard_Deref_Obj_Kind,
                                  Designated_Type    =>
                                     Type_Of_Obj_Id (Existing_Part)));
            end if;
         when others =>
            pragma Assert (False);
            return null;
      end case;
   end Get_Corresponding_Part;

   function Get_Corresponding_Subcomponent
     (Current_Proc           : SCIL.Procedure_Body;
      New_Composite_Obj      : Obj_Id;
      Existing_Composite_Obj : Obj_Id;
      Existing_Subcomponent  : Obj_Id;
      Read_Only              : Boolean := False)
      return                   Obj_Id
   is
   --  Return an obj-id representing a subcomponent of New_Composite_Obj
   --  that corresponds to the given Existing_Subcomponent of the
   --  Existing_Composite_Obj.
   begin
      if Existing_Subcomponent = Existing_Composite_Obj then
         --  Recursed up to Existing_Composite_Obj --> All done
         return New_Composite_Obj;
      else
         case Kind (Existing_Subcomponent) is
            when Selected_Component_Obj_Kind | Indexed_Component_Obj_Kind =>
               --  Recurse on enclosing obj-id, and then
               --  get corresponding part of that.
               return Get_Corresponding_Part
                        (Current_Proc,
                         Container     =>
                            Get_Corresponding_Subcomponent
                              (Current_Proc,
                               New_Composite_Obj      => New_Composite_Obj,
                               Existing_Composite_Obj =>
                 Existing_Composite_Obj,
                               Existing_Subcomponent  =>
                                  Enclosing_Obj_Id
                                    (Sub_Obj_Id (Existing_Subcomponent)),
                               Read_Only              => Read_Only),
                         Existing_Part => Existing_Subcomponent,
                         Read_Only     => Read_Only);
            when others =>
               pragma Assert (False);
               return No_Obj_Id;
         end case;
      end if;
   end Get_Corresponding_Subcomponent;

   function Get_Designated_Obj_For_In_Param
     (Current_Proc : SCIL.Procedure_Body;
      Ptr_In_Param : Obj_Id)
      return         Obj_Id
   is
      --  Return an obj-id to be used as the object designated
      --  by the pointer.  This is to handle the case of an
      --  in param that points at an object that is known to
      --  be an uninitialized, "new" object, allocated by the caller.
      --  This is used for Ada elementary OUT parameters, and for
      --  the "this" parameter of a Java constructor.  It could also
      --  be used for caller-allocated "return" objects for Ada,
      --  though AdaJava doesn't currently use those.
      Proc_Region_Info : constant Region_Information.Procedure_Body_Info_Ptr
         :=
         Region_Information.Associated_Region_Info (Current_Proc);

      Mapping : Obj_Id_Mappings.Obj_Id_Mapping renames
        Region_Information.In_Param_Designated_Obj_Mapping
          (Proc_Region_Info).all;

      Existing_Desig_Obj : constant Obj_Id :=
         Obj_Id_Mappings.Lookup_Obj_Id (Mapping, Ptr_In_Param);
   begin
      if Existing_Desig_Obj /= No_Obj_Id then
         return Existing_Desig_Obj;
      else
         --  Create and add to mapping
         declare
            use type SCIL.SCIL_Node; --  for "+"

            Within : Varying_Pools.Within_Pool
              (  --  use "cumulative" pool
              Driver.Cur_Obj_Ids_Cumulative_Pool);
            pragma Unreferenced (Within);

            function Source_Info_To_Use return  SCIL.Source_Info_DPtr is
            --  Return appropriate Source info for new call-site obj
            begin
               if Kind (Ptr_In_Param) = Call_Site_Obj_Kind then
                  --  NOTE: This is the case of creating a desig obj
                  --       for an unknown call result
                  return Source_Info (Call_Site_Obj_Id (Ptr_In_Param));
               else
                  return SCIL.Source (+Denotes (Ptr_In_Param));
               end if;
            end Source_Info_To_Use;

            New_Desig_Obj : constant Obj_Id :=
               new Call_Site_Obj_Table_Entry'
              (Obj_Table_Entry with
               Level                => Level_Num (Ptr_In_Param),
               Denotes              => Denotes (Ptr_In_Param),    --  TBD:
                                                                  --  Different
                                                                  --  type?
               Is_Shadow            => False,
               Is_String_Literal    => False,
               Is_Imported          => False,
               Type_Of_Object       =>
  SCIL.SCIL_Types.Designated_Type
    (SCIL.Pointer_Type (Type_Of_Obj_Id (Ptr_In_Param))),
               Collection_Of_Object => Collection_Of_Obj_Id (Ptr_In_Param),
            --  NOTE: Not using collection of pointed-to objects
               Source_Info          => Source_Info_To_Use,
               Non_Shadow_Obj_Id    => No_Non_Shadow);

         begin
            New_Desig_Obj.Kind := Call_Site_Obj_Kind;

            --  Finish initialization of obj-id
            Factory.Init_New_Obj_Id (New_Desig_Obj, Driver.Cur_SCIL_Module);

            --  NOTE: Don't compute hash for call-site object
            --       (hash is only used for "shadow" table)

            Obj_Id_Mappings.Insert_Pair
              (Mapping,
               From => Ptr_In_Param,
               To   => New_Desig_Obj);

            declare
               Within_Temp : Varying_Pools.Within_Pool (  --  use "temp" pool
                 Driver.Cur_Temp_Pool);
               pragma Unreferenced (Within_Temp);
            begin
               --  Add to appropriate mappings, sets, etc.
               Track_New_Obj_Id (Current_Proc, New_Desig_Obj);
            end;

            return New_Desig_Obj;
         end;
      end if;
   end Get_Designated_Obj_For_In_Param;

   function Is_Designated_Obj_For_In_Param
     (Whole_Refed_Obj : Obj_Id)
      return            Boolean
   is
   --  Return True if Whole_Refed_Obj is an object created
   --  by "Obj_Ids.Factory.Get_Designated_Obj_For_In_Param"
   --  NOTE: This returns false for objects created for
   --       unknown calls when Returns_New_Object is true.
   begin
      if Kind (Whole_Refed_Obj) = Call_Site_Obj_Kind then
         declare
            Denoted_Obj : constant SCIL.Object_Decl :=
               Denotes (Whole_Refed_Obj);
            use type SCIL.Object_Decl; --  for "/="
            use type SCIL.SCIL_Type; --  for "/="
            use type SCIL.Node_Kind; --  for "/="
         begin
            return Denoted_Obj /= SCIL.No_Object_Decl
                  and then SCIL.Object_Decls.Type_Of_Object (Denoted_Obj) /=
                           Type_Of_Obj_Id (Whole_Refed_Obj)
                  and then SCIL.Kind (Denoted_Obj) /=
                           SCIL.Spec_Param_Decl_Kind;
            --  NOTE: This last check will be false when this is
            --       actually a designated obj for an unknown call result.
         end;
      else
         return False;
      end if;
   end Is_Designated_Obj_For_In_Param;

   function Is_Designated_Obj_For_In_Out_Param
     (Whole_Refed_Obj : Obj_Id)
      return            Boolean
   is
   --  Return True if Whole_Refed_Obj is an object created
   --  by "Obj_Ids.Factory.Get_Designated_Obj_For_In_Param"
   --  NOTE: This returns false for objects created for
   --       unknown calls when Returns_New_Object is true.
   begin
      if Kind (Whole_Refed_Obj) = Call_Site_Obj_Kind then
         declare
            Denoted_Obj : constant SCIL.Object_Decl :=
               Denotes (Whole_Refed_Obj);
            use type SCIL.Object_Decl; --  for "/="
            use type SCIL.SCIL_Type; --  for "/="
            use type SCIL.Node_Kind; --  for "/="
         begin
            return Denoted_Obj /= SCIL.No_Object_Decl
                  and then SCIL.Object_Decls.Type_Of_Object (Denoted_Obj) /=
                           Type_Of_Obj_Id (Whole_Refed_Obj)
                  and then SCIL.Kind (Denoted_Obj) /=
                           SCIL.Spec_Param_Decl_Kind
                  and then
            --  NOTE: This check will be false when this is
            --       actually a designated obj for an unknown call result.
                   Obj_Ids.Info.Is_By_Copy_In_Out_Param_Obj
                      (Obj_Ids.SCIL_Extension.Object_Decl_Obj_Id
                          (Denoted_Obj));
         end;
      else
         return False;
      end if;
   end Is_Designated_Obj_For_In_Out_Param;

   function Is_Object_Tag_Of_In_Param
     (Current_Proc : SCIL.Procedure_Body;
      Obj          : Selected_Component_Obj_Id)
      return         Boolean
   is
      --  Return True if is Object_Tag component of deref of in-param
      --  or of designated, uninit obj.
      use BE.SCIL;
   begin
      if Source_Information.Is_Object_Tag_Selector
           (Source_Information.Common_Printable_Source_Info'Class (Source
                                                                      (+
           Current_Proc).all),
            Selector (Obj))
      then
         --  We have the right selector for the Object_Tag,
         --  now check the base object.
         declare
            Base_Obj : constant Obj_Id := Record_Obj_Id (Obj);
         begin
            case Kind (Base_Obj) is
               when Call_Site_Obj_Kind =>
                  if Is_Designated_Obj_For_In_Param (Base_Obj) then
                     --  Base is an uninit desig-obj
                     return True;
                  end if;
               when Simple_Deref_Obj_Kind =>
                  if Obj_Ids.Info.Is_In_Param_Pointing_To_Unaliased_Obj
                       (Ptr_Obj        =>
                           Pointer_Obj_Id (Simple_Deref_Obj_Id (Base_Obj)),
                        Enclosing_Proc => Current_Proc)
                  then
                     --  Base is deref of an in-param designating an uninit obj
                     return True;
                  end if;
               when others =>
                  null;
            end case;
         end;
      end if;
      return False;
   end Is_Object_Tag_Of_In_Param;

   function Is_Value_Of_By_Copy_In_Out_Param
     (Current_Proc : SCIL.Procedure_Body;
      Obj          : Selected_Component_Obj_Id)
      return         Boolean
   is
      --  Return True if is Value of deref of in-param
      --  or of designated obj used for ref to by-copy
      --  in-out param.
      use BE.SCIL;
   begin
      if Source_Information.Is_Elementary_Value_Selector
           (Source_Information.Common_Printable_Source_Info'Class (Source
                                                                      (+
           Current_Proc).all),
            Selector (Obj))
      then
         --  We have the right selector for the
         --  by-copy in-out-param value
         declare
            Base_Obj : constant Obj_Id := Record_Obj_Id (Obj);
         begin
            case Kind (Base_Obj) is
               when Call_Site_Obj_Kind =>
                  if Is_Designated_Obj_For_In_Out_Param (Base_Obj) then
                     --  Base is a desig-obj for in-out param
                     return True;
                  end if;
               when Simple_Deref_Obj_Kind =>
                  if Obj_Ids.Info.Is_By_Copy_In_Out_Param_Obj
                       (Pointer_Obj_Id (Simple_Deref_Obj_Id (Base_Obj)))
                  then
                     --  Base is deref of an in-param designating temp
                     --  used for by-copy in-out param
                     return True;
                  end if;
               when others =>
                  null;
            end case;
         end;
      end if;
      return False;
   end Is_Value_Of_By_Copy_In_Out_Param;

   function Is_Inited_Component_Of_Unaliased_Obj
     (Current_Proc : SCIL.Procedure_Body;
      Obj          : Selected_Component_Obj_Id)
      return         Boolean
   is
   --  Return True if is Object_Tag component of deref of in-param
   --  or of designated, uninit obj, or is Value of by-copy in-out param
   begin
      return Is_Object_Tag_Of_In_Param (Current_Proc, Obj)
            or else Is_Value_Of_By_Copy_In_Out_Param (Current_Proc, Obj);
   end Is_Inited_Component_Of_Unaliased_Obj;

   procedure Assign_Inited_Components_Of_Unaliased_Objs
     (Current_Proc         : SCIL.Procedure_Body;
      Var_Entry_BB_Assigns : in out SCIL.Factory.Var_Statement_Seq)
   is
      --  For each "local" created to represent an unaliased param,
      --  initialize any components whose value comes from the caller.

      pragma Warnings (Off);  -- TBD
      Proc_Region_Info : constant Region_Information.Procedure_Body_Info_Ptr :=
        Region_Information.Associated_Region_Info (Current_Proc);

      Mapping : Obj_Id_Mappings.Obj_Id_Mapping renames
        Region_Information.In_Param_Designated_Obj_Mapping
          (Proc_Region_Info).all;
      --  TBD: We are using this one mapping for two
      --      different purposes (in-param => desig obj,
      --      and deref-based-obj => desig-obj-based-obj).
      --      Probably should have two different mappings someday.
      pragma Warnings (On);

      procedure Assign_Component_If_Inited
        (Original_Obj : Obj_Id;
         Local_Obj    : Obj_Id)
      is
      begin
         if Kind (Local_Obj) = Selected_Component_Obj_Kind
           and then Is_Inited_Component_Of_Unaliased_Obj
                       (Current_Proc,
                        Selected_Component_Obj_Id (Local_Obj))
         then
            --  Add an assignment to Entry_BB_Assigns
            --  from Original_Obj to Local_Obj
            declare
               use SCIL;  --  for operators, etc.

               Local_Obj_Name : constant SCIL.Name      :=
                  +Create_Obj_Id_Name
                      (Local_Obj,
                       SCIL.Object_Decls.Var,
                       Source (+Current_Proc));
               Orig_Obj_Name  : constant SCIL.Name      :=
                  +Create_Obj_Id_Name
                      (Original_Obj,
                       SCIL.Object_Decls.Var,
                       Source (+Current_Proc));
               Assign         : constant SCIL.Statement :=
                  +SCIL.Factory.New_Elementary_Assign_Stm
                      (Local_Obj_Name,
                       +SCIL.Factory.New_Fetch_Exp
                           (Object_Name_Subn => Orig_Obj_Name,
                            Source_Info      => Source (+Current_Proc)),
                       Source (+Current_Proc));
            begin
               Statement_Seqs.Append (Var_Entry_BB_Assigns, Assign);
            end;

         end if;
      end Assign_Component_If_Inited;

      procedure Assign_Inited_Components is new Obj_Id_Mappings.Iterate (
         Assign_Component_If_Inited);
      pragma Unreferenced (Assign_Inited_Components);
   begin
      null;
      --  TBD: Assign_Inited_Components(Mapping);  -- TBD: is this necessary?
   end Assign_Inited_Components_Of_Unaliased_Objs;

   function Replace_Designated_Obj
     (Current_Proc : SCIL.Procedure_Body;
      Tracked_Obj  : Obj_Id)
      return         Obj_Id
   is
      --  Return Tracked_Obj, after replacing its "root" object,
      --  if it is an obj created to represent the designated, uninit obj
      --  of an in-param pointer, with a deref of the in-param.
      Root_Obj : Obj_Id;
   begin
      if Tracked_Obj = No_Obj_Id then
         --  Pass through No_Obj_Id without change
         return Tracked_Obj;
      end if;

      --  Jump up to base of addressing tree
      Root_Obj := Obj_Ids.Info.Enclosing_Root_Obj (Tracked_Obj);

      if not Is_Designated_Obj_For_In_Param (Root_Obj) then
         --  No replacement needed
         return Tracked_Obj;
      else
         --  Replace designated obj with deref of in param
         --  and remember in mapping
         declare
            Proc_Region_Info : constant
              Region_Information.Procedure_Body_Info_Ptr :=
               Region_Information.Associated_Region_Info (Current_Proc);

            Mapping : Obj_Id_Mappings.Obj_Id_Mapping renames
              Region_Information.In_Param_Designated_Obj_Mapping (
              Proc_Region_Info).all;
            --  TBD: We are using this one mapping for two
            --      different purposes (in-param => desig obj,
            --      and deref-based-obj => desig-obj-based-obj).
            --      Probably should have two different mappings someday.

            function Replace_Designated_Obj_Rec
              (Tracked_Obj : Obj_Id)
               return        Obj_Id
            is
               --  Recursive version of Replace_Designated_Obj, which
               --  presumes that Root_Obj is a designated-obj for an in-param
               use Obj_Ids.Factory;
            begin
               case Kind (Tracked_Obj) is
                  when Simple_Obj_Kind               |
                       Call_Site_Heap_Slice_Obj_Kind |
                       Sliding_Obj_Kind              |
                       Unknown_Result_Obj_Kind       |
                       Call_Side_Effects_Obj_Kind    =>
                     --  Shouldn't happen
                     pragma Assert (False);
                     return Tracked_Obj;

                  when Call_Site_Obj_Kind =>
                     --  We reached the root
                     declare
                        pragma Assert (Tracked_Obj = Root_Obj);
                        Associated_Pointer_Obj : constant Obj_Id :=
                           Obj_Ids.SCIL_Extension.Object_Decl_Obj_Id
                             (Denotes (Tracked_Obj));
                     begin
                        --  Create an explicit deref of the associated in
                        --  param.
                        return Obj_Id (Get_Dereference_Obj_Id
                                          (Current_Proc      => Current_Proc,
                                           Pointer_Obj_Id    =>
                          Associated_Pointer_Obj,
                                           Collection_Obj_Id =>
                                              Get_Collection_Obj_Id
                                                (Current_Proc,
                                                 Associated_Pointer_Obj)));
                     end;

                  when Selected_Component_Obj_Kind =>
                     --  Return corresponding component
                     return Get_Corresponding_Part
                              (Current_Proc,
                               Container     =>
                                  Replace_Designated_Obj_Rec
                                    ( --  Recurse
                                     Record_Obj_Id
                                        (Selected_Component_Obj_Id (
                       Tracked_Obj))),
                               Existing_Part => Tracked_Obj);

                  when Indexed_Component_Obj_Kind =>
                     --  Return corresponding component
                     return
                       Get_Corresponding_Part
                         (Current_Proc,
                          Container     =>
                            Replace_Designated_Obj_Rec
                              ( --  Recurse
                               Array_Obj_Id
                                 (Indexed_Component_Obj_Id (Tracked_Obj))),
                               Existing_Part => Tracked_Obj);

                  when Dereference_Obj_Kind =>
                     return Get_Corresponding_Part
                              (Current_Proc,
                               Container     =>
                                  Replace_Designated_Obj_Rec
                                    ( --  Recurse
                                     Pointer_Obj_Id
                                        (Dereference_Obj_Id (Tracked_Obj))),
                               Existing_Part => Tracked_Obj);

               end case;
            end Replace_Designated_Obj_Rec;

            Within : Varying_Pools.Within_Pool (Driver.Cur_Temp_Pool);
            pragma Unreferenced (Within);
            --  TBD: Not clear why this is necessary
            --      but deep within Get_Sub_Object_Obj_Id
            --      we expect to be in the temp-pool

            Result : constant Obj_Id :=
               Replace_Designated_Obj_Rec (Tracked_Obj);
         begin
            if Result = No_Obj_Id then
               --  Unexpected kind of obj in Get_Corresponding_Part
               pragma Assert (False);
               return Tracked_Obj;
            elsif not Obj_Id_Mappings.Is_Present_In_Mapping
                        (Mapping,
                         Result)
            then
               --  Enter Result into mapping
               declare
                  Within_Cumulative : Varying_Pools.Within_Pool
                    (Driver.Cur_Obj_Ids_Cumulative_Pool);
                  pragma Unreferenced (Within_Cumulative);
               begin
                  Obj_Id_Mappings.Insert_Pair
                    (Mapping,
                     From => Result,
                     To   => Tracked_Obj);
               end;
            end if;

            return Result;
         end;
      end if;
   end Replace_Designated_Obj;

   function Get_Tracked_Obj_For_DMOD
     (Current_Proc : SCIL.Procedure_Body;
      DMOD_Obj     : Obj_Id)
      return         Obj_Id
   is
      --  Return Tracked Obj for given DMOD, in case that
      --  DMOD is based on uninit designated obj for in-param.
      Proc_Region_Info : constant Region_Information.Procedure_Body_Info_Ptr
         :=
         Region_Information.Associated_Region_Info (Current_Proc);

      Mapping : Obj_Id_Mappings.Obj_Id_Mapping renames
        Region_Information.In_Param_Designated_Obj_Mapping
          (Proc_Region_Info).all;
      --  TBD: We are using this one mapping for two
      --      different purposes (in-param => desig obj,
      --      and deref-based-obj => desig-obj-based-obj).
      --      Probably should have two different mappings someday.

      Tracked_Obj : constant Obj_Id :=
         Obj_Id_Mappings.Lookup_Obj_Id (Mapping, DMOD_Obj);
      use type SCIL.SCIL_Type; --  for "="
   begin
      if Tracked_Obj /= No_Obj_Id
        and then Type_Of_Obj_Id (Tracked_Obj) = Type_Of_Obj_Id (DMOD_Obj)
      then
         --  Found the original tracked obj
         return Tracked_Obj;
      else
         --  No original tracked obj in mapping
         return DMOD_Obj;
      end if;
   end Get_Tracked_Obj_For_DMOD;

   function Create_Obj_Id_Name
     (Obj                : Obj_Id;
      Obj_Id_Object_Kind : SCIL.Object_Decls.Object_Kind_Enum;
      Source_Info        : SCIL.Source_Info_DPtr;
      New_Obj_Id_Level   : Level_Type := Invalid_Level)
      --  If defaulted means use Level_Num(Obj)
      return               SCIL.Obj_Id_Name
   is
      --  Create Obj_Id_Name for Obj and set its Name_Obj_Id extension to Obj

      Obj_Id_Level : SCIL.Decl_Regions.Nesting_Level;
      Result       : SCIL.Obj_Id_Name;
      Within       : Varying_Pools.Within_Pool
        (Driver.Cur_Obj_Ids_Cumulative_Pool);
      pragma Unreferenced (Within);

      use type SCIL.Decl_Regions.Nesting_Level; --  for "/="
      use type SCIL.Name; --  for "+"
   begin
      if New_Obj_Id_Level /= Invalid_Level then
         --  Explicit Obj_Id_Level has been provided
         Obj_Id_Level := New_Obj_Id_Level;
      else
         Obj_Id_Level := Level_Num (Obj);
      end if;

      --  Create Obj_Id_Name
      Result :=
         SCIL.Factory.New_Obj_Id_Name
           (Type_Of_Obj_Id (Obj),
            Obj_Id_Level,
            Obj_Id_Object_Kind,
            Source_Info);

      --  Set Name_Obj_Id extension
      SCIL_Extension.Update.Set_Name_Obj_Id (+Result, Obj);

      return Result;
   end Create_Obj_Id_Name;

end BE.Obj_Ids.Factory;
