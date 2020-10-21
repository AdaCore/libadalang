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

with BE.Obj_Ids.Constant_Init_Procs;
with BE.Obj_Ids.Global_Constants;
with BE.Obj_Ids.SCIL_Extension.Update;

with BE.Driver;
with BE.SCIL.Decl_Regions;

with ST.Mains.Debug_Flags;
pragma Elaborate_All (ST.Mains.Debug_Flags);

with Utils.Storage_Management.Varying_Pools.Debug;

package body BE.Obj_Ids.Region_Information.Factory is

   Test_Reinit_Dmods : constant Boolean :=
      ST.Mains.Debug_Flags.Flag_Is_On
        ("reinit_dmods",
         On_By_Default => False);
   --  This seems to cause "too many iterations" in picocontainer (3/20/06)

   procedure Init_Module_Region_Info
     (Node        : SCIL.Module;
      Is_Imported : Boolean)
   is
      --  Init region info associated with given module and Is_Imported flag.
      use type SCIL.Decl_Region; --  for "+"

      Within      : Varying_Pools.Within_Pool
        (Driver.Cur_Obj_Ids_Cumulative_Pool);
      pragma Unreferenced (Within);
      Module_Info : constant Module_Info_Ptr := new Module_Info_Rec;
   begin
      --  Initialize Module_Info record
      Module_Info.Module_Is_Imported         := Is_Imported;
      Module_Info.Level_Num                  := 0;
      Module_Info.Associated_Decl_Region     := +Node;
      Module_Info.Associated_Module          := Node;
      Module_Info.Shadow_Obj_Table           := null;
      Module_Info.Global_Const_Init_Table    :=
        Constant_Init_Procs.No_Constant_Init_Proc_Mapping;
      Module_Info.Global_Const_Ref_Table     :=
        Global_Constants.No_Obj_Id_Global_Constant_Mapping;

      --  Initialize shadow-obj table with access-discrim to module
      Module_Info.Shadow_Obj_Table           :=
        new Obj_Table (Module_Info.Associated_Module'Unchecked_Access);
      Module_Info.Global_Const_Init_Table    :=
         Constant_Init_Procs.New_Empty_Constant_Init_Proc_Mapping;
      Module_Info.Global_Const_Ref_Table     :=
         Global_Constants.New_Empty_Global_Constant_Mapping;

      --  Now store the region info in an extension of module node
      SCIL_Extension.Update.Set_Decl_Region_Info
        (+Node,
         Region_Info_Ptr (Module_Info));
   end Init_Module_Region_Info;

   procedure Reinit_Module_Region_Info (Node : SCIL.Module) is
      --  Reinit region that was marked Imported before
      --  to be non-imported.
      Module_Info : constant Module_Info_Ptr := Associated_Region_Info (Node);
      pragma Assert (Module_Info.Module_Is_Imported);
   begin
      Module_Info.Module_Is_Imported := False;
   end Reinit_Module_Region_Info;

   procedure Init_Procedure_Body_Region_Info
     (Node : SCIL.Procedure_Body)
   is
      --  Init region info associated with given procedure body
      use type SCIL.Decl_Region; --  for "+"
      Null_VM : Known_Value_Sets.Obj_Id_Value_Mapping;
      --  default is null;
      Within    : Varying_Pools.Within_Pool
        (Driver.Cur_Obj_Ids_Cumulative_Pool);
      pragma Unreferenced (Within);
      Reg_Info  : constant Region_Info_Ptr         :=
         new Procedure_Body_Info_Rec;
      Proc_Info : constant Procedure_Body_Info_Ptr :=
         Procedure_Body_Info_Ptr (Reg_Info);
   begin
      Proc_Info.Proc_Is_Poisoned       := False;
      Proc_Info.Level_Num              :=
         SCIL.Decl_Regions.Region_Level (+Node);
      Proc_Info.Associated_Decl_Region := +Node;
      Proc_Info.Enclosing_Region_Info  :=
         Associated_Region_Info (SCIL.Decl_Regions.Container (Node));
      Obj_Id_Sets.Make_Empty (Proc_Info.DMOD_Set);
      Obj_Id_Sets.Make_Empty (Proc_Info.Proc_Obj_Id_In_Progress);
      Obj_Id_Sets.Make_Empty (Proc_Info.Final_DMOD_Set);
      Obj_Id_Sets.Make_Empty (Proc_Info.Uplevel_Referenced_Constants);
      Obj_Id_Sets.Make_Empty (Proc_Info.Exported_New_Objects);
      Obj_Id_Sets.Make_Empty (Proc_Info.Exported_Unknown_Results);
      --  Formals is constructed when needed from info on procedure body
      --  (maybe not a good idea?)
      Proc_Info.KVS_Clock                         := 0;
      Proc_Info.Proc_Obj_Id_Alias_Mapping         :=
         Aliasing.New_Empty_Alias_Mapping;
      Proc_Info.Proc_Obj_Id_Tracked_Alias_Mapping :=
         Aliasing.New_Empty_Alias_Mapping;
      Proc_Info.Proc_Obj_Id_Other_Alias_Mapping   :=
         Aliasing.New_Empty_Alias_Mapping;
      Proc_Info.Proc_Obj_Id_Value_Mapping         := Null_VM;
      Obj_Id_Sets.Make_Empty (Proc_Info.Final_UPE_Set);
      Obj_Id_Sets.Make_Empty (Proc_Info.Final_EAV_Set);
      Proc_Info.DMOD_Values                            :=
         Known_Value_Sets.New_Empty_Value_Mapping;
      Proc_Info.DMOD_Change_Counts                     :=
         Known_Value_Sets.New_Empty_Change_Count_Mapping;
      Proc_Info.Index_Change_Counts                    :=
         Known_Value_Sets.New_Empty_Change_Count_Mapping;
      Proc_Info.Index_Mapping                          :=
         Known_Value_Sets.New_Empty_Index_Mapping;
      Proc_Info.Proc_Obj_Id_Static_Components_Mapping  :=
         Tracked_Components.New_Empty_Components_Mapping;
      Proc_Info.Proc_Obj_Id_Dynamic_Components_Mapping :=
         Tracked_Components.New_Empty_Components_Mapping;
      Obj_Id_Sets.Make_Empty (Proc_Info.Proc_Referenced_Obj_Ids);
      Obj_Id_Sets.Make_Empty (Proc_Info.Refed_Obj_Id_Exporters);
      Obj_Id_Sets.Make_Empty (Proc_Info.Caller_Relevant_Refed_Obj_Ids);
      Proc_Info.Proc_Heap_Array_Mapping :=
         SCIL_To_Obj_Id_Mappings.New_Empty_SCIL_To_Obj_Id_Mapping;

      Obj_Id_Sets.Make_Empty (Proc_Info.Obj_Ids_Already_Tracked);
      Obj_Id_Sets.Make_Empty (Proc_Info.Composite_Objs_Outside_Of_Loop);
      Obj_Id_Sets.Make_Empty (Proc_Info.Composite_Objs_Of_Current_Loop);

      Proc_Info.Do_Not_Untrack_Types :=
        SCIL.SCIL_Node_Sets.New_Empty_SCIL_Node_Set;

      Proc_Info.Matching_Type_Info               :=
         Matching_Types.New_Empty_Matching_Type_Info;
      Proc_Info.Procedure_Effects                :=
         Known_Value_Sets.Empty_Procedure_Effects;
      Proc_Info.Rerun_Obj_Main                   := False;
      Proc_Info.Iterate_Obj_Id                   := False;
      Proc_Info.Is_Part_Of_Loop                  := False;
      Proc_Info.Procedure_Effect_Timestamp       := 0;
      Proc_Info.Procedure_Dependence_Mapping     :=
         Procedure_Dependence.New_Empty_Dependence_Mapping;
      Proc_Info.In_Param_Designated_Obj_Mapping :=
         Obj_Id_Mappings.New_Empty_Obj_Id_Mapping;

      SCIL_Extension.Update.Set_Decl_Region_Info (+Node, Reg_Info);
   end Init_Procedure_Body_Region_Info;

   procedure Reinit_Procedure_Body_Region_Info
     (Node : SCIL.Procedure_Body)
   is
      --  Init region info associated with given procedure body.
      --  NOTE: This involves only the data that starts fresh on each
      --       iteration.  In particular, we do not re-init the
      --       information that is caller visible (e.g. DMOD_Values).
      use type SCIL.Decl_Region; --  for "+"
      Region_Info : constant Procedure_Body_Info_Ptr :=
         Procedure_Body_Info_Ptr (Associated_Region_Info (+Node));
      pragma Assert
        (Driver.Cur_Temp_Pool = Varying_Pools.Debug.Get_Current_Pool);
      Temp_Pool_VM : constant Known_Value_Sets.Obj_Id_Value_Mapping :=
         Known_Value_Sets.New_Empty_Value_Mapping;
   begin
      Obj_Id_Sets.Reinit (Region_Info.Obj_Ids_Already_Tracked);
      Obj_Id_Sets.Reinit (Region_Info.Composite_Objs_Outside_Of_Loop);
      Obj_Id_Sets.Reinit (Region_Info.Composite_Objs_Of_Current_Loop);
      SCIL.SCIL_Node_Sets.Reinit (Region_Info.Do_Not_Untrack_Types);
      Obj_Id_Sets.Reinit (Region_Info.Proc_Obj_Id_In_Progress);
      Obj_Id_Sets.Reinit (Region_Info.Refed_Obj_Id_Exporters);

      --  Reinitialize structures that live in Cumulative Pool
      --  TBD: We should dealloate the existing space occupied
      --      by these data structures.  Perhaps these should
      --      be in the "batch" pool since they aren't of interest
      --      to callers, only to subsequent phases.  By putting
      --      them in the "batch" pool we would get some amount
      --      of automatic cleanup.

      declare
         Within : Varying_Pools.Within_Pool
           (Driver.Cur_Obj_Ids_Cumulative_Pool);
         pragma Unreferenced (Within);
      begin
         if Test_Reinit_Dmods then
            --  Reinitialize the DMOD set (prior final DMODs saved in
            --   Final_DMOD_Set)
            --  NOTE: This seems to cause "too many iterations"
            --       in picocontainer (3/20/06)
            Obj_Id_Sets.Deallocate (Region_Info.DMOD_Set);
         end if;
         Region_Info.KVS_Clock                         := 0;
         Region_Info.Proc_Obj_Id_Value_Mapping         := Temp_Pool_VM;
         Region_Info.Proc_Obj_Id_Alias_Mapping         :=
            Aliasing.New_Empty_Alias_Mapping;
         Region_Info.Proc_Obj_Id_Tracked_Alias_Mapping :=
            Aliasing.New_Empty_Alias_Mapping;
         Region_Info.Proc_Obj_Id_Other_Alias_Mapping   :=
            Aliasing.New_Empty_Alias_Mapping;
         Region_Info.Rerun_Obj_Main                    := False;
         Region_Info.Iterate_Obj_Id                    := False;
         Region_Info.Is_Part_Of_Loop                   := False;
         --  NOTE: We are now reinitializing the static tracked-components
         --       mapping as well as the above information, to help
         --       ensure that the effects of obj-id stabilize.
         --       It turns out the presence of a component in the tracked
         --       components mapping affects obj-id "expansion" during value
         --       tracking, and if a tracked component is added during
         --       value tracking itself, on subsequent passes its presence
         --       earlier could affect the final results.  By starting the
         --       mapping back at empty on each iteration, we avoid that
         --       problem.
         Region_Info.Proc_Obj_Id_Static_Components_Mapping :=
            Tracked_Components.New_Empty_Components_Mapping;
      end;

      --  NOTE: We could reinit the procedure dependence mapping here,
      --       but that seems to be fairly slow, and it seems
      --       relatively rare that on a subsequent pass, a given
      --       dependence doesn't appear at all (e.g. due to an overflow
      --       situation) and the corresponding procedure does in fact
      --       change.

   end Reinit_Procedure_Body_Region_Info;

   procedure Reset_Before_Value_Tracker_Iteration
     (Proc_Info : Procedure_Body_Info_Ptr)
   is
      --  Reset mappings and sets before iterating value tracker
      --  due to new composite-obj subcomponent.
      pragma Assert
        (Driver.Cur_Temp_Pool = Varying_Pools.Debug.Get_Current_Pool);
   begin
      Obj_Id_Sets.Make_Empty (Proc_Info.Composite_Objs_Outside_Of_Loop);
      Obj_Id_Sets.Make_Empty (Proc_Info.Composite_Objs_Of_Current_Loop);

      Proc_Info.KVS_Clock := 0;
      Known_Value_Sets.Reset_Value_Mapping
        (Proc_Info.Proc_Obj_Id_Value_Mapping);
      Proc_Info.Iterate_Obj_Id  := False;
      Proc_Info.Is_Part_Of_Loop := False;
   end Reset_Before_Value_Tracker_Iteration;

end BE.Obj_Ids.Region_Information.Factory;
