------------------------------------------------------------------------------
--                                 Q G E N                                  --
--                                                                          --
--                     Copyright (C) 2011-2017, AdaCore                     --
--                     Copyright (C) 2011-2017, IB Krates                   --
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
------------------------------------------------------------------------------

with Ada.Assertions;                use Ada.Assertions;
with EventHandler;                  use EventHandler;
with Geneauto_Constructors;         use Geneauto_Constructors;
with Geneauto_Factory;              use Geneauto_Factory;
with HiMoCo.BlockDiagramCmg.Utils;  use HiMoCo.BlockDiagramCmg.Utils;
with HiMoCo.BlockLibrary;           use HiMoCo.BlockLibrary;
with HiMoCo.BlockLibrary.SubSystem; use HiMoCo.BlockLibrary.SubSystem;
with HiMoCo.BlockLibrary.SFChart;   use HiMoCo.BlockLibrary.SFChart;
with HiMoCo.Utils;                  use HiMoCo.Utils;
with HiMoCo.Utils.Code_Model;       use HiMoCo.Utils.Code_Model;
with Debugger;
with QGen_Traces;
with QGen_Traces.Renderer;          use QGen_Traces.Renderer;

with HiMoCo.Name_Handlers;
pragma Elaborate (HiMoCo.Name_Handlers);

with HiMoCo.BlockDiagramCmg.SB_Vars_Manager;
use HiMoCo.BlockDiagramCmg.SB_Vars_Manager;
with HiMoCo.BlockDiagramCmg.Flattener;
use HiMoCo.BlockDiagramCmg.Flattener;
with HiMoCo.BlockDiagramCmg.Argument_Lifter;
use HiMoCo.BlockDiagramCmg.Argument_Lifter;
pragma Elaborate (HiMoCo.BlockDiagramCmg.SB_Vars_Manager);

package body HiMoCo.BlockDiagramCmg is

   function Mangled_Name
     (Raw_Name        : String;
      Repl_Invalid    : String := "_";
      Whitespace_Only : Boolean := False;
      Repl_Empty      : String := "a")
      return String renames Get_Mangled_Name;

   function Params_Module_Name
     (Self : GACodeModel'Class) return String renames Get_Params_Module_Name;

   package Module_Name_Handler is
     new HiMoCo.Name_Handlers (Module'Class);

   function "=" (Left, Right : Variable'Class) return Boolean
   is (Left.Equals (Right));
   --  Workaround for N625-026 compiler bug

   pragma Warnings (Off, "=");
   --  N625-026 Workaround for another compiler bug. "=" is used to instantiate
   --  the generic package below but the compiler still issues a warning.

   To_Flatten : SystemBlock_List;
   --  List of subsystems with function packaging activated

   procedure Mark_Unreferenced_IncomingVariable (Bl : Block'Class);
   --  mark Bl incoming variable as unreferenced if it has no other destination
   --  block.

   procedure Process_Params_And_Types
     (Self  : in out CodeModelGenerator;
      SM    : GASystemModel'Class;
      CM    : GACodeModel'Class;
      Kind  : Manage_Vars_Kind;
      Types : in out CustomType_List);
   --  process types and variables in the system model:
   --  * list all custom types found in the system model in New_Type_List.
   --  * create and initialize variables found in the system model in new
   --    module Params.
   --  * move external modules from system model to code model
   --  Note that the only placeholder for variables and types in system model
   --  is the first root subsystem of the model.

   procedure Process_Block
     (Self : in out CodeModelGenerator;
      Obj  : Block'Class);
   --  In this function we check for the use of unsupported matlab functions
   --  inside the block's parameters.

   procedure Create_Complex_Module (Cm : GACodeModel'Class);
   --  Create complex module with primitive functions for complex types
   --  This module should be replaced by himoco_arith

   ----------------------------------------
   -- Mark_Unreferenced_IncomingVariable --
   ----------------------------------------

   procedure Mark_Unreferenced_IncomingVariable (Bl : Block'Class) is

      function All_Signals_To_Sink_Blocks (P : Outport'Class) return Boolean;
      --  Returns true if all signals are going to T_Display, BT_Scope,
      --  BT_SignalViewerScope, BT_Terminator, BT_ToWorkspace blocks.

      function All_Signals_To_Sink_Blocks (P : Outport'Class) return Boolean
      is
         S_List : constant Signal_List'Class := P.Get_outgoingSignals;
         Signals_To_Sinks : Natural := 0;
      begin
         for S of S_List loop
            declare
               Bl : constant Block'Class := Block'Class
                 (S.Get_dstPort.Container);
            begin
               if Bl.Is_Not_Null then
                  case Get_Block_Type (Bl.Get_type) is
                  when BT_Display |
                       BT_Scope |
                       BT_SignalViewerScope |
                       BT_Terminator |
                       BT_DataTypeDuplicate |
                       BT_ToWorkspace =>
                     Signals_To_Sinks := Signals_To_Sinks + 1;
                  when others =>
                     null;
                  end case;
               end if;

            end;
         end loop;
         if P.Get_outgoingSignals.Length = Signals_To_Sinks then
            return True;
         else
            return False;
         end if;
      end All_Signals_To_Sink_Blocks;

   begin
      for Port of Bl.Get_inDataPorts loop
         declare
            Src_Port : constant Outport'Class :=
              Port.Get_incomingSignal.Get_srcPort;
            --  the source port of Bl incoming signal.

            In_Exp : constant Expression'Class :=
              Src_Port.Get_interfaceReference;
            --  Bl incoming expression.
         begin

            --  no need to go further if the incoming expression is null.
            if In_Exp.Is_Not_Null and then
              In_Exp in VariableExpression'Class
            then
               declare
                  In_Var : constant Variable'Class :=
                    VariableExpression'Class (In_Exp).Get_variable;
                  --  Bl incoming variable.

               begin
                  --  mark Bl incoming variable as unreferenced if this
                  --  variable has only one destination block.
                  In_Var.Set_isUnreferenced
                    (All_Signals_To_Sink_Blocks (Src_Port));
               end;
            end if;
         end;
      end loop;
   end Mark_Unreferenced_IncomingVariable;

   -------------------
   -- Process_Block --
   -------------------

   procedure Process_Block
     (Self : in out CodeModelGenerator;
      Obj  : Block'Class)
   is
      Enabler_Refs : Expression_List;
      --  Expressions_From_EnablePorts (Obj);

      --  determine the block type implementation
      BT : constant BlockTypeDefinition_Access := Get_Block_Type_Impl (Obj);

      SB : SystemBlock'Class renames SystemBlock'Class (Obj.Container);
   begin

      --  Check that no unsupported matlab function is used in
      --  block parameters
      for Par of Obj.Get_parameters loop
         if Par.Get_value.Is_Not_Null then
            declare
               To_Check : Element_Sequence'Class :=
                 Par.Get_value.Contained_Elements (Recurse => True);
            begin
               To_Check.Append (Par.Get_value);

               for E of To_Check loop
                  if E in CallExpression'Class then
                     Assert_Resolved_Call
                       (CallExpression'Class (E), Obj, Par.Get_name);
                  end if;
               end loop;
            end;
         end if;
      end loop;

      --  process end block.
      if Is_End_Block (Obj) then
         --  for end blocks, detect unreferenced incoming variable.
         Mark_Unreferenced_IncomingVariable (Obj);
      end if;

      Enabler_Refs.Allocate;   --  just to initialize

      --  init states
      declare
         St_List : constant Statement_List'Class :=
           BT.Get_InitStateStatements (Obj);
      begin
         if St_List.Length > 0 then
            SB.Get_initStatesFunction.Append_Statement
              (Annotate_Statement_List (Obj, St_List));
         end if;
      end;

      --  reset outputs
      if Obj in InterfaceBlock'Class and then Obj.Get_type = "Outport" then
         declare
            St_List : constant Statement_List'Class :=
              BT.Get_ResetOutputsStatements (Obj);
         begin
            if St_List.Length > 0 then
               SB.Get_resetOutputsFunction.Append_Statement
                 (Annotate_Statement_List (Obj, St_List));
            end if;
         end;
      end if;

      --  init outputs
      declare
         St_List : constant Statement_List'Class :=
           BT.Get_InitOutputsStatements (Obj);
      begin
         if St_List.Length > 0 then
            SB.Get_initOutputsFunction.Append_Statement
              (Annotate_Statement_List (Obj, St_List));
         end if;
      end;

      --  main init
      declare
         St_List : constant Statement_List'Class :=
           BT.Get_InitStatements (Obj);
      begin
         if St_List.Length > 0 then
            SB.Get_initFunction.Append_Statement
              (Annotate_Statement_List (Obj, St_List));
         end if;
      end;

      --  compose the compute phase code
      --  BDCG-PB-60-Compute
      if Obj.Get_inlinableInSwitchPort.Is_Null then
         --  Code for inlinable blocks will be generated by the switch blocks

         begin
            declare
               St_List : constant Statement_List'Class :=
                 BT.Get_ComputeStatements (Obj);

            begin
               if not Obj.Get_outControlPorts.Is_Empty then

                  --  Memorize function calls to other blocks. The arguments of
                  --  these calls will later be lifted to the function
                  --  arguments of the caller function.
                  Self.Compute_Function_Calls.Extend (St_List);
               end if;

               SB.Get_computeFunction.Append_Statement
                 (Annotate_Statement_List (Obj, St_List));
            end;
         exception
               --  when assertions are disabled, this exception cannot be
               --  raised
            when Assertion_Error =>
               Handle_Event
                 ("Error when processing block " & Obj.Get_infoString, Error);
               --  stop execution asap
               raise;
            when Failed_Code_Gen =>
               --  propagate code generation exception
               raise;
            when others =>
               Handle_Event
                 ("Error when processing block " & Obj.Get_infoString, Error);
               raise;
         end;
      end if;

      --  compose the memory update phase code
      --  BDCG-PB-70-MemUpdate
      if not Obj.Is_Controlled then
         declare
            St_List  : constant Statement_List'Class :=
              BT.Get_MemUpdateStatements (Obj);
            Msg : constant String := "update ";

         begin
            if Obj in InterfaceBlock'Class then
               Self.MemUpdate_St_List.Prepend
                 (Annotate_Statement_List
                    (Obj, St_List, Msg));
            else
               Self.MemUpdate_St_List.Append
                 (Annotate_Statement_List
                    (Obj, St_List, Msg));
            end if;

         end;
      end if;

   end Process_Block;

   ------------------------------
   -- Process_Params_And_Types --
   ------------------------------

   procedure Process_Params_And_Types
     (Self  : in out CodeModelGenerator;
      SM    : GASystemModel'Class;
      CM    : GACodeModel'Class;
      Kind  : Manage_Vars_Kind;
      Types : in out CustomType_List)
   is

      procedure Add_Custom_Types_From_Namespace
        (Ns    : NameSpace'Class;
         Types : in out CustomType_List);
      --  Removes a Custom Type from NameSpace Ns and moves it to the Types
      --  collection.

      procedure Add_Custom_Types_From_Namespace
        (Ns    : NameSpace'Class;
         Types : in out CustomType_List)
      is
         Cur : Element_Sequences.Cursor :=
           Ns.Get_customTypes.First;
      begin
         while Cur.Has_Element loop
            declare
               CT : constant CustomType'Class :=
                 CustomType'Class (Cur.Element);
            begin
               Cur.Next;
               CT.Remove_From_Container;
               Types.Append (CT);
            end;
         end loop;
      end Add_Custom_Types_From_Namespace;

      Cm_Modules           : Module_List'Class := CM.Get_modules;
      --  list of modules in code model
      Sm_Modules           : Module_List;
      --  a buffer for system model modules

      procedure Add_Custom_Types_And_Signals (SB : SystemBlock'Class);
      --  Recursively collect all custom types and signals objects added to
      --  systemblocks.

      procedure Add_Custom_Types_And_Signals (SB : SystemBlock'Class)
      is

      begin
         --  Add custom types
         Add_Custom_Types_From_Namespace (SB, Types);

         --  Add simulink signals
         for Var of SB.Get_variables loop
            if Var.Get_isSignalObject then
               Self.Simulink_Signals.Append (Var);
            end if;
         end loop;

         --  Process all the contained subsystems
         for Bl of SB.Get_blocks loop
            if Bl in SystemBlock'Class then
               Add_Custom_Types_And_Signals (SystemBlock'Class (Bl));
            end if;
         end loop;
      end  Add_Custom_Types_And_Signals;
   begin
      for M of SM.Get_modules loop
         Sm_Modules.Append (M);
      end loop;

      for M of Sm_Modules loop
         if M.Get_name = Params_Module_Name (CM) then

            --  Copy all the variables declared in the base workspace
            Create_and_Init_Variables
              (SM, CM, M, Kind, Self.Simulink_Signals);

            --  also add the custom types from the base workspace
            Add_Custom_Types_From_Namespace (M, Types);
         else
            --  move other external modules from system model to code model

            for Var of M.Get_variables loop
               if Var.Get_isSignalObject then
                  Self.Simulink_Signals.Append (Var);
               end if;
            end loop;
            M.Remove_From_Container;
            Cm_Modules.Append (M);
         end if;
      end loop;

      --  look for types and signals in subsystems
      for SB of Get_Root_Subsystems (SM) loop
         Add_Custom_Types_And_Signals (SB);
      end loop;
   end Process_Params_And_Types;

   ------------------------------
   -- Process_SystemBlock_Code --
   ------------------------------

   procedure Process_SystemBlock_Code
     (Self         : in out CodeModelGenerator;
      Obj          : SystemBlock'Class;
      CM           : GACodeModel'Class;
      Kind         : Manage_Vars_Kind;
      Is_Top_Level : Boolean := False;
      Use_Fcn_Pkg  : Boolean;
      All_Variants : Boolean)
   is

      BT : HiMoCo.BlockLibrary.SubSystem.BlockSubSystem;

      procedure Process_Block_Memory (C : Ordered_Block_Lists.Cursor);
      procedure Process_Block_Outdataports (C : Ordered_Block_Lists.Cursor);

      procedure Process_Blocks_Recursive (C : Ordered_Block_Lists.Cursor);
      --  Dispatch call according to block type.
      --  Introduces recursion in case of system block

      Subsystem_CMG : aliased CodeModelGenerator :=
        (Code_Model      => Self.Code_Model,
         Full_Flattening => Self.Full_Flattening,
         Simulink_Signals => Self.Simulink_Signals,
         others          => <>);

      Sorted_Block_List : Ordered_Block_Lists.Set;

      --------------------------
      -- Process_Block_Memory --
      --------------------------

      procedure Process_Block_Memory (C : Ordered_Block_Lists.Cursor)
      is
         B : constant Block'Class := Ordered_Block_Lists.Element (C);
      begin
         Variables_From_BlockState (B, Subsystem_CMG.Sys_Module.all);
      end Process_Block_Memory;

      --------------------------------
      -- Process_Block_Outdataports --
      --------------------------------

      procedure Process_Block_Outdataports (C : Ordered_Block_Lists.Cursor)
      is
         B : constant Block'Class := Ordered_Block_Lists.Element (C);
      begin
         Variables_From_BlockOutDataPorts (B, Subsystem_CMG);

         for OP of B.Get_outDataPorts loop
            if OP.Get_dataType.Get_primitiveType in TComplexNumeric'Class then
               --  Check whether the model uses complex types
               Create_Complex_Module (CM);
               exit;
            end if;
         end loop;
      end Process_Block_Outdataports;

      ------------------------------
      -- Process_Blocks_Recursive --
      ------------------------------

      procedure Process_Blocks_Recursive (C : Ordered_Block_Lists.Cursor)
      is
         B : constant Block'Class := Ordered_Block_Lists.Element (C);
      begin
         if B in SystemBlock'Class then
            Process_SystemBlock_Code
              (Subsystem_CMG, SystemBlock'Class (B), CM, Kind,
               Use_Fcn_Pkg => Use_Fcn_Pkg, All_Variants =>  All_Variants);
         else
            Process_Block (Subsystem_CMG, B);
         end if;

         --  Lift arguments of function call targets, as needed
         --  TODO: Only arguments that correspond to ports that are in
         --  subsytems *above* the container of the caller must be lifted.
         --  Currently, all are lifted.

      end Process_Blocks_Recursive;

      Is_Top_Level_Not_Lib : constant Boolean :=
        Is_Top_Level
        and then Obj.Get_Parameter_Value_Str ("Is_Lib") /= "on";

   begin

      Log (Component => QGen_Traces.CMG_Simulink,
           Msg       => "Generating code model elements for SystemBlock",
           Obj       => Obj,
           Level     => QGen_Traces.Debug);

      --  create function stubs and add references to Systemblock object
      --  generate arguments for compute and init functions
      Log (Component => QGen_Traces.CMG_Simulink,
           Msg       => "Creating functions",
           Obj       => Obj,
           Level     => QGen_Traces.Debug);

      HiMoCo.BlockLibrary.SubSystem.Create_Functions (Obj);

      Subsystem_CMG.Sys_Module :=
        new Module'Class'(Module'Class (Obj.Get_codeModelElement));

      --  order the blocks by executionOrder attribute
      for Bl of Obj.Get_blocks loop
         if Debugger.Status then
            Debugger.Store_ExecutionOrder (Bl.Get_originalFullName,
                                           Bl.Get_executionOrder);
         end if;
         Sorted_Block_List.Insert (Bl);
      end loop;

      --  process variables from block
      Log (Component => QGen_Traces.CMG_Simulink,
           Msg       => "Creating variables for OutDataPorts",
           Obj       => Obj,
           Level     => QGen_Traces.Debug);
      Sorted_Block_List.Iterate (Process_Block_Outdataports'Access);

      --  Copy model workspace variables to the codemodel
      --  Note that this operation must be performed after creating the
      --  variables for OutDataPorts, because there are workspace variables
      --  (such as Simulink parameters) whose type may be derived from the
      --  processing of the out data ports (when datatype is "auto").
      Log (Component => QGen_Traces.CMG_Simulink,
           Msg       => "Copying model workspace variables",
           Obj       => Obj,
           Level     => QGen_Traces.Debug);

      declare
         CM_Vars : Variable_List'Class :=
           Subsystem_CMG.Sys_Module.Get_variables;

      begin
         for SM_Var of Obj.Get_variables loop
            declare
               CM_Var : constant Variable'Class :=
                 Variable'Class (SM_Var.Copy);

            begin
               SM_Var.Set_codeModelElement (CM_Var);
               CM_Var.Set_sourceElement (SM_Var);

               --  Allow name mangling for model workspace variables.
               --
               --  Generally we do not want to change the name of user
               --  defined constants. However since the model workspace
               --  variables can override the base workspace ones and also
               --  span over multiple atomic systems a compromise has to
               --  be made to avoid naming problems in C as there are no
               --  namespaces there to make global variables unique.
               --
               --  Base workspace variables (_params module) are the only
               --  user defined variables that cannot be mangled.
               CM_Var.Set_isFixedName (False);

               CM_Vars.Append (CM_Var);
            end;
         end loop;
      end;

      Log (Component => QGen_Traces.CMG_Simulink,
           Msg       => "Creating memory variables",
           Obj       => Obj,
           Level     => QGen_Traces.Debug);
      Sorted_Block_List.Iterate (Process_Block_Memory'Access);

      --  generate (init and compute) code from the block
      Log (Component => QGen_Traces.CMG_Simulink,
           Msg       => "Creating code statements",
           Obj       => Obj,
           Level     => QGen_Traces.Debug);
      Sorted_Block_List.Iterate (Process_Blocks_Recursive'Access);

      --  transfer the memory update statements to the update or compute
      --  function
      for St of Subsystem_CMG.MemUpdate_St_List loop
         if Is_Top_Level_Not_Lib then
            Obj.Get_computeFunction.Append_Statement (St);
         else
            Obj.Get_updateFunction.Append_Statement (St);
         end if;
      end loop;

      --  generate calls to initStates and initOutputs in init function.
      if Is_Top_Level_Not_Lib then
         declare
            Init_Fun            : constant FFunction'Class :=
              Obj.Get_initFunction;
            Init_Outputs_Stmnts : constant Statement_List'Class :=
              BT.Get_InitOutputsStatements (Obj);

         begin
            Init_Fun.Append_Statement
              (New_ExpressionStatement
                 (New_CallExpression (Obj.Get_initStatesFunction)));

            Init_Fun.Append_Statement
              (Collapse_Statement_List (Init_Outputs_Stmnts));
         end;

         --  no update function for top-level block
         declare
            Update_F : FFunction'Class := Obj.Get_updateFunction;
         begin
            Update_F.Delete;
         end;

      elsif not Is_Top_Level then
         --  Handle non library blocks

         --  generate call to resetOutputs in initOutputs function
         declare
            Reset_Outputs_Stmnts : constant Statement_List'Class :=
              BT.Get_ResetOutputsStatements (Obj);

         begin
            Subsystem_CMG.Reset_Outputs_Function_Calls.Extend
              (Reset_Outputs_Stmnts);

            Obj.Get_initOutputsFunction.Append_Statement
              (Collapse_Statement_List (Reset_Outputs_Stmnts));
         end;

         --  only the top-level block has init function
         declare
            Init_F : FFunction'Class := Obj.Get_initFunction;
         begin
            Init_F.Delete;
         end;

         --  if not in the top-level subsystem, then call block visitor to
         --  generate calls
         Process_Block (Self, Obj);

         if Use_Fcn_Pkg then
            --  Determine whether the subsystem should be flattened
            declare
               Code_Gen_Mode : constant String :=
                 Obj.Get_Parameter_Value_Str ("RTWSystemCode");
            begin
               if Code_Gen_Mode /= "Function"
                 or else Code_Gen_Mode /= "Reusable function"
               then
                  To_Flatten.Append (Obj);
               end if;
            end;
         end if;
      end if;
   end Process_SystemBlock_Code;

   ---------------------------
   -- Create_Complex_Module --
   ---------------------------

   procedure Create_Complex_Module (Cm : GACodeModel'Class) is
      Complex_Mod : constant Module'Class :=
        Get_Complex_Module (Cm);
      Functions   : FFunction_List'Class := Complex_Mod.Get_functions;
   begin
      if Functions.Is_Empty then
         Complex_Mod.Set_sourceLanguage (Val => "WRAPPER");
         declare
            --  ComplexSingle functions
            Make_SComp      : FFunction'Class :=
              New_Function
                ("make_scomplex", Complex_Mod, Scope => EXPORTED);
            Make_SComp_Real : FunctionArgument'Class :=
              New_FunctionArgument
                ("real", New_TRealSingle, IIN, Make_SComp);
            pragma Unreferenced (Make_SComp_Real);
            Make_SComp_Imag : FunctionArgument'Class :=
              New_FunctionArgument
                ("imag", New_TRealSingle, IIN, Make_SComp);
            pragma Unreferenced (Make_SComp_Imag);
            Make_SComp_Res  : FunctionArgument'Class :=
              New_FunctionArgument
                ("res", New_TComplexSingle, OOUT, Make_SComp);
            pragma Unreferenced (Make_SComp_Res);

            SComp_Real      : FFunction'Class :=
              New_Function
                ("scomplex_real", Complex_Mod, New_TRealSingle, EXPORTED);
            SComp_Real_Arg  : FunctionArgument'Class :=
              New_FunctionArgument
                ("c", New_TComplexSingle, IIN, SComp_Real);
            pragma Unreferenced (SComp_Real_Arg);

            SComp_Imag      : FFunction'Class :=
              New_Function
                ("scomplex_imag", Complex_Mod, New_TRealSingle, EXPORTED);
               SComp_Imag_Arg  : FunctionArgument'Class :=
              New_FunctionArgument
                ("c", New_TComplexSingle, IIN, SComp_Imag);
            pragma Unreferenced (SComp_Imag_Arg);

            --  ComplexDouble functions
            Make_DComp      : FFunction'Class :=
              New_Function
                ("make_dcomplex", Complex_Mod, Scope => EXPORTED);
            Make_DComp_Real : FunctionArgument'Class :=
              New_FunctionArgument
                ("real", New_TRealDouble, IIN, Make_DComp);
            pragma Unreferenced (Make_DComp_Real);
            Make_DComp_Imag : FunctionArgument'Class :=
              New_FunctionArgument
                ("imag", New_TRealDouble, IIN, Make_DComp);
            pragma Unreferenced (Make_DComp_Imag);
            Make_DComp_Res  : FunctionArgument'Class :=
              New_FunctionArgument
                ("res", New_TComplexDouble, OOUT, Make_DComp);
            pragma Unreferenced (Make_DComp_Res);

            DComp_Real      : FFunction'Class :=
              New_Function
                ("dcomplex_real", Complex_Mod, New_TRealDouble, EXPORTED);
            DComp_Real_Arg  : FunctionArgument'Class :=
              New_FunctionArgument
                ("c", New_TComplexDouble, IIN, DComp_Real);
            pragma Unreferenced (DComp_Real_Arg);

            DComp_Imag      : FFunction'Class :=
              New_Function
                ("dcomplex_imag", Complex_Mod, New_TRealDouble, EXPORTED);
            DComp_Imag_Arg  : FunctionArgument'Class :=
              New_FunctionArgument
                ("c", New_TComplexDouble, IIN, DComp_Imag);
            pragma Unreferenced (DComp_Imag_Arg);

         begin
            Functions.Append (Make_SComp);
            Functions.Append (SComp_Real);
            Functions.Append (SComp_Imag);

            Functions.Append (Make_DComp);
            Functions.Append (DComp_Real);
            Functions.Append (DComp_Imag);
         end;
      end if;
   end Create_Complex_Module;

   -------------------------
   -- Generate_Code_Model --
   -------------------------

   procedure Generate_Code_Model
     (Self         : in out CodeModelGenerator;
      System_Model : MOF.Model'Class;
      Code_Model   : MOF.Model'Class;
      Kind         : Manage_Vars_Kind;
      Use_Fcn_Pkg  : Boolean;
      All_Variants : Boolean)
   is
      procedure Resolve_Variable_Refs_And_Name_Conflicts;
      --  replaces references to system model variables with corresponding
      --  code model variable references
      --
      --  checks all namespaces in the model for name conflicts
      --  each namespace is checked individually -- if lower level
      --  namespaces include the same name, it is assumed,
      --  that the name is redefined. Name uniqueness is enforced for all
      --  namespace elements across all subtypes
      --
      --  Binds variable expressions with no variable reference to variables
      --  by name

      procedure Resolve_Variable_Scopes;
      --  Checks if all of the variable expressions and the variables they
      --  reference are in the same hierarchical NS. Resolves all conflicting
      --  pairs.

      procedure Remove_Unused_Signal_Objects;
      --  Check that all signal objects have been typed. Remove signals that
      --  are still untyped (TInherited) and are not used in any expression.

      ----------------------------------------------
      -- Resolve_Variable_Refs_And_Name_Conflicts --
      ----------------------------------------------

      procedure Resolve_Variable_Refs_And_Name_Conflicts
      is
         GACm : constant GACodeModel'Class :=
           Get_GACodeModel (Self.Code_Model, True);

         All_Elems : constant Element_Sequence'Class :=
           GACm.Contained_Elements (Recurse => True);

         Module_Name_Idx  : Module_Name_Handler.Name_Handler;

         --  variable references without a variable
         Unbound_Var_Refs : Expression_List;

         --  local variables sorted by namesapce
         Variables_By_Ns  : Namespace_To_Element_Map_Maps.Map;
         --  global variables
         Variables_Global : Name_To_Ns_Element_Maps.Map;

         function Get_Variable_From_Params_Module (Var_Name : String)
                                                   return Variable'Class;
         function Get_Variable_From_Params_Module (Var_Name : String)
                                                   return Variable'Class
         is
            Param_Module : constant String := Params_Module_Name (GACm);

         begin
            for M of GACm.Get_modules loop
               if M.Get_name = Param_Module then
                  for V of M.Get_variables loop
                     if V.Get_name = Var_Name then
                        return V;
                     end if;
                  end loop;
               end if;
            end loop;
            return Null_Variable;
         end Get_Variable_From_Params_Module;

         function Get_All_Vars_From_Params_Module return Variable_List;
         function Get_All_Vars_From_Params_Module return Variable_List
         is
            Params_Module : constant String := Params_Module_Name (GACm);
            Vars : Variable_List;
         begin
            for M of GACm.Get_modules loop
               if M.Get_name = Params_Module then
                  for V of M.Get_variables loop
                     Vars.Append (V);
                  end loop;
               end if;
            end loop;
            return Vars;
         end Get_All_Vars_From_Params_Module;

      begin
         --  index the module names
         Module_Name_Idx.Index_Elements (GACm, False);

         --  index all other elements
         for E of All_Elems loop
            if E in Module'Class then
               --  Resolve name conflicts by enforcing name uniqueness inside
               --  of a module
               Ensure_Unique_Names (Module'Class (E),
                                    Get_All_Vars_From_Params_Module);
            elsif E in VariableExpression'Class then
               --  Resolve variable references from SystemModel to CodeModel
               declare
                  Var_Ex  : VariableExpression'Class renames
                    VariableExpression'Class (E);
                  Old_Var : constant Variable'Class :=
                    Var_Ex.Get_variable;
               begin
                  if Old_Var.Is_Null then
                     --  schedule for variable binding
                     Unbound_Var_Refs.Append (Var_Ex);
                  else

                     --  check if the variable is in the code model
                     if Old_Var.Get_codeModelElement.Is_Not_Null then
                        Var_Ex.Set_variable
                          (Variable'Class (Old_Var.Get_codeModelElement));
                     end if;

                     --  relocate variable, if needed
                     Relocate_Variable (Var_Ex);
                  end if;
               end;
            elsif E in Variable'Class then
               Elem_To_Map (Variable'Class (E),
                            Variables_Global,
                            Variables_By_Ns);
            elsif E in CallExpression'Class then
               declare
                  Call : CallExpression'Class renames CallExpression'Class (E);
                  Fun  : constant FFunction'Class := Call.Get_function;
               begin

                  --  Check that the call is made to a known function
                  Assert_Resolved_Call (Call);

                  --  Replace calls to SM functions with calls to resp.
                  --  CM functions
                  if Call.Get_standardFunction = NONE
                    and then Fun.Is_Not_Null
                    and then Fun.Get_codeModelElement.Is_Not_Null
                  then
                     Call.Set_function (FFunction'Class
                                        (Fun.Get_codeModelElement));
                  end if;
               end;
            end if;
         end loop;

         --  bind variables by name
         --  given that the code above ensures unique names over all model
         --  elements, we can expect, that if name exists, then it is a
         --  variable
         for Var_Ex of Unbound_Var_Refs loop
            declare
               Var : constant Variable'Class :=
                 Variable'Class (Elem_By_Name (Get_Namespace (Var_Ex),
                                 Var_Ex.Get_name,
                                 Variables_Global,
                                 Variables_By_Ns));
               Param_Var : constant Variable'Class :=
                 Get_Variable_From_Params_Module (Var_Ex.Get_name);
            begin
               if Var.Is_Not_Null then
                  VariableExpression'Class (Var_Ex).Set_variable (Var);
               elsif Param_Var.Is_Not_Null then
                  VariableExpression'Class (Var_Ex).Set_variable (Param_Var);
               else
                  declare
                     Source_Ref : constant String :=
                         (if Var_Ex.Get_sourceElement.Is_Not_Null
                          then
                              ASCII.LF
                              & "  Source element of the expression: "
                              & ASCII.LF
                              & Var_Ex.Get_sourceElement.Get_Original_Ref
                          else "");
                  begin
                     Handle_Event
                       ("Can not resolve VariableExpression."
                        & ASCII.LF
                        & "  Expression : " & Var_Ex.Get_Original_Ref
                        & ASCII.LF
                        & "  references variable with name='" & Var_Ex.Get_name
                        & "', which is not found."
                        & Source_Ref, Error);
                  end;
               end if;
            end;
         end loop;

      end Resolve_Variable_Refs_And_Name_Conflicts;

      -----------------------------
      -- Resolve_Variable_Scopes --
      -----------------------------

      procedure Resolve_Variable_Scopes is
         GACm : constant GACodeModel'Class :=
           Get_GACodeModel (Self.Code_Model, True);

         All_Elems : constant Element_Sequence'Class :=
           GACm.Contained_Elements (Recurse => True);
      begin
         for E of All_Elems loop
            if E in VariableExpression'Class then
               --  Resolve variable references from SystemModel to CodeModel
               declare
                  Var_Ex  : VariableExpression'Class renames
                    VariableExpression'Class (E);
               begin
                  Relocate_Variable (Var_Ex);
               end;
            end if;
         end loop;
      end Resolve_Variable_Scopes;

      ----------------------------------
      -- Remove_Unused_Signal_Objects --
      ----------------------------------

      procedure Remove_Unused_Signal_Objects is
         To_Remove : Variable_List;
      begin
         for Var of Self.Simulink_Signals loop
            if Var.Is_Not_Null and then Var.Get_dataType in TInherited'Class
            then

               if Var.Get_referencedBy.Is_Empty then
                  To_Remove.Append (Var);
               end if;

               --  make sure we remove the codemodel variable as well
               if Var.Get_codeModelElement.Is_Not_Null and then
                 Var.Get_codeModelElement in Variable'Class
               then
                  declare
                     CM_Var : constant Variable'Class := Variable'Class
                       (Var.Get_codeModelElement);
                  begin
                     if CM_Var.Get_dataType in TInherited'Class then

                        if CM_Var.Get_referencedBy.Is_Empty then
                           To_Remove.Append (CM_Var);
                        end if;
                     end if;
                  end;
               end if;
            end if;
         end loop;

         for E of To_Remove loop
            declare
               EE : EObject'Class := E;
            begin
               EE.Delete;
            end;
         end loop;
      end Remove_Unused_Signal_Objects;

      function Type_Filter_CB (El : EObject'Class) return Boolean
      is (El in ChartBlock'Class);

      SM : constant GASystemModel'Class :=
        Get_GASystemModel (System_Model, True);
      --  if a code model already exists, then take that one. This may be
      --  possible because we may have imported a code model and a system
      --  model
      CM : constant GACodeModel'Class :=
        (if not Code_Model.All_Root_Elements.Is_Empty and then
         Code_Model.All_Root_Elements.First_Element in GACodeModel'Class then
         GACodeModel'Class (Code_Model.All_Root_Elements.First_Element)
         else New_GACodeModel (Code_Model, SM));

      New_Type_List : CustomType_List;

      Ordered_Roots : constant SystemBlock_List := Get_Root_Subsystems (SM);
   begin

      Self.Code_Model := MOF.Model (Code_Model);
      --  and for the block library
      Init_Block_Library (Self.Code_Model);

      --  process types and variables in the system model:
      --  * list all custom types found in the system model in New_Type_List.
      --  * create and initialize variables found in the system model in new
      --    module Params.
      --  * move external modules from system model to code model
      Process_Params_And_Types (Self, SM, CM, Kind, New_Type_List);

      for Sys of Ordered_Roots loop
         Process_SystemBlock_Code
           (Self, Sys, CM, Kind, True, Use_Fcn_Pkg, All_Variants);
      end loop;

      --  create a types module, when new types have been generated
      if not New_Type_List.Is_Empty then
         declare
            Types_Module : constant Module'Class :=
              New_Module
                (In_Code_Model => CM,
                 Name          => Get_Types_Module_Name (CM),
                 Is_External   => False,
                 Unique        => True);
            Ns_Types     : CustomType_List'Class :=
              Types_Module.Get_customTypes;
         begin
            Ns_Types.Extend (New_Type_List);
         end;
      end if;

      Process_Call_Arguments (SM);

      for CB of SM.Contained_Elements
        (Type_Filter_CB'Unrestricted_Access, True)
      loop
         Copy_Chart_IO_Variables (ChartBlock'Class (CB));
      end loop;

      Resolve_Variable_Refs_And_Name_Conflicts;

      Remove_Unused_Signal_Objects;

      --  remove all void functions
      Remove_Void_Functions (SM);

      for Subsys of To_Flatten loop
         Flatten (Subsys, False, False);
      end loop;

      if Use_Fcn_Pkg then
         Resolve_Variable_Scopes;
      end if;

      if Self.Full_Flattening /= SKIP then
         Flatten (SM, Self.Full_Flattening);

         --  Check if all variable expressions have the right scope after
         --  flattening
         Resolve_Variable_Scopes;
      end if;

      To_Flatten.Clear;
   end Generate_Code_Model;

end HiMoCo.BlockDiagramCmg;
