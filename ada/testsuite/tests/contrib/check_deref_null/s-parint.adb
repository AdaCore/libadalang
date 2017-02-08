------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           S Y S T E M . P A R T I T I O N _ I N T E R F A C E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2016, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;

with Ada.Characters.Handling;
with Ada.Finalization;
with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System.Address_To_Access_Conversions;
with System.Standard_Library;

with GNAT.HTable;
with GNAT.Regpat;

with PolyORB.Binding_Data;
with PolyORB.DSA_P.Exceptions;
with PolyORB.DSA_P.Name_Service;
with PolyORB.Dynamic_Dict;
with PolyORB.Errors;
with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.Opaque;
with PolyORB.ORB;
with PolyORB.Parameters;
pragma Elaborate_All (PolyORB.Parameters);
with PolyORB.POA;
with PolyORB.POA_Config;
with PolyORB.POA_Config.RACWs;
with PolyORB.POA_Manager;
with PolyORB.POA_Types;
with PolyORB.QoS;
with PolyORB.QoS.Exception_Informations;
with PolyORB.QoS.Term_Manager_Info;
with PolyORB.Request_QoS;
with PolyORB.Sequences.Unbounded;
with PolyORB.Sequences.Unbounded.Helper;
pragma Elaborate_All (PolyORB.Sequences.Unbounded.Helper);
with PolyORB.Services.Naming;
with PolyORB.Services.Naming.Helper;
with PolyORB.Services.Naming.NamingContext;
with PolyORB.Services.Naming.NamingContext.Client;
with PolyORB.Setup;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;
with PolyORB.Termination_Activity;
with PolyORB.Utils.Configuration_File;
with PolyORB.Utils.Strings.Lists;

package body System.Partition_Interface is

   use Ada.Streams;

   use PolyORB.Any;
   use PolyORB.DSA_P.Name_Service;
   use PolyORB.References;

   package PL renames PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("system.partition_interface");
   procedure O (Message : String; Level : PL.Log_Level := PL.Debug)
     renames L.Output;
   function C (Level : PL.Log_Level := PL.Debug) return Boolean
     renames L.Enabled;

   --  A few handy aliases

   package PSNNC  renames PolyORB.Services.Naming.NamingContext;
   package PTC    renames PolyORB.Tasking.Condition_Variables;
   package PTM    renames PolyORB.Tasking.Mutexes;
   package PTCV   renames PolyORB.Tasking.Condition_Variables;
   package PTT    renames PolyORB.Tasking.Threads;
   package PUCFCT renames PolyORB.Utils.Configuration_File.Configuration_Table;

   subtype String_Ptr is PolyORB.Utils.Strings.String_Ptr;
   use type String_Ptr;

   function To_Lower (S : String) return String
     renames Ada.Characters.Handling.To_Lower;

   function Make_Global_Key (Section, Key : String) return String
     renames PolyORB.Parameters.Make_Global_Key;

   --  An opaque octet sequence

   package Octet_Sequences is
     new PolyORB.Sequences.Unbounded (PolyORB.Types.Octet);

   package Octet_Sequences_Helper is new Octet_Sequences.Helper
     (Element_From_Any => PolyORB.Any.From_Any,
      Element_To_Any   => PolyORB.Any.To_Any,
      Element_Wrap     => PolyORB.Any.Wrap);

   TC_Opaque_Cache : PATC.Local_Ref;
   --  Typecode for the opaque octet sequence

   ---------------------------------------------------------------
   -- Special operation names for remote call interface objects --
   ---------------------------------------------------------------

   Op_Resolve : constant String := "resolve";
   --  Corresponds to the CORBA CosNaming::NamingContext::resolve operation.

   Op_Get_Partition_Id : constant String := "_get_partition_id";
   --  Get the DSA partition identifier for the partition that hosts an RCI

   ------------------------
   -- Local declarations --
   ------------------------

   Critical_Section : PTM.Mutex_Access;
   --  Protects shared data structures at the DSA personality level

   procedure Initialize_Parameters;
   procedure Initialize;
   procedure Shutdown (Wait_For_Completion : Boolean);
   --  Procedures called during global PolyORB initialization

   procedure Detach;
   --  Detach a procedure using setsid() and closing the standard
   --  input/standard output/standard error file descriptors.

   Local_PID_Barrier   : PTC.Condition_Access;
   --  Barrier used by task waiting for Local_PID_Allocated to become True

   ------------------------------------------------
   -- Termination manager of the local partition --
   ------------------------------------------------

   --  These values are set by Register_Termination_Manager, which is called
   --  during elaboration of Termination_Manager.Bootstrap.

   The_TM_Ref : Ref := Nil_Ref;
   --  Reference to the termination manager

   The_TM_Oid     : PolyORB.Objects.Object_Id_Access;
   --  The termination manager Object ID

   The_TM_Address : System.Address;
   --  The local termination manager servant address

   The_TM_Shutdown : PolyORB.Initialization.Finalizer;
   --  The local termination manager shutdown hook

   --------------------------------
   -- Map of all known RCI units --
   --------------------------------

   type RCI_State is (Initial, Invalid, Pending, Live, Dead);

   type RCI_Info is limited record
      Is_All_Calls_Remote : Boolean := True;
      --  True if the package is remote or pragma All_Call_Remotes applies

      Base_Ref            : Object_Ref;
      --  Main reference for package

      Lookup_Done         : PTCV.Condition_Access;
      --  Condition variable used for callers to block while Base_Ref is
      --  being looked up from the name server.

      Is_Local            : Boolean := False;
      --  True if the package is assigned on local partition

      Reconnection_Policy : Reconnection_Policy_Type :=
        Default_Reconnection_Policy;
      --  Reconnection policy for this RCI's partition

      State               : RCI_State := Initial;
      --  Initial: never looked up from name server
      --  Live:    valid ref or trying to reconnect
      --  Dead:    permanently unreachable (for Reject_On_Restart policy)

      Known_Partition_ID  : Boolean := False;
      --  True if the package is not assigned on local partition, and its
      --  partition ID is known.

      RCI_Partition_ID    : RPC.Partition_ID := RPC.Partition_ID'First;
      --  Cache of RCI's partition ID, if known

   end record;

   type RCI_Info_Access is access all RCI_Info;

   package Known_RCIs is new PolyORB.Dynamic_Dict (RCI_Info_Access);
   --  This list is keyed with the lowercased full names of the RCI units.
   --  Concurrent accesses to Known_RCIs after elaboration must be protected
   --  by the DSA critical section.

   procedure Retrieve_RCI_Info (Name : String; Info : in out RCI_Info_Access);
   --  Retrieve RCI information for a local or remote RCI package. If Info
   --  is already set to a non-null value, it is used as the RCI_Info for the
   --  unit, else it is looked up from Known_RCIs, and possibly dynamically
   --  allocated (if not alread present in Known_RCIs).

   --  To limit the amount of memory leaked by the use of distributed object
   --  stub types, these are referenced in a hash table and reused whenever
   --  possible. Access to this hash table is protected by the DSA critical
   --  section.

   type Hash_Index is range 0 .. 100;
   function Hash (K : RACW_Stub_Type_Access) return Hash_Index;

   ---------------------------
   -- DSA parameters source --
   ---------------------------

   Conf_Table : PUCFCT.Table_Instance;

   type DSA_Source is
     new PolyORB.Parameters.Parameters_Source with null record;

   overriding function Get_Conf
     (Source       : access DSA_Source;
      Section, Key : String) return String;

   The_DSA_Source : aliased DSA_Source;

   --  DSA configuration manages attributes that apply to RCI units, or to
   --  partitions. All attributes have names of the form
   --  <rci-or-partition-name>'<attribute>

   type DSA_Attribute is (Partition, Reconnection, Location);

   subtype RCI_Attribute is
     DSA_Attribute range Partition .. Reconnection;
   subtype Partition_Attribute is
     DSA_Attribute range Location  .. Location;

   function DSA_Attr (Name : String; Attr : DSA_Attribute) return String;

   function RCI_Attr (Name : String; Attr : RCI_Attribute) return String
     renames DSA_Attr;

   function Partition_Attr
     (Name : String; Attr : Partition_Attribute) return String
     renames DSA_Attr;

   RPC_Timeout : Duration;
   --  Default timeout applied to all remote calls

   Local_Partition_Name : String_Ptr;
   --  Name of the local partition

   ------------------------
   -- Internal functions --
   ------------------------

   function Compare_Content
     (Left, Right : RACW_Stub_Type_Access) return Boolean;

   package Objects_HTable is new GNAT.HTable.Simple_HTable
     (Header_Num => Hash_Index,
      Element    => RACW_Stub_Type_Access,
      No_Element => null,
      Key        => RACW_Stub_Type_Access,
      Hash       => Hash,
      Equal      => Compare_Content);

   --  When a RACW must be constructed to designate a local object, an object
   --  identifier is created using the address of the object.

   subtype Local_Oid is
     PolyORB.Objects.Object_Id (1 .. System.Address'Size / 8);

   function To_Local_Oid is
     new Ada.Unchecked_Conversion (System.Address, Local_Oid);
   function To_Address is
     new Ada.Unchecked_Conversion (Local_Oid, System.Address);

   procedure Setup_Object_RPC_Receiver
     (Name            : String;
      Default_Servant : Servant_Access);
   --  Set up an object adapter to receive method invocation requests for
   --  distributed object type Name. Use the specified POA configuration (which
   --  must include the USER_ID, NON_RETAIN and USE_DEFAULT_SERVANT policies).
   --  The components of Servant are set appropriately.

   function DSA_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence) return Any;
   --  Construct an Any from an Ada exception raised by a servant

   function To_Name (Id, Kind : String) return PolyORB.Services.Naming.Name;
   --  Construct a name consisting of a single name component with the given
   --  id and kind.

   function Get_DSA_Conf (Var : String) return String;
   --  Return Var from configuration section [dsa]

   ------------------------------------------
   -- List of all RPC receivers (servants) --
   ------------------------------------------

   function Link
     (S     : access Private_Info;
      Which : PolyORB.Utils.Ilists.Link_Type)
      return access Private_Info_Access;

   package Receiving_Stub_Lists is new PolyORB.Utils.Ilists.Lists
     (Private_Info, Private_Info_Access, Doubly_Linked => False);

   All_Receiving_Stubs : Receiving_Stub_Lists.List;

   procedure Activate_RPC_Receiver (Default_Servant : Servant_Access);
   --  Activate one RPC receiver (i.e. enable the processing of incoming remote
   --  subprogram calls to that servant).

   ---------------
   -- Locations --
   ---------------

   use GNAT.Regpat;

   Loc_Regex   : constant String := "tcp://(.*):([0-9]*)";
   Loc_Matcher : constant Pattern_Matcher := Compile (Loc_Regex);
   subtype Loc_Matches is Match_Array (0 .. Paren_Count (Loc_Matcher));

   function Make_Corbaloc
     (Loc : String;
      Obj : String) return String;
   --  Convert a DSA location and object identifier to a corbaloc URL

   ---------------------------
   -- Activate_RPC_Receiver --
   ---------------------------

   procedure Activate_RPC_Receiver (Default_Servant : Servant_Access) is
      use PolyORB.Errors;
      use PolyORB.POA;
      use PolyORB.POA_Manager;

      POA   : constant Obj_Adapter_Access :=
        Obj_Adapter_Access (Default_Servant.Object_Adapter);
      Error : Error_Container;
   begin
      pragma Debug (C, O ("Activate_RPC_Receiver: "
                            & Default_Servant.Impl_Info.Name.all));

      Activate (POAManager_Access (Entity_Of (POA.POA_Manager)), Error);
      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Activate_RPC_Receiver;

   ----------------------------
   -- Activate_RPC_Receivers --
   ----------------------------

   procedure Activate_RPC_Receivers is
      use Receiving_Stub_Lists;
      It : Iterator;
   begin
      pragma Debug (C, O ("Activate_RPC_Receivers: enter"));
      It := First (All_Receiving_Stubs);
      while not Last (It) loop
         Activate_RPC_Receiver (Value (It).Receiver);
         Next (It);
      end loop;
      pragma Debug (C, O ("Activate_RPC_Receivers: end"));
   end Activate_RPC_Receivers;

   -------------------------
   -- Any_Aggregate_Build --
   -------------------------

   function Any_Aggregate_Build
     (TypeCode : PATC.Local_Ref;
      Contents : Any_Array) return Any
   is
      Result : Any := Get_Empty_Any_Aggregate (TypeCode);
   begin
      for J in Contents'Range loop
         Add_Aggregate_Element (Result, Contents (J));
      end loop;
      return Result;
   end Any_Aggregate_Build;

   ---------------------
   -- Any_Member_Type --
   ---------------------

   function Any_Member_Type
     (A     : Any;
      Index : U32) return PATC.Local_Ref
   is
   begin
      return PATC.To_Ref
        (PATC.Member_Type
          (Get_Unwound_Type (A), PolyORB.Types.Unsigned_Long (Index)));
   end Any_Member_Type;

   ---------------
   -- Any_To_BS --
   ---------------

   procedure Any_To_BS (Item : Any; Stream : out Buffer_Stream_Type) is
      use type PolyORB.Types.Unsigned_Long;

      AC  : Any_Container'Class renames Get_Container (Item).all;
      ACC : Aggregate_Content'Class renames
        Aggregate_Content'Class (Get_Value (AC).all);

      El_Count : constant PolyORB.Types.Unsigned_Long :=
        Get_Aggregate_Count (ACC);
      Data_Length  : constant Stream_Element_Count :=
        Stream_Element_Count (El_Count - 1);
      pragma Assert (El_Count - 1 = Get_Aggregate_Element (AC, 0));
      --  Note: for a sequence aggregate, the first aggregate element is the
      --  sequence length.

      Data_Address : System.Address := Unchecked_Get_V (ACC'Access);
   begin
      if Data_Address /= Null_Address then
         PolyORB.Buffers.Initialize_Buffer
           (Stream.Buf'Access,
            Data_Length,
            Data_Address,
            PolyORB.Buffers.Endianness_Type'First, --  XXX Irrelevant
            0);

      else
         --  Case of default aggregate contents: there is no materialized
         --  array of octets. Note, this is quite inefficient, instead
         --  PolyORB.Any.Get_Empty_Any_Aggregate should always make sure that
         --  any sequence<octet> contents uses the specific shadow any rather
         --  than the inefficient default aggregate contents. Or alternatively
         --  the default aggregate contents could be optimized for the case
         --  of components of an elementary type, and provide an actual
         --  content array in that case, accessable through Unchecked_Get_V.

         PolyORB.Buffers.Allocate_And_Insert_Cooked_Data
           (Stream.Buf'Access,
            Data_Length,
            Data_Address);

         declare
            Data : array (1 .. Data_Length) of PolyORB.Types.Octet;
            for Data'Address use Data_Address;
            pragma Import (Ada, Data);
         begin
            for J in Data'Range loop
               Data (J) := Get_Aggregate_Element
                             (AC, PolyORB.Types.Unsigned_Long (J));
            end loop;
         end;

         PolyORB.Buffers.Rewind (Stream.Buf'Access);
      end if;
   end Any_To_BS;

   ---------------
   -- BS_To_Any --
   ---------------

   procedure BS_To_Any (Stream : Buffer_Stream_Type; Item : out Any) is
      use Octet_Sequences;

      S : PolyORB.Opaque.Zone_Access :=
        new Stream_Element_Array'(PolyORB.Buffers.To_Stream_Element_Array
                                    (Stream.Buf));

      subtype OSEA_T is Element_Array (1 .. S'Length);
      OSEA_Addr : constant System.Address := S (S'First)'Address;
      OSEA : OSEA_T;
      for OSEA'Address use OSEA_Addr;
      pragma Import (Ada, OSEA);
   begin
      Item := Octet_Sequences_Helper.To_Any (To_Sequence (OSEA));
      PolyORB.Opaque.Free (S);
   end BS_To_Any;

   ---------------------------
   -- Build_Local_Reference --
   ---------------------------

   procedure Build_Local_Reference
     (Addr     : System.Address;
      Typ      : String;
      Receiver : access Servant;
      Ref      : out PolyORB.References.Ref)
   is
      use PolyORB.Errors;
      use type PolyORB.Obj_Adapters.Obj_Adapter_Access;

      Error : Error_Container;
   begin
      if Addr /= Null_Address then
         pragma Assert (Receiver.Object_Adapter /= null);

         declare
            Key   : aliased PolyORB.Objects.Object_Id := To_Local_Oid (Addr);
            U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;
            Oid   : PolyORB.POA_Types.Object_Id_Access;

         begin
            PolyORB.POA.Activate_Object
              (Self      => PolyORB.POA.Obj_Adapter_Access
                              (Receiver.Object_Adapter),
               P_Servant => null,
               Hint      => Key'Unchecked_Access,
               U_Oid     => U_Oid,
               Error     => Error);

            if Found (Error) then
               PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
            end if;

            Oid := PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);

            if Found (Error) then
               PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
            end if;

            PolyORB.ORB.Create_Reference
              (PolyORB.Setup.The_ORB,
               Oid, "DSA:" & Typ & ":1.0", Ref);

            PolyORB.Objects.Free (Oid);
         end;
      end if;
   exception
      when E : others =>
         pragma Debug
           (C, O ("Build_Local_Reference: got exception "
                 & Ada.Exceptions.Exception_Information (E)));
         pragma Debug (C, O ("returning a nil ref."));
         null;
   end Build_Local_Reference;

   ------------------------
   -- Caseless_String_Eq --
   ------------------------

   function Caseless_String_Eq (S1, S2 : String) return Boolean is
   begin
      return To_Lower (S1) = To_Lower (S2);
   end Caseless_String_Eq;

   -----------
   -- Check --
   -----------

   procedure Check
     (Name    : String;
      Version : String;
      RCI     : Boolean := True)
   is
      use Ada.Exceptions;
      Info : RCI_Info_Access;
   begin
      pragma Debug (C, O ("Check: checking versions consistency for " & Name));

      if not RCI then
         return;
      end if;

      Retrieve_RCI_Info (Name, Info);

      declare
         use Ada.Strings, Ada.Strings.Fixed;

         Type_Id        : constant String := Type_Id_Of (Info.Base_Ref);
         Last_Colon     : constant Natural := Index (Type_Id, ":", Backward);
         Actual_Version : String
           renames Type_Id (Last_Colon + 1 .. Type_Id'Last);
      begin
         if Version /= Type_Id (Last_Colon + 1 .. Type_Id'Last) then
            raise Program_Error
              with "Versions differ for unit """ & Name & """: expected "
                & Version & ", actual " & Actual_Version;
         end if;
      end;
   end Check;

   ---------------------
   -- Compare_Content --
   ---------------------

   function Compare_Content
     (Left, Right : RACW_Stub_Type_Access) return Boolean
   is
      use System.RPC;
      Left_Object, Right_Object : PolyORB.References.Ref;
   begin
      Set (Left_Object, Left.Target);
      Set (Right_Object, Right.Target);

      return Left /= null and then Right /= null
        and then PolyORB.References.Is_Equivalent (Left_Object, Right_Object);
   end Compare_Content;

   ----------------
   -- Create_Any --
   ----------------

   function Create_Any (TC : PATC.Local_Ref) return Any is
      use type PATC.Local_Ref;
   begin
      if Unwind_Typedefs (TC) = TC_Opaque then
         declare
            Empty_Seq : Octet_Sequences.Sequence;
         begin
            return Octet_Sequences_Helper.To_Any (Empty_Seq);
         end;
      else
         return Get_Empty_Any_Aggregate (TC);
      end if;
   end Create_Any;

   ------------
   -- Detach --
   ------------

   procedure Detach is
      procedure C_Detach;
      pragma Import (C, C_Detach, "__PolyORB_detach");
   begin
      C_Detach;
   end Detach;

   --------------------------
   -- DSA_Exception_To_Any --
   --------------------------

   function DSA_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence) return Any
   is
      use PolyORB.Errors;
      use PolyORB.Types;

      Name   : constant String := PolyORB.Exceptions.Occurrence_To_Name (E);
      TC     : constant PATC.Local_Ref := PATC.TCF_Except;
      Result : PolyORB.Any.Any;
   begin
      --  Name

      PATC.Add_Parameter (TC, To_Any (To_PolyORB_String (Name)));

      --  RepositoryId

      PATC.Add_Parameter
        (TC, To_Any (To_PolyORB_String
                       (PolyORB.DSA_P.Exceptions.Exception_Repository_Id
                          (Name, "1.0"))));

      --  Valuation: Exception_Message

      PATC.Add_Parameter (TC, To_Any (TC_String));
      PATC.Add_Parameter (TC,
        To_Any (To_PolyORB_String ("exception_message")));

      Result := Get_Empty_Any_Aggregate (TC);
      Add_Aggregate_Element
        (Result, To_Any
           (To_PolyORB_String (Ada.Exceptions.Exception_Message (E))));
      return Result;
   end DSA_Exception_To_Any;

   ---------------------
   -- Execute_Servant --
   ---------------------

   overriding function Execute_Servant
     (Self : not null access Servant;
      Req  : PolyORB.Requests.Request_Access) return Boolean
   is
   begin
      if Self.Impl_Info.Kind = Pkg_Stub then

         --  The base reference for an RCI unit implements operations
         --  that correspond to the visible subprograms of the unit
         --  (which are handled by Self.Handler).
         --  In addition, it implements the following special operations:

         --  XXX these hand-crafted skels should be generated by
         --  auto-generated ones constructed from a distributed object
         --  type declaration.

         if Req.Operation.all = Op_Resolve then

            -------------
            -- resolve --
            -------------

            --  Resolve the name of a remote subprogram declared in this
            --  remote call interface unit to the corresponding reference
            --  for the purpose of constructing a RAS value.

            --  Code extracted from CosNaming::NamingContext IDL skel.

            declare
               package ISNC renames
                 PolyORB.Services.Naming.SEQUENCE_NameComponent;

               n           : PolyORB.Services.Naming.Name;

               Arg_Name_n  : constant PolyORB.Types.Identifier :=
                 To_PolyORB_String ("n");
               Argument_n  : constant Any :=
                 Get_Empty_Any (PolyORB.Services.Naming.Helper.TC_Name);

               Result      : Object_Ref;
               Arg_List    : NVList_Ref;
            begin
               --  Create argument list

               NVList_Create (Arg_List);
               NVList_Add_Item
                 (Arg_List,
                  Arg_Name_n,
                  Argument_n,
                  ARG_IN);

               Request_Arguments (Req, Arg_List);

               --  Convert arguments from their Any

               n := PolyORB.Services.Naming.Helper.From_Any (Argument_n);

               --  Call implementation

               declare
                  Subprogram_Name : constant String :=
                    PolyORB.Services.Naming.To_Standard_String
                      (ISNC.Get_Element (ISNC.Sequence (n), 1).id);
               begin
                  if Subprogram_Name'Length > 0 then
                     Get_RAS_Info
                       (Self.Impl_Info.Name.all, Subprogram_Name, Result);
                  else
                     --  Special case: when looking up an empty name, just
                     --  return the RCI base ref.

                     Result := Self.Impl_Info.Base_Ref;
                  end if;
               end;

               --  Set Result

               Req.Result :=
                 (Name      => PolyORB.Types.To_PolyORB_String
                  ("result"),
                  Arg_Modes => ARG_OUT,
                  Argument  => PolyORB.Any.ObjRef.To_Any (Result));
            end;
            goto Request_Completed;

         elsif Req.Operation.all = Op_Get_Partition_Id then
            declare
               Arg_List    : NVList_Ref;
            begin

               -----------------------
               -- _get_partition_id --
               -----------------------

               --  Return the partition identifier assigned to the
               --  partition on which this RCI unit resides.

               NVList_Create (Arg_List);
               Request_Arguments (Req, Arg_List);

               --  Must call Arguments (with an empty Arg_List) to
               --  notify the protocol personality that this request has
               --  been completely received.

               Req.Result :=
                 (Name      => PolyORB.Types.To_PolyORB_String
                    ("result"),
                  Arg_Modes => ARG_OUT,
                  Argument  => Integer'To_Any
                                 (Integer (Get_Local_Partition_ID)));
               goto Request_Completed;
            end;
         end if;
      end if;

      --  User-defined subprogram: perform upcall to implementation

      --  Extract service context info used by the termination manager

      PolyORB.QoS.Term_Manager_Info.Extract_TM_Info (Req);

      pragma Assert (Self.Handler /= null);
      declare
         use PolyORB.Errors;
      begin
         Self.Handler.all (Req);
      exception
         when E : others =>
            --  Save exception occurrence in request

            Req.Exception_Info := DSA_Exception_To_Any (E);

            --  Also record additional exception information in optional
            --  service context.

            PolyORB.QoS.Exception_Informations.Set_Exception_Information
              (Req.all, E);
      end;

      <<Request_Completed>>
      return True;
   end Execute_Servant;

   -------------------------
   -- Extract_Union_Value --
   -------------------------

   function Extract_Union_Value (U : Any) return Any is
      U_Type : constant PATC.Local_Ref := Get_Type (U);
      Label_Any : constant Any :=
        PolyORB.Any.Get_Aggregate_Element
          (U, PATC.Discriminator_Type (U_Type), 0);
      Value_Type : constant PATC.Local_Ref :=
        PATC.Member_Type_With_Label (U_Type, Label_Any);
   begin
      return PolyORB.Any.Get_Aggregate_Element (U, Value_Type, 1);
   end Extract_Union_Value;

   --------------
   -- From_Any --
   --------------

   function FA_A (Item : PolyORB.Any.Any) return DSAT.Any_Container_Ptr is
      Item_ACP : constant PolyORB.Any.Any_Container_Ptr :=
        PolyORB.Any.Get_Container (PolyORB.Any.From_Any (Item));
      pragma Warnings (Off);
      --  No aliasing issues since DSAT.Any_Container_Ptr values are never
      --  dereferenced without first being converted back to
      --  PolyORB.Any.Any_Container_Ptr.

      function To_DSAT_ACP is new Ada.Unchecked_Conversion
        (PolyORB.Any.Any_Container_Ptr, DSAT.Any_Container_Ptr);
   begin
      return To_DSAT_ACP (Item_ACP);
   end FA_A;

   function FA_B (Item : PolyORB.Any.Any) return Boolean is
   begin
      return Boolean (PolyORB.Types.Boolean'(From_Any (Item)));
   end FA_B;

   function FA_C (Item : PolyORB.Any.Any) return Character is
   begin
      return Character (PolyORB.Types.Char'(From_Any (Item)));
   end FA_C;

   function FA_F (Item : PolyORB.Any.Any) return Float is
   begin
      return Float (PolyORB.Types.Float'(From_Any (Item)));
   end FA_F;

   function FA_I8 (Item : PolyORB.Any.Any) return I8 is
   begin
      return I8 (PolyORB.Types.Short'(From_Any (Item)));
   end FA_I8;

   function FA_I16 (Item : PolyORB.Any.Any) return I16 is
   begin
      return I16 (PolyORB.Types.Short'(From_Any (Item)));
   end FA_I16;

   function FA_I32 (Item : PolyORB.Any.Any) return I32 is
   begin
      return I32 (PolyORB.Types.Long'(From_Any (Item)));
   end FA_I32;

   function FA_I64 (Item : PolyORB.Any.Any) return I64 is
   begin
      return I64 (PolyORB.Types.Long_Long'(From_Any (Item)));
   end FA_I64;

   function FA_U8 (Item : PolyORB.Any.Any) return U8 is
   begin
      return U8 (PolyORB.Types.Octet'(From_Any (Item)));
   end FA_U8;

   function FA_U16 (Item : PolyORB.Any.Any) return U16 is
   begin
      return U16 (PolyORB.Types.Unsigned_Short'(From_Any (Item)));
   end FA_U16;

   function FA_U32 (Item : PolyORB.Any.Any) return U32 is
   begin
      return U32 (PolyORB.Types.Unsigned_Long'(From_Any (Item)));
   end FA_U32;

   function FA_U64 (Item : PolyORB.Any.Any) return U64 is
   begin
      return U64 (PolyORB.Types.Unsigned_Long_Long'(From_Any (Item)));
   end FA_U64;

   function FA_LF (Item : PolyORB.Any.Any) return Long_Float is
   begin
      return Long_Float (PolyORB.Types.Double'(From_Any (Item)));
   end FA_LF;

   function FA_LLF (Item : PolyORB.Any.Any) return Long_Long_Float is
   begin
      return Long_Long_Float (PolyORB.Types.Long_Double'(From_Any (Item)));
   end FA_LLF;

   function FA_SF (Item : PolyORB.Any.Any) return Short_Float is
   begin
      return Short_Float (PolyORB.Types.Float'(From_Any (Item)));
   end FA_SF;

   function FA_WC (Item : PolyORB.Any.Any) return Wide_Character is
   begin
      return Wide_Character (PolyORB.Types.Wchar'(From_Any (Item)));
   end FA_WC;

   function FA_String
     (Item : PolyORB.Any.Any) return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      return Ada.Strings.Unbounded.Unbounded_String
        (PolyORB.Types.String'(From_Any (Item)));
   end FA_String;

   ---------------
   -- Free_Stub --
   ---------------

   procedure Free_Stub (RACW : in out RACW_Stub_Type_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation
              (RACW_Stub_Type'Class, RACW_Stub_Type_Access);
      H_Entry : RACW_Stub_Type_Access;
   begin
      if RACW = null then
         return;
      end if;

      PTM.Enter (Critical_Section);

      --  Check that this RACW value is properly recorded in Objects_HTable

      H_Entry := Objects_HTable.Get (RACW);
      if H_Entry /= RACW then
         PTM.Leave (Critical_Section);
         raise Constraint_Error with "invalid RACW";
      end if;

      --  Proceed to remove it

      Objects_HTable.Remove (RACW);
      PTM.Leave (Critical_Section);

      --  We can now safely deallocate the stub

      PolyORB.Smart_Pointers.Dec_Usage (RACW.Target);
      Free (RACW);
   end Free_Stub;

   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   function Get_Aggregate_Element
     (Value : Any;
      TC    : PATC.Local_Ref;
      Index : U32) return Any
   is
   begin
      return PolyORB.Any.Get_Aggregate_Element
        (Value, TC, PolyORB.Types.Unsigned_Long (Index));
   end Get_Aggregate_Element;

   -----------------------------
   -- Get_Active_Partition_ID --
   -----------------------------

   function Get_Active_Partition_ID (Name : Unit_Name) return RPC.Partition_ID
   is
      Is_Local : constant Boolean := Get_DSA_Conf (RCI_Attr (Name, Partition))
                                       = Get_Local_Partition_Name;
      Info     : RCI_Info_Access;
   begin

      --  If the unit is local, we should return the partition_id of the local
      --  partition.
      --  To determine if the unit is local, we cannot use the
      --  RCI_Info.Is_Local attribute, since RCI_Info will only be available
      --  after the RCI receiving stub is registered (and this is not
      --  guaranteed to happen before a call to Get_Active_Partition_Id is
      --  issued).
      --  Thus we use the configuration parameters set up by po_gnatdist in the
      --  per-partition specific unit 'PolyORB.Parameters.Partition'.

      if Is_Local then
         return Get_Local_Partition_ID;
      end if;

      Retrieve_RCI_Info (Name, Info);

      if not Info.Known_Partition_ID then
         declare
            Request  : PolyORB.Requests.Request_Access;
            Arg_List : PolyORB.Any.NVList.Ref;
            Result   : PolyORB.Any.NamedValue;
         begin

            --  XXX This hand-crafted stub should be replaced with
            --  one automatically generated from a remote object type
            --  declaration.

            PolyORB.Any.NVList.Create (Arg_List);
            Result := (Name      => To_PolyORB_String ("result"),
                       Argument  => Get_Empty_Any (Integer'Typecode),
                       Arg_Modes => 0);

            PolyORB.Requests.Create_Request
              (Target    => Info.Base_Ref,
               Operation => Op_Get_Partition_Id,
               Arg_List  => Arg_List,
               Result    => Result,
               Req       => Request);

            PolyORB.Requests.Invoke (Request);
            PolyORB.Requests.Destroy_Request (Request);
            Info.Known_Partition_ID := True;
            Info.RCI_Partition_ID   :=
              RPC.Partition_ID (Integer'From_Any (Result.Argument));
         end;
      end if;
      pragma Assert (Info.Known_Partition_ID);
      return Info.RCI_Partition_ID;
   end Get_Active_Partition_ID;

   --------------
   -- Get_Conf --
   --------------

   overriding function Get_Conf
     (Source       : access DSA_Source;
      Section, Key : String) return String
   is
      pragma Unreferenced (Source);
      V : constant String_Ptr :=
        PUCFCT.Lookup (Conf_Table, Make_Global_Key (Section, Key), null);
   begin
      if V /= null then
         return V.all;
      else
         return "";
      end if;
   end Get_Conf;

   ------------------
   -- Get_DSA_Conf --
   ------------------

   function Get_DSA_Conf (Var : String) return String is
   begin
      return PolyORB.Parameters.Get_Conf ("dsa", Var);
   end Get_DSA_Conf;

   -----------------------
   -- Get_Local_Address --
   -----------------------

   procedure Get_Local_Address
     (Ref      : PolyORB.References.Ref;
      Is_Local : out Boolean;
      Addr     : out System.Address)
   is
      use PolyORB.Errors;

      Profiles : constant Profile_Array :=
        PolyORB.References.Profiles_Of (Ref);

      Error : Error_Container;

   begin
      for J in Profiles'Range loop
         if PolyORB.ORB.Is_Profile_Local
           (PolyORB.Setup.The_ORB,
            Profiles (J))
         then
            declare
               use PolyORB.Binding_Data;
               use PolyORB.Objects;
               Key : Object_Id_Access;
            begin
               PolyORB.Obj_Adapters.Object_Key
                 (OA      =>  PolyORB.ORB.Object_Adapter
                  (PolyORB.Setup.The_ORB),
                  Id      => PolyORB.Binding_Data.Get_Object_Key
                  (Profiles (J).all),
                  User_Id => Key,
                  Error   => Error);

               if not Found (Error) then
                  Is_Local := True;

                  if The_TM_Oid /= null and then
                    Get_Object_Key (Profiles (J).all).all = The_TM_Oid.all
                  then
                     --  Requests for the Termination Manager do not contain
                     --  the local memory address of the object (they are
                     --  performed through a reference created under a specific
                     --  POA and a dummy object key) so these have to be
                     --  handled specifically.

                     Addr := The_TM_Address;

                  elsif Key'Length = Local_Oid'Length then
                     Addr := To_Address (Key (Key'Range));

                  else
                     Addr := Null_Address;

                  end if;

                  PolyORB.Objects.Free (Key);
                  --  XXX not sure we can do that ...

                  return;
               elsif Error.Kind = Invalid_Object_Id_E then
                  Catch (Error);

                  --  This object identifier does not contain a user-assigned
                  --  object key.

               else
                  PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
               end if;
            end;
         end if;
      end loop;

      Is_Local := False;
      Addr := Null_Address;
   end Get_Local_Address;

   ------------
   -- Get_TC --
   ------------

   function Get_TC (A : Any) return PATC.Local_Ref is
   begin
      return PATC.To_Ref (PolyORB.Any.Get_Unwound_Type (A));
   end Get_TC;

   ----------
   -- Link --
   ----------

   function Link
     (X     : access RACW_Stub_Type'Class;
      Which : PolyORB.Utils.Ilists.Link_Type)
      return access RACW_Stub_Type_Access
   is
   begin
      return X.Notification_Links (Which)'Unchecked_Access;
   end Link;

   function Link
     (S     : access Private_Info;
      Which : PolyORB.Utils.Ilists.Link_Type)
      return access Private_Info_Access
   is
      use PolyORB.Utils.Ilists;
   begin
      pragma Assert (Which = Next);
      return S.Next'Unchecked_Access;
   end Link;

   -------------------------
   -- Local_PID_Allocated --
   -------------------------

   function Local_PID_Allocated return Boolean is
   begin
      return System.Standard_Library.Local_Partition_ID /= 0;
   end Local_PID_Allocated;

   ----------------------------
   -- Set_Local_Partition_ID --
   ----------------------------

   procedure Set_Local_Partition_ID (PID : RPC.Partition_ID) is
      use type RPC.Partition_ID;
   begin
      --  A PID of 0 denotes the unset (initial) state of
      --  System.Standard_Library.Local_Partition_ID.

      pragma Assert (PID /= 0);

      PTM.Enter (Critical_Section);

      if not Local_PID_Allocated then
         System.Standard_Library.Local_Partition_ID := Natural (PID);
         PTC.Broadcast (Local_PID_Barrier);

      else
         --  Should attempts to set the local PID twice be diagnosed???

         null;
      end if;

      PTM.Leave (Critical_Section);
   end Set_Local_Partition_ID;

   ----------------------------
   -- Get_Local_Partition_ID --
   ----------------------------

   function Get_Local_Partition_ID return RPC.Partition_ID is
   begin
      PTM.Enter (Critical_Section);
      if not Local_PID_Allocated then

         --  Wait until a partition identifier has been assigned to the
         --  local partition (this barrier is opened once System.DSA_Services
         --  is elaborated).

         PTC.Wait (Local_PID_Barrier, Critical_Section);
         pragma Assert (Local_PID_Allocated);
      end if;
      PTM.Leave (Critical_Section);

      return RPC.Partition_ID (System.Standard_Library.Local_Partition_ID);
   end Get_Local_Partition_ID;

   ------------------------------
   -- Get_Local_Partition_Name --
   ------------------------------

   function Get_Local_Partition_Name return String is
   begin
      return Local_Partition_Name.all;
   end Get_Local_Partition_Name;

   --------------------------------
   -- Get_Nested_Sequence_Length --
   --------------------------------

   function Get_Nested_Sequence_Length
     (Value : Any;
      Depth : Positive) return U32
   is
      use type PolyORB.Types.Unsigned_Long;

      Seq_Any : PolyORB.Any.Any;
      TC      : constant PATC.Object_Ptr := Get_Unwound_Type (Value);
   begin
      pragma Debug (C, O ("Get_Nested_Sequence_Length: enter,"
                       & " Depth =" & Depth'Img & ","
                       & " TC = " & Image (TC)));

      if PATC.Kind (TC) = Tk_Struct then
         declare
            Index : constant PolyORB.Types.Unsigned_Long :=
              PATC.Member_Count (TC) - 1;
         begin
            pragma Debug (C, O ("Index of last member is" & Index'Img));

            Seq_Any := Get_Aggregate_Element (Value,
                         PATC.Member_Type (TC, Index), Index);
         end;
      else
         pragma Debug (C, O ("TC is (assumed to be) a Tk_Sequence"));
         pragma Assert (PATC.Kind (TC) = Tk_Sequence);
         Seq_Any := Value;
      end if;

      declare
         use type U32;

         Outer_Length : constant U32 :=
           FA_U32 (PolyORB.Any.Get_Aggregate_Element (Seq_Any, TC_U32, 0));
      begin
         if Depth = 1 or else Outer_Length = 0 then
            return Outer_Length;
         else
            Seq_Any := PolyORB.Any.Get_Aggregate_Element
                         (Seq_Any, Content_Type (Get_Type (Seq_Any)), 1);
            return Get_Nested_Sequence_Length (Seq_Any, Depth - 1);
         end if;
      end;
   end Get_Nested_Sequence_Length;

   --------------
   -- Get_RACW --
   --------------

   function Get_RACW
     (Ref              : PolyORB.References.Ref;
      Stub_Tag         : Ada.Tags.Tag;
      Is_RAS           : Boolean;
      Asynchronous     : Boolean) return System.Address
   is
      Is_Local     : Boolean;
      Addr         : System.Address;

      Stub_Obj     : aliased RACW_Stub_Type;
      Stub_Acc     : RACW_Stub_Type_Access := Stub_Obj'Unchecked_Access;

      Stub_Obj_Tag : access Ada.Tags.Tag;

   begin
      --  Case of a nil reference: return a null address

      if Is_Nil (Ref) then
         return Null_Address;
      end if;

      Get_Local_Address (Ref, Is_Local, Addr);

      --  Local case: return address of local object

      if Is_Local then
         declare
            RAS_Proxy : RAS_Proxy_Type;
            for RAS_Proxy'Address use Addr;
            pragma Import (Ada, RAS_Proxy);
         begin
            if not (Is_RAS and then RAS_Proxy.All_Calls_Remote) then
               return Addr;
            end if;
         end;
      end if;

      --  Remote case: return address of stub

      Stub_Obj.Target := Entity_Of (Ref);
      Inc_Usage (Stub_Obj.Target);

      Stub_Obj.Asynchronous := Asynchronous;

      Get_Unique_Remote_Pointer (Stub_Acc);

      --  Fix up stub tag. This is safe because we carefully ensure that
      --  all stub types have the same layout as RACW_Stub_Type.

      declare
         CW_Stub_Obj : RACW_Stub_Type'Class
                         renames RACW_Stub_Type'Class (Stub_Acc.all);
         --  Class-wide view of stub object, to which 'Tag can be applied
      begin
         Stub_Obj_Tag := CW_Stub_Obj'Tag'Unrestricted_Access;
         Stub_Obj_Tag.all := Stub_Tag;
      end;

      return Stub_Acc.all'Address;
   end Get_RACW;

   function Resolve_RCI_Entity
     (Base_Ref : Object_Ref;
      Name     : String;
      Kind     : String) return Object_Ref;
   --  Resolve Name in the RCI package naming context Base_Ref. Name can
   --  be either the name of an RCI subprogram (kind SUBP, case of obtaining a
   --  reference for a RAS), or the empty string (kind RCI), in which case the
   --  reference of the base RCI object itself is returned. (Note: in the
   --  latter case, the returned ref is not necessarily identical to Base_Ref,
   --  in particular the returned ref has accurate version information that
   --  might not be present in Base_Ref).

   ------------------
   -- Get_RAS_Info --
   ------------------

   procedure Get_RAS_Info
     (Pkg_Name        :     String;
      Subprogram_Name :     String;
      Subp_Ref        : out Object_Ref)
   is
      Info : RCI_Info_Access;

   begin
      Retrieve_RCI_Info (Pkg_Name, Info);

      if Info.Is_Local then
         --  Retrieve subprogram address using subprogram name and subprogram
         --  table. Warning: the name used MUST be the distribution-name (with
         --  overload suffix, where appropriate.)

         declare
            use Receiving_Stub_Lists;
            It : Receiving_Stub_Lists.Iterator := First (All_Receiving_Stubs);

            Addr : System.Address := System.Null_Address;
            Receiver : Servant_Access := null;

         begin

            --  XXX
            --  The following is ugly and inefficient (two levels of linear
            --  search) and should probably be optimized in some way.

            pragma Debug (C, O ("Looking up RAS ref for " &
                               Subprogram_Name & " in " &
                               Pkg_Name));

            All_Stubs :
            while not Last (It) loop
               declare
                  RS : Private_Info renames Value (It).all;
                  pragma Assert (RS.Subp_Info /= Null_Address);

                  subtype Subp_Array is
                    RCI_Subp_Info_Array (0 .. RS.Subp_Info_Len - 1);

                  package Subp_Info_Addr_Conv is
                     new System.Address_To_Access_Conversions (Subp_Array);

                  Subp_Info : constant Subp_Info_Addr_Conv.Object_Pointer :=
                    Subp_Info_Addr_Conv.To_Pointer (RS.Subp_Info);
               begin
                  if RS.Kind = Pkg_Stub
                    and then To_Lower (RS.Name.all) = To_Lower (Pkg_Name)
                  then
                     for J in Subp_Info'Range loop
                        declare
                           Info : RCI_Subp_Info renames Subp_Info (J);
                           subtype Str is String (1 .. Info.Name_Length);

                           package Str_Addr_Conv is
                              new System.Address_To_Access_Conversions (Str);
                        begin
                           if Str_Addr_Conv.To_Pointer (Info.Name).all
                             = Subprogram_Name
                           then
                              Addr     := Info.Addr;
                              Receiver := RS.Receiver;
                              exit All_Stubs;
                           end if;
                        end;
                     end loop;
                  end if;
               end;
               Next (It);
            end loop All_Stubs;

            pragma Assert (Addr /= System.Null_Address);

            Build_Local_Reference (Addr, Pkg_Name, Receiver, Subp_Ref);
         end;

      else
         Subp_Ref :=
           Resolve_RCI_Entity (Info.Base_Ref, Subprogram_Name, "SUBP");
      end if;
   end Get_RAS_Info;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference
     (RACW             : System.Address;
      Type_Name        : String;
      Stub_Tag         : Ada.Tags.Tag;
      Is_RAS           : Boolean;
      Receiver         : access Servant) return PolyORB.References.Ref
   is
      RACW_Stub : RACW_Stub_Type;
      for RACW_Stub'Address use RACW;
      pragma Import (Ada, RACW_Stub);

      CW_RACW_Stub : RACW_Stub_Type'Class
                       renames RACW_Stub_Type'Class (RACW_Stub);

      use type Ada.Tags.Tag;

   begin
      --  Null case

      if RACW = System.Null_Address then
         --  Nothing to do, default initialization for Result is Nil

         return Nil_Ref;

      --  Case of a remote object

      elsif CW_RACW_Stub'Tag = Stub_Tag then
         return Make_Ref (RACW_Stub.Target);

      --  Case of a local object

      elsif Is_RAS then
         --  Remote access to subprogram: use ref from proxy

         declare
            RAS_Proxy : RAS_Proxy_Type;
            for RAS_Proxy'Address use RACW;
            pragma Import (Ada, RAS_Proxy);
         begin
            return Make_Ref (RAS_Proxy.Target);
         end;

      else
         --  Local object

         declare
            Result : PolyORB.References.Ref;
         begin
            Build_Local_Reference (RACW, Type_Name, Receiver, Result);
            return Result;
         end;
      end if;
   end Get_Reference;

   -------------------------------
   -- Get_Unique_Remote_Pointer --
   -------------------------------

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access)
   is
      Answer : RACW_Stub_Type_Access;
   begin
      PTM.Enter (Critical_Section);
      Answer := Objects_HTable.Get (Handler);
      if Answer = null then
         Answer := new RACW_Stub_Type;

         --  We leak memory here each time we receive a new unique value of a
         --  remote access to classwide or remote access to subprogram type.

         Answer.Target       := Handler.Target;
         Answer.Asynchronous := Handler.Asynchronous;

         Objects_HTable.Set (Answer, Answer);
      else
         PolyORB.Smart_Pointers.Dec_Usage (Handler.Target);
      end if;
      Handler := Answer;
      PTM.Leave (Critical_Section);
   end Get_Unique_Remote_Pointer;

   ----------
   -- Hash --
   ----------

   function Hash_String is new GNAT.HTable.Hash (Hash_Index);

   function Hash (K : RACW_Stub_Type_Access) return Hash_Index is
      K_Ref : PolyORB.References.Ref;
   begin
      Set (K_Ref, K.Target);
      return Hash_String (PolyORB.References.Image (K_Ref));
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      TC_Opaque_Cache := PATC.Build_Sequence_TC (TC_Octet, 0);
      Octet_Sequences_Helper.Initialize
        (Element_TC  => PolyORB.Any.TC_Octet,
         Sequence_TC => TC_Opaque_Cache);

      PTM.Create (Critical_Section);
      PTC.Create (Local_PID_Barrier);

      --  Get runtime parameters

      RPC_Timeout :=
        PolyORB.Parameters.Get_Conf
          (Section => "dsa",
           Key     => "rpc_timeout",
           Default => 0.0);

      Local_Partition_Name :=
        new String'(PolyORB.Parameters.Get_Conf
                      (Section => "dsa",
                       Key     => "partition_name",
                       Default => "NO NAME"));

      --  Set up DSA name server

      Initialize_Name_Server;
   end Initialize;

   ---------------------------
   -- Initialize_Parameters --
   ---------------------------

   procedure Initialize_Parameters is

      procedure Set_Conf (Section, Key, Value : String);
      --  Call back to set the given configuration parameter

      procedure Set_Location (Loc : String; Self : Boolean; Boot : Boolean);
      --  Parse location Loc and set is as the current partition's location
      --  (if Self is True), and/or as the boot partition location (if Boot is
      --  True).

      --------------
      -- Set_Conf --
      --------------

      procedure Set_Conf (Section, Key, Value : String) is
         LS : constant String := To_Lower (Section);
         LK : constant String := To_Lower (Key);
      begin
         pragma Debug
           (C, O ("Set_Conf: [" & Section & "] " & Key & " = " & Value));
         PUCFCT.Insert (Conf_Table, Make_Global_Key (Section, Key), +Value);
      end Set_Conf;

      ------------------
      -- Set_Location --
      ------------------

      procedure Set_Location (Loc : String; Self : Boolean; Boot : Boolean) is
         M : Loc_Matches;
      begin
         Match (Loc_Matcher, Loc, M);
         if M (M'First) = No_Match then
            return;
         end if;

         if Self then
            if Boot then

               --  Setting the main partition's self location using pragma
               --  Boot_Location: set port only (and bind on all addresses).
               --  Use Self_Location on main partition to force binding on a
               --  specific interface.

               Set_Conf ("iiop", "polyorb.protocols.iiop.default_port",
                         Loc (M (2).First .. M (2).Last));
            else
               --  Explicit pragma Self_Location: set full location (host and
               --  port).

               Set_Conf ("iiop", "polyorb.protocols.iiop.default_addr",
                         Loc (M (1).First .. M (1).Last)
                         & ":"
                         & Loc (M (2).First .. M (2).Last));
            end if;
         end if;

         if Boot then
            Set_Conf ("dsa", "name_service",
                      Make_Corbaloc (Loc, "_NameService"));
         end if;
      end Set_Location;

   --  Start of processing for Initialize_Parameters

   begin
      --  Set up parameter source for values provided by gnatdist

      PUCFCT.Initialize (Conf_Table);
      PolyORB.Partition_Elaboration.Configure (Set_Conf'Access);
      PolyORB.Parameters.Register_Source (The_DSA_Source'Access);

      --  Perform additional automated configuration

      declare
         use PolyORB.Parameters;

         Is_Main_Partition : constant Boolean :=
           Get_Conf ("dsa", "main_partition", False);
         Has_Embedded_Nameserver : constant Boolean :=
           Ada.Characters.Handling.To_Upper (Get_DSA_Conf ("name_server_kind"))
             = "EMBEDDED";
         Boot_Loc : constant String := Get_DSA_Conf ("boot_location");
         Self_Loc : constant String := Get_DSA_Conf ("self_location");
      begin

         if Boot_Loc /= "" then
            Set_Location (Boot_Loc,
              Self => Is_Main_Partition and then Has_Embedded_Nameserver,
              Boot => True);
         end if;

         if Self_Loc /= "" then
            Set_Location (Self_Loc, Self => True, Boot => False);
         end if;
      end;
   end Initialize_Parameters;

   -------------------
   -- Make_Corbaloc --
   -------------------

   function Make_Corbaloc
     (Loc : String;
      Obj : String) return String
   is
      M : Loc_Matches;
   begin
      pragma Debug (C,
        O ("Make_Corbaloc: enter, object " & Obj & " at " & Loc));

      Match (Loc_Matcher, Loc, M);
      if M (M'First) = No_Match then
         pragma Debug (C, O ("Make_Corbaloc: leave, invalid loc"));
         return "";
      end if;

      return
        "corbaloc:iiop:"
          & Loc (M (1).First .. M (1).Last)
          & ":"
          & Loc (M (2).First .. M (2).Last)
          & "/" & Obj;
   end Make_Corbaloc;

   --------------
   -- Make_Ref --
   --------------

   function Make_Ref (The_Entity : PolyORB.Smart_Pointers.Entity_Ptr)
     return PolyORB.References.Ref
   is
      Result : PolyORB.References.Ref;
   begin
      Set_Ref (Result, The_Entity);
      return Result;
   end Make_Ref;

   -------------------------------------
   -- Raise_Program_Error_Unknown_Tag --
   -------------------------------------

   procedure Raise_Program_Error_Unknown_Tag
     (E : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Ada.Exceptions.Raise_Exception
        (Program_Error'Identity, Ada.Exceptions.Exception_Message (E));
   end Raise_Program_Error_Unknown_Tag;

   -----------------
   -- RCI_Locator --
   -----------------

   package body RCI_Locator is

      Info : RCI_Info_Access;
      --  Cached access to RCI_Info to avoid extra hash table lookups on
      --  subsequent calls.

      -------------------------
      -- Get_RCI_Package_Ref --
      -------------------------

      function Get_RCI_Package_Ref return Object_Ref is
      begin
         Retrieve_RCI_Info (RCI_Name, Info);

         --  In case of failure to obtain a valid reference, Retrieve_RCI_Info
         --  raises Communication_Error, so here we know we have one.

         pragma Assert (not Is_Nil (Info.Base_Ref));
         return Info.Base_Ref;
      end Get_RCI_Package_Ref;
   end RCI_Locator;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Buffer_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      use PolyORB.Buffers;

      Transfer_Length : constant Stream_Element_Count :=
        Stream_Element_Count'Min
          (Remaining (Stream.Buf'Access), Item'Length);
      Data : PolyORB.Opaque.Opaque_Pointer;
   begin
      Extract_Data (Stream.Buf'Access, Data, Transfer_Length);
      Last := Item'First + Transfer_Length - 1;
      declare
         Z_Addr : constant System.Address := Data;
         Z : Stream_Element_Array (Item'First .. Last);
         for Z'Address use Z_Addr;
         pragma Import (Ada, Z);
      begin
         Item (Item'First .. Last) := Z;
      end;
   end Read;

   ---------------------------------
   -- Register_Obj_Receiving_Stub --
   ---------------------------------

   procedure Register_Obj_Receiving_Stub
     (Name     : String;
      Handler  : Request_Handler_Access;
      Receiver : Servant_Access)
   is
      use Receiving_Stub_Lists;
      Stub : Private_Info renames Receiver.Impl_Info;
   begin
      Receiver.Handler := Handler;

      Stub :=
        (Kind                => Obj_Stub,
         Name                => +Name (Name'First .. Name'Last),
         Receiver            => Receiver,
         Version             => null,
         Subp_Info           => Null_Address,
         Subp_Info_Len       => 0,
         Is_All_Calls_Remote => False,
         others              => <>);
      Prepend (All_Receiving_Stubs, Stub'Access);

      pragma Debug (C, O ("Setting up RPC receiver: " & Stub.Name.all));
      Setup_Object_RPC_Receiver (Stub.Name.all, Stub.Receiver);
   end Register_Obj_Receiving_Stub;

   ------------------------
   -- Resolve_RCI_Entity --
   ------------------------

   function Resolve_RCI_Entity
     (Base_Ref : Object_Ref;
      Name     : String;
      Kind     : String) return Object_Ref
   is
      Ctx_Ref : PSNNC.Ref;
   begin
      PSNNC.Set (Ctx_Ref, Entity_Of (Base_Ref));
      return PSNNC.Client.Resolve (Ctx_Ref, To_Name (Name, Kind));
   exception
      when others =>
         return Nil_Ref;
   end Resolve_RCI_Entity;

   -------------------------
   -- Find_Receiving_Stub --
   -------------------------

   function Find_Receiving_Stub
     (Name : String; Kind : Receiving_Stub_Kind) return Servant_Access
   is
      use Receiving_Stub_Lists;
      It : Receiving_Stub_Lists.Iterator := First (All_Receiving_Stubs);
   begin
      All_Stubs :
      while not Last (It) loop
         declare
            RS : Private_Info renames Value (It).all;
         begin
            if RS.Kind = Kind
              and then To_Lower (RS.Name.all) = To_Lower (Name)
            then
               return RS.Receiver;
            end if;
         end;
         Next (It);
      end loop All_Stubs;

      return null;
   end Find_Receiving_Stub;

   --------------
   -- DSA_Attr --
   --------------

   function DSA_Attr (Name : String; Attr : DSA_Attribute) return String is
   begin
      return To_Lower (Name & "'" & Attr'Img);
   end DSA_Attr;

   ------------------------------
   -- Register_Passive_Package --
   ------------------------------

   procedure Register_Passive_Package
     (Name    : Unit_Name;
      Version : String := "")
   is
      pragma Unreferenced (Name);
      pragma Unreferenced (Version);
   begin
      null;
   end Register_Passive_Package;

   ---------------------------------
   -- Register_Pkg_Receiving_Stub --
   ---------------------------------

   procedure Register_Pkg_Receiving_Stub
     (Name                : String;
      Version             : String;
      Handler             : Request_Handler_Access;
      Receiver            : Servant_Access;
      Subp_Info           : System.Address;
      Subp_Info_Len       : Integer;
      Is_All_Calls_Remote : Boolean)
   is
      use Receiving_Stub_Lists;
      Stub : Private_Info renames Receiver.Impl_Info;
   begin
      Receiver.Handler := Handler;
      Receiver.Impl_Info :=
        (Kind                => Pkg_Stub,
         Name                => +Name,
         Receiver            => Receiver,
         Version             => +Version,
         Subp_Info           => Subp_Info,
         Subp_Info_Len       => Subp_Info_Len,
         Is_All_Calls_Remote => Is_All_Calls_Remote,
         others              => <>);
      Prepend (All_Receiving_Stubs, Stub'Access);

      declare
         use PolyORB.Errors;
         use PolyORB.ORB;
         use PolyORB.Obj_Adapters;
         use PolyORB.Setup;

         use type PolyORB.POA.Obj_Adapter_Access;

         Error : Error_Container;
         Key : aliased PolyORB.Objects.Object_Id :=
           To_Local_Oid (System.Null_Address);

         U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;
         Oid : PolyORB.POA_Types.Object_Id_Access;

      begin
         pragma Debug (C, O ("Setting up RPC receiver: " & Stub.Name.all));

         --  Establish a child POA for this stub. For RACWs, this POA will
         --  serve all objects of the same type. For RCIs, this POA will serve
         --  the base object corresponding to the RCI, as well as the
         --  sub-objects corresponding to each subprogram considered as an
         --  object (for RAS).

         Setup_Object_RPC_Receiver (Stub.Name.all, Stub.Receiver);

         PolyORB.POA.Activate_Object
           (Self      => PolyORB.POA.Obj_Adapter_Access
                           (Servant_Access (Stub.Receiver).Object_Adapter),
            P_Servant => null,
            Hint      => Key'Unchecked_Access,
            U_Oid     => U_Oid,
            Error     => Error);

         if Found (Error) then
            PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
         end if;

         Oid := PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);
         Create_Reference
           (The_ORB,
            Oid,
            "DSA:" & Stub.Name.all & ":" & Stub.Version.all,
            Receiver.Impl_Info.Base_Ref);

         PolyORB.Objects.Free (Oid);

         pragma Debug (C, O ("Registering local RCI: " & Stub.Name.all));

         Known_RCIs.Register
           (To_Lower (Stub.Name.all),
            new RCI_Info'
              (Base_Ref            => Receiver.Impl_Info.Base_Ref,
               Is_Local            => True,
               Reconnection_Policy => Default_Reconnection_Policy,
               State               => Live,
               Is_All_Calls_Remote => Stub.Is_All_Calls_Remote,
               Known_Partition_ID  => False,
               RCI_Partition_ID    => RPC.Partition_ID'First,
               others              => <>));

         Nameserver_Register
           (Name_Ctx => Get_Name_Server,
            Name     => To_Lower (Stub.Name.all),
            Kind     => "RCI",
            Obj      => Receiver.Impl_Info.Base_Ref);
      end;

   exception

      --  An exception occurring during registration of an RCI is fatal to
      --  the application: terminate PCS and propagate.

      when E : others =>
         O ("Cannot register information for RCI "
             & Name & " with name server.", PL.Error);
         pragma Debug (C, O ("exception raised: "
                             & Ada.Exceptions.Exception_Information (E)));
         PolyORB.Initialization.Shutdown_World (Wait_For_Completion => False);
         raise;
   end Register_Pkg_Receiving_Stub;

   ----------------------------------
   -- Register_Termination_Manager --
   ----------------------------------

   procedure Register_Termination_Manager
     (Ref      : PolyORB.References.Ref;
      Oid      : PolyORB.Objects.Object_Id_Access;
      Address  : System.Address;
      Shutdown : PolyORB.Initialization.Finalizer)
   is
   begin
      The_TM_Ref      := Ref;
      The_TM_Oid      := Oid;
      The_TM_Address  := Address;
      The_TM_Shutdown := Shutdown;
      pragma Debug (C, O ("Registered the termination manager"));
   end Register_Termination_Manager;

   -----------------------
   -- Request_Arguments --
   -----------------------

   procedure Request_Arguments
     (R     :        PolyORB.Requests.Request_Access;
      Args  : in out PolyORB.Any.NVList.Ref)
   is
      Error : PolyORB.Errors.Error_Container;
   begin
      PolyORB.Requests.Arguments (R, Args, Error);
      if PolyORB.Errors.Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Request_Arguments;

   ------------------------------
   -- Request_Raise_Occurrence --
   ------------------------------

   procedure Request_Raise_Occurrence (R : Request) is
      use Ada.Exceptions;
      use PolyORB.DSA_P.Exceptions;
      use PolyORB.Exceptions;
   begin
      if not Is_Empty (R.Exception_Info) then
         declare
            E   : constant PolyORB.Any.Any := R.Exception_Info;
            Msg : constant String :=
              PolyORB.QoS.Exception_Informations.Get_Exception_Message (R);
         begin
            Raise_From_Any (E, Msg);
         end;
      end if;
   end Request_Raise_Occurrence;

   ----------------------------------
   -- Register_RACW_In_Name_Server --
   ----------------------------------

   procedure Register_RACW_In_Name_Server
     (Addr     : System.Address;
      Type_Tag : Ada.Tags.Tag;
      Name     : String;
      Kind     : String)
   is
      use type PolyORB.Obj_Adapters.Obj_Adapter_Access;
      use PolyORB.Errors;
      use PolyORB.ORB;
      use PolyORB.Obj_Adapters;
      use PolyORB.Setup;

      Key   : aliased PolyORB.Objects.Object_Id := To_Local_Oid (Addr);
      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;
      Oid   : PolyORB.POA_Types.Object_Id_Access;
      Error : Error_Container;
      Ref   : PolyORB.References.Ref;
      Receiver : System.Partition_Interface.Servant_Access;

   begin
      pragma Debug (C, O ("Register RACW In Name Server: enter"));
      Receiver := Find_Receiving_Stub
        (Ada.Tags.External_Tag (Type_Tag), Obj_Stub);

      PolyORB.POA.Activate_Object
        (Self      => PolyORB.POA.Obj_Adapter_Access
           (Receiver.Object_Adapter),
         P_Servant => null,
         Hint      => Key'Unchecked_Access,
         U_Oid     => U_Oid,
         Error     => Error);

      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;

      Oid := PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);
      PolyORB.ORB.Create_Reference
        (ORB => The_ORB,
         Oid => Oid,
         Typ => "DSA:" & Name,
         Ref => Ref);

      PolyORB.Objects.Free (Oid);

      Nameserver_Register
        (Name_Ctx => Get_Name_Server,
         Name     => To_Lower (Name),
         Kind     => Kind,
         Obj      => Ref);

      pragma Debug (C, O ("Register RACW In Name Server: leave"));
   end Register_RACW_In_Name_Server;

   --------------------
   -- Release_Buffer --
   --------------------

   procedure Release_Buffer (Stream : in out Buffer_Stream_Type) is
   begin
      PolyORB.Buffers.Release_Contents (Stream.Buf);
   end Release_Buffer;

   ---------------------
   -- Request_Set_Out --
   ---------------------

   procedure Request_Set_Out
     (R     : PolyORB.Requests.Request_Access)
   is
      Error : PolyORB.Errors.Error_Container;
   begin
      PolyORB.Requests.Set_Out_Args (R, Error);
      if PolyORB.Errors.Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Request_Set_Out;

   --------------------
   -- Request_Invoke --
   --------------------

   procedure Request_Invoke
     (R            : access PolyORB.Requests.Request;
      Invoke_Flags : PolyORB.Requests.Flags := 0)
   is
      use PolyORB.QoS;
      use PolyORB.QoS.Term_Manager_Info;
      use PolyORB.Request_QoS;
      use PolyORB.Termination_Activity;
   begin
      Increment_Activity;

      Add_Request_QoS
        (R.all,
         DSA_TM_Info,
         new QoS_DSA_TM_Info_Parameter'
               (Kind   => DSA_TM_Info,
                TM_Ref => The_TM_Ref));

      PolyORB.Requests.Invoke (R, Invoke_Flags, Timeout => RPC_Timeout);
   end Request_Invoke;

   -----------------------
   -- Retrieve_RCI_Info --
   -----------------------

   type Lookup_Witness (Info : access RCI_Info; SL : access PTM.Scope_Lock) is
     new Ada.Finalization.Limited_Controlled with
   record
      Set_State : RCI_State;
      --  State to be set upon lookup completion

      LU_Ref : Ref;
      --  Reference from name service lookup
   end record;

   overriding procedure Initialize (W : in out Lookup_Witness);
   overriding procedure Finalize (W : in out Lookup_Witness);

   procedure Initialize (W : in out Lookup_Witness) is
   begin
      pragma Assert (W.Info.State /= Pending);
      pragma Debug
        (C, O ("Lookup_Witness: initialize, saved state " & W.Info.State'Img));
      W.Set_State  := Invalid;
      W.Info.State := Pending;
      W.SL.Leave;
   end Initialize;

   procedure Finalize (W : in out Lookup_Witness) is
   begin
      W.SL.Enter;
      pragma Assert (W.Info.State = Pending);

      pragma Debug
        (C, O ("Lookup_Witness: finalize, set state to " & W.Set_State'Img));
      W.Info.State := W.Set_State;
      W.Info.Base_Ref.Set (W.LU_Ref.Entity_Of);
      PTCV.Broadcast (W.Info.Lookup_Done);
   end Finalize;

   procedure Retrieve_RCI_Info
     (Name : String;
      Info : in out RCI_Info_Access)
   is
      LName     : constant String := To_Lower (Name);
      SL        : aliased PTM.Scope_Lock (Critical_Section);

      Do_Lookup : Boolean := True;
      --  Set false if no lookup required

   begin
      pragma Debug (C, O ("Retrieve_RCI_Info: enter, Name = " & Name));

      if Info = null then
         Info := Known_RCIs.Lookup (LName, null);
      end if;

      if Info = null then
         --  Here for a new remote RCI

         Info := new RCI_Info'
           (Is_Local            => False,
            Reconnection_Policy => Get_Reconnection_Policy (Name),
            State               => Initial,
            Is_All_Calls_Remote => True,
            Known_Partition_ID  => False,
            RCI_Partition_ID    => RPC.Partition_ID'First,
            others              => <>);
         PTCV.Create (Info.Lookup_Done);
         Known_RCIs.Register (LName, Info);
      end if;

      <<Again>>

      --  If another task is taking care of the lookup, and ends up setting
      --  the ref to Live, do not bother doing a lookup ourselves.

      while Info.State = Pending loop
         pragma Debug (C, O ("waiting on pending nameserver lookup"));
         PTCV.Wait (Info.Lookup_Done, SL'Access);
         pragma Debug (C, O ("returned, state now " & Info.State'Img));

         if Info.State = Live then
            Do_Lookup := False;
         end if;
      end loop;

      --  No lookup if RCI is definitely dead

      if Info.State = Dead then
         Do_Lookup := False;
      end if;

      --  If RCI is Live, check reference validity

      if Do_Lookup
        and then Info.State = Live
      then
         if Is_Reference_Valid (Info.Base_Ref) then
            Do_Lookup := False;
         else
            Info.Base_Ref.Release;
            Info.State := Invalid;
         end if;
      end if;

      pragma Debug (C, O ("State  = " & Info.State'Img));
      pragma Debug (C, O ("Nil?   = " & Boolean'Image (Info.Base_Ref.Is_Nil)));
      pragma Debug (C, O ("Lookup = " & Boolean'Image (Do_Lookup)));

      --  If RCI information is not available locally, we request it from the
      --  name server. Since some partitions might not be registered yet, we
      --  retry the query up to Max_Requests times.

      if Do_Lookup then
         declare
            Is_Initial : constant Boolean := Info.State = Initial;

            --  Initialize lookup witness object: set Info.State to Pending,
            --  and leave critical section.

            LW : Lookup_Witness (Info, SL'Access);
            pragma Unreferenced (LW);

            Loc : constant String :=
              Get_DSA_Conf
                (Partition_Attr
                     (Get_DSA_Conf (RCI_Attr (LName, Partition)), Location));

         begin
            --  If we have a location from configuration, use it

            if Loc /= "" then
               pragma Debug (C, O ("Configured location: " & Loc));
               declare
                  Typed_Base_Ref : PolyORB.References.Ref;
                  --  Object reference with type id, returned by the RCI
                  --  unit, needed for the RCI version check.

               begin
                  --  Note: the object key for an RCI root object is the
                  --  RCI name in uppercase.

                  String_To_Object
                    (Make_Corbaloc (Loc,
                     Ada.Characters.Handling.To_Upper (Name)),
                     LW.LU_Ref);

                  --  Now we can contact the RCI base object to obtain
                  --  its actual reference with a proper type id, which
                  --  we can use to check the version

                  for Retry in 1 .. Max_Requests loop
                     Typed_Base_Ref := Resolve_RCI_Entity
                       (Base_Ref => LW.LU_Ref,
                        Name     => "",
                        Kind     => "RCI");
                     exit when not Typed_Base_Ref.Is_Nil;

                     if Retry < Max_Requests then
                        PTT.Relative_Delay (Time_Between_Requests);
                     end if;
                  end loop;

                  --  We want to keep the original Base_Ref (because it
                  --  may have different profiles than the one returned
                  --  by the RCI), but we propagate the type information
                  --  for the benefit of the RCI version check.

                  Set_Type_Id (LW.LU_Ref, Type_Id_Of (Typed_Base_Ref));
               end;

            --  Else do a name server lookup

            else
               LW.LU_Ref := Nameserver_Lookup
                              (Get_Name_Server,
                               LName, "RCI",
                               Initial => Is_Initial);
            end if;

            --  Update state if lookup was succesful

            if not LW.LU_Ref.Is_Nil then
               LW.Set_State := Live;
            end if;

         --  Upon exiting this block, we are re-entering the critical section,
         --  and setting RCI state according to the information in LW.

         end;
      end if;

      if Info.Base_Ref.Is_Nil then
         case Info.Reconnection_Policy is
            when Reject_On_Restart =>
               Info.State := Dead;

            when Block_Until_Restart =>
               PTM.Leave (SL'Access);
               PTT.Relative_Delay (Time_Between_Requests);
               PTM.Enter (SL'Access);
               goto Again;

            when Fail_Until_Restart =>
               Info.State := Invalid;
         end case;

         raise System.RPC.Communication_Error
           with "unable to locate RCI " & Name;
      end if;

      pragma Debug (C, O ("Retrieve_RCI_Info: leave"));
   end Retrieve_RCI_Info;

   ------------------------------------
   -- Retrieve_RACW_From_Name_Server --
   ------------------------------------

   procedure Retrieve_RACW_From_Name_Server
     (Name     : String;
      Kind     : String;
      Stub_Tag : Ada.Tags.Tag;
      Addr     : out System.Address)
   is
      Reg_Obj     : PolyORB.References.Ref;

   begin
      pragma Debug (C, O ("Retrieve RACW From Name Server: enter"));
      Reg_Obj := Nameserver_Lookup (Get_Name_Server, Name, Kind);
      Addr := Get_RACW
        (Ref          => Reg_Obj,
         Stub_Tag     => Stub_Tag,
         Is_RAS       => False,
         Asynchronous => True);
      pragma Debug (C, O ("Retrieve RACW From Name Server: leave"));
   end Retrieve_RACW_From_Name_Server;

   --------------------
   -- Same_Partition --
   --------------------

   function Same_Partition
     (Left  : access RACW_Stub_Type'Class;
      Right : access RACW_Stub_Type'Class) return Boolean
   is
   begin
      return Same_Node (Make_Ref (Left.Target), Make_Ref (Right.Target));
   end Same_Partition;

   -------------------------------
   -- Setup_Object_RPC_Receiver --
   -------------------------------

   procedure Setup_Object_RPC_Receiver
     (Name            : String;
      Default_Servant : Servant_Access)
   is
      use PolyORB.Errors;
      use PolyORB.POA;
      use PolyORB.POA_Config;
      use PolyORB.POA_Config.RACWs;
      use type PolyORB.Obj_Adapters.Obj_Adapter_Access;

      POA   : Obj_Adapter_Access;
      PName : constant PolyORB.Types.String :=
        PolyORB.Types.String (To_PolyORB_String (Name));
      Error : Error_Container;
   begin
      --  NOTE: Actually this does more than set up an RPC receiver. A TypeCode
      --  corresponding to the RACW is also constructed (and this is vital also
      --  on the client side).

      Default_Servant.Obj_TypeCode := PATC.TCF_Object;
      PATC.Add_Parameter
        (Default_Servant.Obj_TypeCode, To_Any (PName));
      PATC.Add_Parameter
        (Default_Servant.Obj_TypeCode, TA_Std_String ("DSA:" & Name & ":1.0"));

      if RACW_POA_Config = null then
         return;
      end if;

      Create_POA
        (Self         => Obj_Adapter_Access
                           (PolyORB.ORB.Object_Adapter
                              (PolyORB.Setup.The_ORB)),
         Adapter_Name => Name,
         A_POAManager => null,
         Policies     => Default_Policies (RACW_POA_Config.all),
         POA          => POA,
         Error        => Error);

      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;

      POA.Default_Servant := PolyORB.Servants.Servant_Access (Default_Servant);

      Default_Servant.Object_Adapter :=
        PolyORB.Obj_Adapters.Obj_Adapter_Access (POA);

      --  RPC receiver will be activated once the partition is completely
      --  elaborated.

   end Setup_Object_RPC_Receiver;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Wait_For_Completion : Boolean) is
      use type PolyORB.Initialization.Finalizer;
   begin
      --  Shut down the local termination manager, if it has already been
      --  registered.

      if The_TM_Shutdown /= null then
         The_TM_Shutdown (Wait_For_Completion);
      end if;
   end Shutdown;

   ---------------
   -- TC_Opaque --
   ---------------

   function TC_Opaque return PATC.Local_Ref is
   begin
      return TC_Opaque_Cache;
   end TC_Opaque;

   ------------
   -- To_Any --
   ------------

   function TA_A (Item : DSAT.Any_Container_Ptr) return PolyORB.Any.Any is
      pragma Warnings (Off);
      --  No aliasing issues since DSAT.Any_Container_Ptr always originally
      --  comes from a PolyORB.Any.Any_Container_Ptr.

      function To_PolyORB_ACP is new Ada.Unchecked_Conversion
        (DSAT.Any_Container_Ptr, PolyORB.Any.Any_Container_Ptr);
      pragma Warnings (On);
      Item_A : PolyORB.Any.Any;
   begin
      PolyORB.Any.Set_Container (Item_A, To_PolyORB_ACP (Item));
      return PolyORB.Any.To_Any (Item_A);
   end TA_A;

   function TA_B (Item : Boolean) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Boolean (Item));
   end TA_B;

   function TA_C (Item : Character) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Char (Item));
   end TA_C;

   function TA_F (Item : Float) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Float (Item));
   end TA_F;

   function TA_I8 (Item : I8) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Short (Item));
   end TA_I8;

   function TA_I16 (Item : I16) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Short (Item));
   end TA_I16;

   function TA_I32 (Item : I32) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Long (Item));
   end TA_I32;

   function TA_I64 (Item : I64) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Long_Long (Item));
   end TA_I64;

   function TA_U8 (Item : U8) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Octet (Item));
   end TA_U8;

   function TA_U16 (Item : U16) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Unsigned_Short (Item));
   end TA_U16;

   function TA_U32 (Item : U32) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Unsigned_Long (Item));
   end TA_U32;

   function TA_U64 (Item : U64) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Unsigned_Long_Long (Item));
   end TA_U64;

   function TA_LF (Item : Long_Float) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Double (Item));
   end TA_LF;

   function TA_LLF (Item : Long_Long_Float) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Long_Double (Item));
   end TA_LLF;

   function TA_SF (Item : Short_Float) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Float (Item));
   end TA_SF;

   function TA_WC (Item : Wide_Character) return PolyORB.Any.Any is
   begin
      return To_Any (PolyORB.Types.Wchar (Item));
   end TA_WC;

   function TA_String
     (S : Ada.Strings.Unbounded.Unbounded_String) return PolyORB.Any.Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.String (S));
   end TA_String;

   function TA_Std_String (S : String) return PolyORB.Any.Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.To_PolyORB_String (S));
   end TA_Std_String;

   -------------
   -- To_Name --
   -------------

   function To_Name (Id, Kind : String) return PolyORB.Services.Naming.Name is
      use PolyORB.Services.Naming.SEQUENCE_NameComponent;
   begin
      return PolyORB.Services.Naming.Name
        (To_Sequence
         ((1 => (id   => PolyORB.Services.Naming.To_PolyORB_String (Id),
                 kind => PolyORB.Services.Naming.To_PolyORB_String (Kind)))));
   end To_Name;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Buffer_Stream_Type;
      Item   : Stream_Element_Array)
   is
      use PolyORB.Buffers;

      Data : PolyORB.Opaque.Opaque_Pointer;
   begin
      Allocate_And_Insert_Cooked_Data
        (Stream.Buf'Access, Item'Length, Data);
      declare
         Z_Addr : constant System.Address := Data;
         Z : Stream_Element_Array (Item'Range);
         for Z'Address use Z_Addr;
         pragma Import (Ada, Z);
      begin
         Z := Item;
      end;
   end Write;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings.Lists;

begin
   --  The DSA runtime parameters depend on parameters_sources.runtime so that
   --  they can be overridden at run time.

   Register_Module
     (Module_Info'
      (Name      => +"parameters.dsa",
       Conflicts => Empty,
       Depends   => +"parameters_sources.runtime?",
       Provides  => +"parameters_sources",
       Implicit  => True,
       Init      => Initialize_Parameters'Access,
       Shutdown  => null));

   Register_Module
     (Module_Info'
      (Name      => +"dsa",
       Conflicts => Empty,
       Depends   => +"orb"
       & "parameters"
       & "poa"
       & "poa_config.racws"
       & "object_adapter"
       & "naming.Helper"
       & "naming.NamingContext.Helper"
       & "dsa.name_server?"
       & "tasking.condition_variables"
       & "tasking.mutexes"
       & "access_points?"
       & "binding_factories"
       & "references"
       & "request_qos.dsa_tm_info"
       & "termination.activity",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => Shutdown'Access));

   --  We initialize PolyORB, so that once s-parint is elaborated, the PCS is
   --  up and running, ready to process RPCs.

   Initialize_World;

   --  Run additional tasks if needed

   PolyORB.Partition_Elaboration.Run_Additional_Tasks;

   --  Elaboration of the PCS is finished, launch others partitions if needed

   PolyORB.Partition_Elaboration.Full_Launch;

   --  Detach partition if needed

   if PolyORB.Parameters.Get_Conf
        (Section => "dsa",
         Key     => "detach",
         Default => False)
   then
      Detach;
   end if;

exception
   when others =>
      O ("PCS initialization failed");
      PolyORB.Initialization.Shutdown_World (Wait_For_Completion => False);
      raise;
end System.Partition_Interface;
