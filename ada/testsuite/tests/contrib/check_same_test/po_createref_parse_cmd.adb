------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O _ C R E A T E R E F _ P A R S E _ C M D                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Command_Line;

package body PO_CreateRef_Parse_Cmd is

   use Ada.Text_IO;
   use GNAT.Command_Line;

   procedure Free is new Ada.Unchecked_Deallocation
     (Codeset_Array, Codeset_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Policies_Array, Policies_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Component_Array, Ptr_Components);

   procedure Free is new Ada.Unchecked_Deallocation
     (Profiles_Array, Ptr_Profiles);

   --  Current profile data
   Profile_Index   : Natural := 0;
   Component_Index : Natural;

   procedure Usage;

   procedure Free (Obj : in out Policy_Subcomponent);

   procedure Free (Obj : in out Parameter_Component);

   procedure Free (Obj : in out Parameter_Profile);

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line (Param : out Parameter_Ref) is
   begin
      --  Globals parameters
      --  t   => (reference) type <string>
      --  pn  => profile number <integer>
      --  h   => help

      --  Profile parameters
      --  pt  => profile type <string>
      --  i   => (profile) index <integer>
      --  g   => system generated? <no argument>
      --  cr  => (profile) POA creator name <string>
      --  vmj => version major <natural>
      --  vmn => version minor <natural>
      --  a   => (profile) inet adress <string>
      --  p   => (profile) listening port <integer>
      --  cn  => components number <integer>
      --  pe  => end of profile <no argument>

      --  Component parameters (generics)
      --  ct   => component type <string>
      --  ce   => end of component <no argument>

      --  codeset parameters
      --  char  => requires C code <string>
      --  wchar => requires W code <string>
      --  s     => supported [C|W] codeset <natural> {<string>}

      --  policies parameters
      --  pol_nb   => policies number <natural>
      --  model    => priority model <string>
      --  priority => priority value <positive>

      --  alternate address parameters
      --  a  => inet address <string>
      --  p  => port  <positive>

      --  SSL parameters
      --  supports => SSL supports flag <string>
      --  requires => SSL requires flag <string>
      --  p        => port <positive>

      Initialize_Option_Scan;

      loop
         case Getopt ("a: char: ce cn: cr: ct: g h  i: inet:requires: "
                      & "supports:  p:  pe pt: pn: pol_nb: port: t: vmj: "
                      & "vmn: wchar:")
         is
            when ASCII.NUL =>
               exit;

            when 'p' =>
               if Full_Switch = "pn" then
                  Param.Profiles := new
                    Profiles_Array (1 .. Natural'Value (Parameter));

               elsif Full_Switch = "pt" then
                  if Param.Profiles = null then
                     Put_Line ("Profile number (pn) must be" &
                               " specified before profiles");
                     exit;
                  end if;

                  Profile_Index := Profile_Index + 1;
                  Param.Profiles.all (Profile_Index).Profile_Type :=
                    new String'(Parameter);

               elsif Full_Switch = "p" then
                  Param.Profiles.all (Profile_Index).Address.Port
                    := Positive'Value (Parameter);

               elsif Full_Switch = "pe" then
                  null;

               elsif Full_Switch = "pol_nb" then
                  Param.Profiles.all (Profile_Index).
                    Components.all (Component_Index).Policies
                    := new Policies_Array
                    (1 .. Natural'Value (Parameter));

                  for I in Param.Profiles.all (Profile_Index).
                    Components.all (Component_Index).Policies.all'Range loop
                     for J in 1 .. 2 loop
                        case Getopt ("model: priority:") is
                           when 'm' =>
                              if Full_Switch = "model" then
                                 Param.Profiles.all (Profile_Index).
                                   Components.all (Component_Index).
                                   Policies.all (I).Priority_Model :=
                                   new String'(Parameter);
                              end if;

                           when 'p' =>
                              if Full_Switch = "priority" then
                                 Param.Profiles.all (Profile_Index).
                                   Components.all (Component_Index).
                                   Policies.all (I).Priority_Value :=
                                   Positive'Value (Parameter);
                              end if;

                           when ASCII.NUL =>  --  it should not happen!
                              raise Program_Error;

                           when others =>
                              raise Program_Error;
                        end case;
                     end loop;
                  end loop;

               elsif Full_Switch = "p" then
                  Param.Profiles.all (Profile_Index).
                    Components.all (Component_Index).Address.Port
                    := Positive'Value (Parameter);
               end if;

            when 't' =>
               if Full_Switch = "t" then
                  Param.Ref_Type := new String'(Parameter);
               end if;

            when 'h' =>
               Usage;
               return;

            when 'a' =>
               if Full_Switch = "a" then
                  Param.Profiles.all (Profile_Index).Address.Inet_Addr
                    := new String'(Parameter);
               end if;

            when 'c' =>
               if Full_Switch = "cr" then
                  Param.Profiles.all (Profile_Index).Creator_Name
                    := new String'(Parameter);

               elsif Full_Switch = "cn" then
                  Param.Profiles.all (Profile_Index).Components :=
                    new Component_Array (1 .. Natural'Value (Parameter));
                  Component_Index := 0;

               elsif Full_Switch = "ct" then
                  if Param.Profiles.all (Profile_Index).Components = null then
                     Put_Line ("No component should be defined before "
                               & "component number");
                     raise Program_Error;

                  end if;
                  Component_Index := Component_Index + 1;
                  Param.Profiles.all (Profile_Index).Components.all
                    (Component_Index).C_Type := new String'(Parameter);

               elsif Full_Switch = "char" then
                  Param.Profiles.all (Profile_Index).
                    Components.all (Component_Index).Cchar
                    := new String'(Parameter);
                  case Getopt ("s:") is

                     when 's' =>
                        Param.Profiles.all (Profile_Index).
                          Components.all (Component_Index).C_Supported
                          := new Codeset_Array
                          (1 .. Positive'Value (Parameter));

                        for I in Param.Profiles.all (Profile_Index).
                          Components.all (Component_Index).
                          C_Supported.all'Range loop

                           Param.Profiles.all (Profile_Index).
                             Components.all (Component_Index).
                             C_Supported.all (I) :=
                             new String'(Get_Argument);
                        end loop;

                     when others =>
                        Put_Line ("coin");

                        raise Program_Error;
                  end case;

               --  elsif Full_Switch = "ce" then
               --     null;
               end if;

            when 'g' =>
               if Full_Switch = "g" then
                  Param.Profiles.all (Profile_Index).Is_Generated := True;
               end if;

            when 'i' =>
               if Full_Switch = "i" then
                  Param.Profiles.all (Profile_Index).Index
                    := new String'(Parameter);
               end if;

            when 'v' =>
               if Full_Switch = "vmj" then
                  Param.Profiles.all (Profile_Index).Version_Major
                    := PolyORB.Types.Octet'Value (Parameter);

               elsif Full_Switch = "vmn" then
                  Param.Profiles.all (Profile_Index).Version_Minor
                    := PolyORB.Types.Octet'Value
                    (Parameter);
               end if;

            when 'w' =>
               if Full_Switch = "wchar" then
                  Param.Profiles.all (Profile_Index).
                    Components.all (Component_Index).Wchar
                    := new String'(Parameter);

                  case Getopt ("s:") is

                     when ASCII.NUL =>
                        Param.Profiles.all (Profile_Index).
                          Components.all (Component_Index).
                          W_Supported := null;
                        exit;

                     when 's' =>
                        Param.Profiles.all (Profile_Index).
                          Components.all (Component_Index).
                          W_Supported := new Codeset_Array
                          (1 .. Positive'Value (Parameter));

                        for I in Param.Profiles.all (Profile_Index).
                          Components.all (Component_Index).
                          W_Supported.all'Range loop
                           Param.Profiles.all (Profile_Index).
                             Components.all (Component_Index).
                             W_Supported.all (I) :=
                             new String'(Get_Argument);
                        end loop;

                     when others =>
                        raise Program_Error;
                  end case;
               end if;

            when 's' =>
               if Full_Switch = "supports" then
                  Param.Profiles.all (Profile_Index).
                    Components.all (Component_Index).SSL_Supports
                    := new String'(Parameter);
               end if;

            when 'r' =>
               if Full_Switch = "requires" then
                  Param.Profiles.all (Profile_Index).
                    Components.all (Component_Index).SSL_Requires
                    := new String'(Parameter);
               end if;

            when others =>
               raise Program_Error;

         end case;
      end loop;

      if Param.Profiles = null then
         Usage;
      end if;

   exception
      when Invalid_Switch =>
         Put_Line ("Unknown switch used : " & Full_Switch);
         raise;

      when Invalid_Parameter =>
         Put_Line ("No parameter provided for " & Full_Switch);
         raise;
   end Parse_Command_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("usage: po_createref -t <type_id> -pn <profile_nb> "
                & "{profile_description}");
      New_Line;

      Put_Line ("GIOP/IIOP profile : "
                & "-pt iiop -i <index> -g -cr <father_poa> "
                & "-vmj <Major> -vmn <Minor> -a <Inet_Address> "
                & "-p <Port> -cn <Components_Number> "
                & "{-ct <component_id> <component> ce} pe");
      New_Line;

      Put_Line ("Policies component : policies -pol_nb <policies_number> "
                & "{-model <SERVER_DECLARED|CLIENT> -priority <value>}");
      New_Line;

      Put_Line ("Code_Sets component : -char <code_set> "
                & "-s <supported_number> {<code_set>}"
                & "-wchar <code_set> -s <supported_number> {<code_set>}");
      New_Line;

      Put_Line ("SSL component : ssl_trans -requires <value> -supports "
                & "<value> -p <port_number>");
      New_Line;

      Put_Line ("Alternate Address component : alternate_address -a "
                & "<inet_address> -p <port_number>");
      New_Line;

      GNAT.OS_Lib.OS_Exit (1);
   end Usage;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : in out Parameter_Ref) is
   begin
      Free (Ptr.Ref_Type);

      for J in Ptr.Profiles.all'Range loop
         Free (Ptr.Profiles.all (J));
      end loop;

      Free (Ptr.Profiles);
   end Free;

   procedure Free (Obj : in out Parameter_Profile) is
   begin
      Free (Obj.Profile_Type);
      Free (Obj.Index);
      Free (Obj.Creator_Name);

      for J in Obj.Components.all'Range loop
         Free (Obj.Components.all (J));
      end loop;

      Free (Obj.Components);
   end Free;

   procedure Free (Obj : in out Parameter_Component) is
   begin
      Free (Obj.C_Type);

      if Obj.Policies /= null then
         for J in Obj.Policies.all'Range loop
            Free (Obj.Policies.all (J));
         end loop;
         Free (Obj.Policies);
      end if;

      if Obj.Cchar /= null then
         Free (Obj.Cchar);
      end if;
      if Obj.Wchar /= null then
         Free (Obj.Wchar);
      end if;
      if Obj.C_Supported /= null then
         for J in Obj.C_Supported.all'Range loop
            Free (Obj.C_Supported.all (J));
         end loop;
         Free (Obj.C_Supported);
      end if;
      if Obj.W_Supported /= null then
         for J in Obj.W_Supported.all'Range loop
            Free (Obj.W_Supported.all (J));
         end loop;
         Free (Obj.W_Supported);
      end if;

      if Obj.SSL_Supports /= null then
         Free (Obj.SSL_Supports);
      end if;
      if Obj.SSL_Requires /= null then
         Free (Obj.SSL_Requires);
      end if;

      if Obj.Address.Inet_Addr /= null then
         Free (Obj.Address.Inet_Addr);
      end if;
   end Free;

   procedure Free (Obj : in out Policy_Subcomponent) is
   begin
      Free (Obj.Priority_Model);
   end Free;

end PO_CreateRef_Parse_Cmd;
