------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with AWS.URL;
with AWS.Utils;

separate (SOAP.Generator)
package body Skel is

   -----------------
   -- End_Service --
   -----------------

   procedure End_Service
     (O    : in out Object;
      Name : String)
   is
      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));
   begin
      --  Spec

      Text_IO.New_Line (Skel_Ads);
      Text_IO.Put_Line (Skel_Ads, "end " & U_Name & ".Server;");

      --  Body

      Text_IO.New_Line (Skel_Adb);
      Text_IO.Put_Line (Skel_Adb, "end " & U_Name & ".Server;");
   end End_Service;

   -------------------
   -- New_Procedure --
   -------------------

   procedure New_Procedure
     (O             : in out Object;
      Proc          : String;
      Documentation : String;
      SOAPAction    : String;
      Namespace     : Name_Space.Object;
      Input         : WSDL.Parameters.P_Set;
      Output        : WSDL.Parameters.P_Set;
      Fault         : WSDL.Parameters.P_Set)
   is
      pragma Unreferenced (Namespace, Fault);

      use Ada.Strings.Fixed;
      use type WSDL.Parameter_Type;
      use type WSDL.Parameters.P_Set;
      use type WSDL.Schema.Binding_Style;
      use type WSDL.Types.Kind;

      procedure Output_Parameters (N : WSDL.Parameters.P_Set);
      --  Output parameters

      L_Proc  : constant String := Format_Name (O, Proc);
      Max_Len : Positive := 1;

      N       : WSDL.Parameters.P_Set;

      -----------------------
      -- Output_Parameters --
      -----------------------

      procedure Output_Parameters (N : WSDL.Parameters.P_Set) is
         T_Name : constant String := WSDL.Types.Name (N.Typ);
         R      : WSDL.Parameters.P_Set;
      begin
         Text_IO.Put (Skel_Adb, "           := ");

         case N.Mode is
            when WSDL.Types.K_Simple =>
               --  Check for Base64 case

               if WSDL.To_Type (T_Name) = WSDL.P_B64 then
                  Text_IO.Put
                    (Skel_Adb,
                     " V (SOAP_Base64'(SOAP.Parameters.Get (Params, """);
                  Text_IO.Put      (Skel_Adb, To_String (N.Name));
                  Text_IO.Put_Line (Skel_Adb, """)));");
               else
                  Text_IO.Put
                    (Skel_Adb, " SOAP.Parameters.Get (Params, """);
                  Text_IO.Put      (Skel_Adb, To_String (N.Name));
                  Text_IO.Put_Line (Skel_Adb, """);");
               end if;

            when WSDL.Types.K_Derived =>
               Text_IO.Put
                 (Skel_Adb,
                  WSDL.Parameters.From_SOAP
                    (N.all,
                     Object =>
                        "SOAP.Parameters.Get (Params, """
                        & To_String (N.Name) & """)",
                     Is_SOAP_Type => True));
               Text_IO.Put_Line (Skel_Adb, ";");

            when WSDL.Types.K_Enumeration =>
               Text_IO.Put_Line
                 (Skel_Adb, T_Name & "_Type'Value");

               Text_IO.Put      (Skel_Adb, "                ");
               Text_IO.Put
                 (Skel_Adb,
                  "(SOAP.Utils.Get (SOAP.Parameters.Argument (Params, """);
               Text_IO.Put      (Skel_Adb, To_String (N.Name));
               Text_IO.Put_Line (Skel_Adb, """)));");

            when WSDL.Types.K_Array =>
               raise Constraint_Error;

            when WSDL.Types.K_Record =>
               Text_IO.Put (Skel_Adb, " (");

               R := N.P;

               while R /= null loop
                  Text_IO.Put
                    (Skel_Adb, "SOAP.Parameters.Get ("
                       & Format_Name (O, T_Name) & "_Record, """
                       & Format_Name (O, To_String (R.Name)) & """)");

                  if R.Next /= null then
                     Text_IO.Put_Line (Skel_Adb, ",");
                     Text_IO.Put      (Skel_Adb, "               ");
                  else
                     Text_IO.Put_Line (Skel_Adb, ");");
                  end if;

                  R := R.Next;
               end loop;
         end case;
      end Output_Parameters;

   begin
      --  Spec

      Text_IO.New_Line (Skel_Ads);
      Text_IO.Put_Line (Skel_Ads, "   generic");
      Text_IO.Put      (Skel_Ads, "      with ");

      Put_Header       (Skel_Ads, O, Proc, Input, Output, Mode => Skel_Spec);

      Text_IO.Put_Line
        (Skel_Ads, "   function " & L_Proc & "_CB");
      Text_IO.Put_Line
        (Skel_Ads, "     (SOAPAction : String;");
      Text_IO.Put_Line
        (Skel_Ads, "      Payload    : SOAP.Message.Payload.Object;");
      Text_IO.Put_Line
        (Skel_Ads, "      Request    : AWS.Status.Data)");
      Text_IO.Put_Line
        (Skel_Ads, "      return AWS.Response.Data;");
      Output_Comment (Skel_Ads, Documentation, Indent => 3);

      --  Body

      Text_IO.New_Line (Skel_Adb);
      Header_Box (O, Skel_Adb, L_Proc & "_CB");

      Text_IO.New_Line (Skel_Adb);
      Text_IO.Put_Line
        (Skel_Adb, "   function " & L_Proc & "_CB");
      Text_IO.Put_Line
        (Skel_Adb, "     (SOAPAction : String;");
      Text_IO.Put_Line
        (Skel_Adb, "      Payload    : SOAP.Message.Payload.Object;");
      Text_IO.Put_Line
        (Skel_Adb, "      Request    : AWS.Status.Data)");
      Text_IO.Put_Line
        (Skel_Adb, "      return AWS.Response.Data");
      Text_IO.Put_Line (Skel_Adb, "   is");

      if O.Style = WSDL.Schema.RPC then
         Text_IO.Put_Line
           (Skel_Adb, "      Proc_Name : constant String");
         Text_IO.Put_Line
           (Skel_Adb, "        := SOAP.Message.Payload.Procedure_Name"
              & " (Payload);");
      end if;

      Text_IO.Put_Line
        (Skel_Adb, "      Params    : constant SOAP.Parameters.List");
      Text_IO.Put_Line
        (Skel_Adb, "        := SOAP.Message.Parameters (Payload);");
      Text_IO.Put_Line
        (Skel_Adb, "      Response  : SOAP.Message.Response.Object;");
      Text_IO.Put_Line
        (Skel_Adb, "      R_Params  : SOAP.Parameters.List;");
      Text_IO.Put_Line (Skel_Adb, "   begin");

      --  Procedure body start here

      --  First check the SOAPAction

      if O.Debug then
         Text_IO.Put_Line
           (Skel_Adb,
            "      Put_Line (""[SERVER/" & L_Proc & "_CB] Payload recv : """);
         Text_IO.Put_Line
           (Skel_Adb,
            "                & AWS.Status.Payload (Request));");
         Text_IO.New_Line (Skel_Adb);

         Text_IO.Put_Line
           (Skel_Adb,
            "      Put_Line (""[SERVER/" & L_Proc & "_CB] SOAPAction : """
            & " & SOAPAction);");
         Text_IO.New_Line (Skel_Adb);
      end if;

      Text_IO.Put_Line
        (Skel_Adb,
         "      if SOAPAction /= """ & To_String (O.Prefix) & SOAPAction
         & """ then");
      Text_IO.Put_Line
        (Skel_Adb, "         return SOAP.Message.Response.Build");
      Text_IO.Put_Line
        (Skel_Adb, "           (SOAP.Message.Response.Error.Build");
      Text_IO.Put_Line
        (Skel_Adb, "              (SOAP.Message.Response.Error.Client,");
      Text_IO.Put_Line
        (Skel_Adb, "               """
           & "SOAPAction "" & SOAPAction & "" in " & L_Proc & ", """);
      Text_IO.Put_Line
        (Skel_Adb, "               "
           & "  & """ & SOAPAction & " expected.""));");
      Text_IO.Put_Line
        (Skel_Adb, "      end if;");
      Text_IO.New_Line (Skel_Adb);

      --  Then check the procedure name

      if O.Style = WSDL.Schema.RPC then
         if O.Debug then
            Text_IO.Put_Line
              (Skel_Adb,
               "      Put_Line (""[SERVER/" & L_Proc & "_CB] Proc_Name : """
               & " & Proc_Name);");
            Text_IO.New_Line (Skel_Adb);
         end if;

         Text_IO.Put_Line
           (Skel_Adb,
            "      if Proc_Name /= """ & To_String (O.Prefix) & Proc
            & """ then");
         Text_IO.Put_Line
           (Skel_Adb, "         return SOAP.Message.Response.Build");
         Text_IO.Put_Line
           (Skel_Adb, "           (SOAP.Message.Response.Error.Build");
         Text_IO.Put_Line
           (Skel_Adb, "              (SOAP.Message.Response.Error.Client,");
         Text_IO.Put_Line
           (Skel_Adb, "               """
            & "Found procedure "" & Proc_Name & "" in " & L_Proc & ", """);
         Text_IO.Put_Line
           (Skel_Adb, "               "
              & "  & """ & Proc & " expected.""));");
         Text_IO.Put_Line
           (Skel_Adb, "      end if;");
         Text_IO.New_Line (Skel_Adb);
      end if;

      if O.Debug then
         Text_IO.Put_Line
           (Skel_Adb,
            "      Put_Line (""[SERVER/" & L_Proc & "_CB] Payload : """);
         Text_IO.Put_Line
           (Skel_Adb,
            "                & SOAP.Message.XML.Image (Payload, Schema));");
         Text_IO.New_Line (Skel_Adb);
      end if;

      --  Initialize the Response structure

      Text_IO.Put_Line
        (Skel_Adb, "      Response := SOAP.Message.Response.From (Payload);");
      Text_IO.New_Line (Skel_Adb);

      if Input /= null or else Output /= null then
         Text_IO.Put_Line
           (Skel_Adb, "      declare");
      end if;

      --  Find maximum parameter name length to align them

      N := Input;

      while N /= null loop
         Max_Len := Positive'Max
           (Max_Len, Format_Name (O, To_String (N.Name))'Length);
         N := N.Next;
      end loop;

      --  Input parameters

      N := Input;

      while N /= null loop
         declare
            use type WSDL.Schema.Encoding_Style;
            T_Name : constant String := WSDL.Types.Name (N.Typ);
         begin
            Text_IO.Put      (Skel_Adb, "         ");

            if N.Mode = WSDL.Types.K_Array then
               Text_IO.Put_Line
                 (Skel_Adb,
                  To_String (N.Name) & "_"
                  & Format_Name (O, T_Name) & "_Array : "
                  & "constant SOAP.Types.SOAP_Array");
               Text_IO.Put_Line
                 (Skel_Adb,
                  "           := SOAP.Parameters.Get (Params, """
                  & To_String (N.Name) & """);");
               Text_IO.Put      (Skel_Adb, "         ");

            elsif N.Mode = WSDL.Types.K_Record then
               Text_IO.Put_Line
                 (Skel_Adb,
                  To_String (N.Name) & "_"
                  & Format_Name (O, T_Name) & "_Record : "
                  & "constant SOAP.Types.SOAP_Record");

               if N.P = null then
                  --  An empty record, just build a corresponding object
                  Text_IO.Put_Line
                    (Skel_Adb,
                     "           := SOAP.Types.R (SOAP.Types.Empty_Object_Set"
                     & ", """ & To_String (N.Name) & """);");
               else
                  Text_IO.Put_Line
                    (Skel_Adb,
                     "           := SOAP.Parameters.Get (Params, """
                     & To_String (N.Name) & """);");
               end if;

               Text_IO.Put      (Skel_Adb, "         ");
            end if;

            declare
               Name : constant String :=
                        Format_Name (O, To_String (N.Name));
            begin
               Text_IO.Put   (Skel_Adb, Name);
               Text_IO.Put   (Skel_Adb, (Max_Len - Name'Length) * ' ');
            end;

            Text_IO.Put      (Skel_Adb, " : constant ");

            case N.Mode is
               when WSDL.Types.K_Simple =>
                  Text_IO.Put_Line
                    (Skel_Adb, WSDL.To_Ada (WSDL.To_Type (T_Name)));
                  Output_Parameters (N);

               when WSDL.Types.K_Derived | WSDL.Types.K_Enumeration =>
                  Text_IO.Put_Line (Skel_Adb, T_Name & "_Type");
                  Output_Parameters (N);

               when WSDL.Types.K_Array =>
                  Text_IO.Put_Line
                    (Skel_Adb, Format_Name (O, T_Name) & "_Type");
                  Text_IO.Put_Line
                    (Skel_Adb, "           := To_"
                     & Format_Name (O, T_Name) & "_Type (V ("
                     & To_String (N.Name) & "_"
                     & Format_Name (O, T_Name) & "_Array));");

               when WSDL.Types.K_Record =>
                  Text_IO.Put_Line
                    (Skel_Adb, Format_Name (O, T_Name) & "_Type");
                  Text_IO.Put_Line
                    (Skel_Adb, "           := To_"
                     & Format_Name (O, T_Name) & "_Type ("
                     & To_String (N.Name) & "_"
                     & Format_Name (O, T_Name) & "_Record);");
            end case;
         end;

         N := N.Next;
      end loop;

      --  Output parameters

      if Output = null then

         Text_IO.Put_Line
           (Skel_Adb, "      begin");
         Text_IO.Put
           (Skel_Adb, "         " & L_Proc);

      else
         Text_IO.Put
           (Skel_Adb, "         Result : constant ");

         if Output.Next = null
           and then Output.Mode = WSDL.Types.K_Simple
         then
            Text_IO.Put_Line
              (Skel_Adb,
               WSDL.To_Ada (WSDL.To_Type (WSDL.Types.Name (Output.Typ))));
         else
            Text_IO.Put_Line
              (Skel_Adb, L_Proc & "_Result");
         end if;

         Text_IO.Put
           (Skel_Adb, "           := " & L_Proc & "_CB." & L_Proc);

         if Input /= null then
            Text_IO.New_Line (Skel_Adb);
         end if;
      end if;

      --  Input parameters

      N := Input;

      while N /= null loop
         Text_IO.Put (Skel_Adb, "                ");

         if N = Input then
            Text_IO.Put (Skel_Adb, "(");
         else
            Text_IO.Put (Skel_Adb, " ");
         end if;

         Text_IO.Put (Skel_Adb, Format_Name (O, To_String (N.Name)));

         if N.Next = null then
            Text_IO.Put (Skel_Adb, ")");
         else
            Text_IO.Put_Line (Skel_Adb, ",");
         end if;

         N := N.Next;
      end loop;

      Text_IO.Put_Line (Skel_Adb, ";");

      --  Set SOAP results

      if Output /= null then

         Text_IO.Put_Line
           (Skel_Adb, "      begin");

         Text_IO.Put_Line
           (Skel_Adb, "         R_Params :=");
         Text_IO.Put
           (Skel_Adb, "           +");

         N := Output;

         while N /= null loop
            if N /= Output then
               Text_IO.Put
                 (Skel_Adb, "           & ");
            end if;

            declare
               T_Name : constant String := WSDL.Types.Name (N.Typ, True);
            begin
               case N.Mode is
                  when WSDL.Types.K_Simple =>

                     if Output.Next = null then
                        --  A single simple parameter as return

                        Text_IO.Put
                          (Skel_Adb, WSDL.Set_Routine (WSDL.To_Type (T_Name)));

                        Text_IO.Put
                          (Skel_Adb,
                           " (Result, """ & To_String (N.Name) & ""","
                           & " Type_Name => """ & T_Name & """)");

                     else
                        --  Multiple value returned, this is a record

                        Text_IO.Put
                          (Skel_Adb,
                           WSDL.Set_Routine
                             (WSDL.To_Type (T_Name), Constrained => True));

                        Text_IO.Put
                          (Skel_Adb, " (Result."
                           & Format_Name (O, To_String (N.Name))
                           & ", """ & To_String (N.Name) & ""","
                           & " Type_Name => """ & T_Name & """)");
                     end if;

                  when WSDL.Types.K_Derived =>

                     if Output.Next = null then
                        Text_IO.Put
                          (Skel_Adb,
                           WSDL.Parameters.To_SOAP
                             (N.all,
                              Object    => "Result",
                              Name      => To_String (N.Name),
                              Type_Name => T_Name));
                     else
                        Text_IO.Put
                          (Skel_Adb,
                           WSDL.Parameters.To_SOAP
                             (N.all,
                              Object    =>
                                "Result."
                                  & Format_Name (O, To_String (N.Name)),
                              Name      => To_String (N.Name),
                              Type_Name => T_Name));
                     end if;

                  when WSDL.Types.K_Enumeration =>

                     if Output.Next = null then
                        Text_IO.Put
                          (Skel_Adb,
                           WSDL.Parameters.To_SOAP
                             (N.all,
                              Object    => "Result",
                              Name      => To_String (N.Name),
                              Type_Name => T_Name));
                     else
                        Text_IO.Put
                          (Skel_Adb,
                           WSDL.Parameters.To_SOAP
                             (N.all,
                              Object    =>
                                "Result."
                                   & Format_Name (O, To_String (N.Name)),
                              Name      => To_String (N.Name),
                              Type_Name => T_Name));
                     end if;

                  when WSDL.Types.K_Array =>
                     if Output.Next = null then
                        --  A single array as returned parameter
                        Text_IO.Put
                          (Skel_Adb,
                           WSDL.Parameters.To_SOAP
                             (N.all,
                              Object    => "Result",
                              Name      => To_String (N.Name),
                              Type_Name => T_Name));

                     else
                        --  Array here is part of an array
                        Text_IO.Put
                          (Skel_Adb,
                           WSDL.Parameters.To_SOAP
                             (N.all,
                              Object    => "Result."
                                           & To_String (N.Name) & ".Item.all",
                              Name      => To_String (N.Name),
                              Type_Name => T_Name));
                     end if;

                  when WSDL.Types.K_Record =>
                     Text_IO.Put
                       (Skel_Adb,
                        WSDL.Parameters.To_SOAP
                          (N.all,
                           Object => "Result",
                           Name   => To_String (N.Name)));
               end case;

               if N.Next = null then
                  Text_IO.Put_Line (Skel_Adb, ";");
               else
                  Text_IO.New_Line (Skel_Adb);
               end if;
            end;

            N := N.Next;
         end loop;
      end if;

      --  Catch user's callback exceptions

      Text_IO.Put_Line
        (Skel_Adb, "      exception");
      Text_IO.Put_Line
        (Skel_Adb, "         when E : others =>");

      Text_IO.Put_Line
        (Skel_Adb, "            --  Here we have a problem with user's"
           & " callback, return a SOAP error");

      Text_IO.Put_Line
        (Skel_Adb, "            return SOAP.Message.Response.Build");
      Text_IO.Put_Line
        (Skel_Adb, "              (SOAP.Message.Response.Error.Build");
      Text_IO.Put_Line
        (Skel_Adb, "                 (SOAP.Message.Response.Error.Client,");
      Text_IO.Put_Line
        (Skel_Adb, "                  """
           & "Error in " & L_Proc & " (""");
      Text_IO.Put_Line
        (Skel_Adb, "                    & Exception_Message (E) & "")""));");

      Text_IO.Put_Line
        (Skel_Adb, "      end;");
      Text_IO.New_Line (Skel_Adb);

      Text_IO.Put_Line
        (Skel_Adb, "      SOAP.Message.Set_Parameters (Response, R_Params);");

      if O.Debug then
         Text_IO.Put_Line
           (Skel_Adb,
            "      Put_Line (""[SERVER/" & L_Proc & "_CB] Response : """);
         Text_IO.Put_Line
           (Skel_Adb,
            "                & SOAP.Message.XML.Image (Response, Schema));");
      end if;

      Text_IO.Put_Line
        (Skel_Adb,
         "      return SOAP.Message.Response.Build (Response, Schema);");

      Text_IO.Put_Line
        (Skel_Adb, "   exception");

      --  Types.Data_Error

      Text_IO.Put_Line
        (Skel_Adb, "      when E : others =>");

      Text_IO.Put_Line
        (Skel_Adb, "         --  Here we have a problem with some"
           & " parameters, return a SOAP error");

      Text_IO.Put_Line
        (Skel_Adb, "         return SOAP.Message.Response.Build");
      Text_IO.Put_Line
        (Skel_Adb, "           (SOAP.Message.Response.Error.Build");
      Text_IO.Put_Line
        (Skel_Adb, "              (SOAP.Message.Response.Error.Client,");
      Text_IO.Put_Line
        (Skel_Adb, "               """
           & "Parameter error in " & L_Proc & " (""");
      Text_IO.Put_Line
        (Skel_Adb, "                 & Exception_Message (E) & "")""));");

      Text_IO.Put_Line (Skel_Adb, "   end " & L_Proc & "_CB;");
   end New_Procedure;

   -------------------
   -- Start_Service --
   -------------------

   procedure Start_Service
     (O                  : in out Object;
      Name               : String;
      Root_Documentation : String;
      Documentation      : String;
      Location           : String)
   is
      pragma Unreferenced (Root_Documentation);

      U_Name : constant String         := To_Unit_Name (Format_Name (O, Name));
      URL    : constant AWS.URL.Object := AWS.URL.Parse (Location);
   begin
      --  Spec

      Text_IO.Put_Line (Skel_Ads, "pragma Warnings (Off);");
      Text_IO.New_Line (Skel_Ads);
      With_Unit (Skel_Ads, "Ada.Calendar", Elab => Off);
      With_Unit (Skel_Ads, "System.Assertions", Elab => Off);
      Text_IO.New_Line (Skel_Ads);
      With_Unit (Skel_Ads, "AWS.Status");
      With_Unit (Skel_Ads, "AWS.Response");
      Text_IO.New_Line (Skel_Ads);
      With_Unit (Skel_Ads, "SOAP.Message.Payload", Elab => Children);
      With_Unit (Skel_Ads, "SOAP.Types");
      Text_IO.New_Line (Skel_Ads);
      With_Unit (Skel_Ads, U_Name & ".Types");
      Text_IO.New_Line (Skel_Ads);

      Output_Comment (Skel_Ads, Documentation, Indent => 0);
      Text_IO.New_Line (Skel_Ads);

      Text_IO.Put_Line (Skel_Ads, "package " & U_Name & ".Server is");
      Text_IO.New_Line (Skel_Ads);
      Text_IO.Put_Line (Skel_Ads, "   use " & U_Name & ".Types;");
      Text_IO.New_Line (Skel_Ads);
      Text_IO.Put_Line
        (Skel_Ads,
         "   Port : constant := "
         & AWS.Utils.Image (AWS.URL.Port (URL)) & ';');

      --  Body

      if O.Debug then
         With_Unit (Skel_Adb, "Ada.Text_IO", Elab => Off);
         With_Unit (Skel_Adb, "SOAP.Message.XML");
      end if;

      With_Unit (Skel_Adb, "Ada.Exceptions", Elab => Off);
      Text_IO.New_Line (Skel_Adb);
      With_Unit (Skel_Adb, "SOAP.Message.Response.Error", Elab => Children);
      With_Unit (Skel_Adb, "SOAP.Name_Space");
      With_Unit (Skel_Adb, "SOAP.Parameters");
      With_Unit (Skel_Adb, "SOAP.Utils");
      Text_IO.New_Line (Skel_Adb);
      Text_IO.Put_Line (Skel_Adb, "package body " & U_Name & ".Server is");
      Text_IO.New_Line (Skel_Adb);

      if O.Debug then
         Text_IO.Put_Line (Skel_Adb, "   use Ada.Text_IO;");
      end if;

      Text_IO.Put_Line (Skel_Adb, "   use Ada.Exceptions;");
      Text_IO.New_Line (Skel_Adb);
      Text_IO.Put_Line (Skel_Adb, "   use SOAP.Types;");
      Text_IO.Put_Line (Skel_Adb, "   use type SOAP.Parameters.List;");
      Text_IO.New_Line (Skel_Adb);
      Text_IO.Put_Line (Skel_Adb, "   pragma Style_Checks (Off);");
   end Start_Service;

end Skel;
