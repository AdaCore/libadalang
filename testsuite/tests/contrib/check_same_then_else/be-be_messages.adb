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

with Utils.Strings;                use Utils.Strings;
with Utils.Files;
with Utils.Output.Streams.Storage;
with Utils.Spellings;
pragma Elaborate_All (Utils.Spellings);

with ST.BE_Message_Leveling_And_Suppression;
with ST.Inspection_Configuration; use ST.Inspection_Configuration;
with ST.Html_Output;

with Java2IL.JUtils;  --  TBD: Should call through Typical_FE
with Gnat2IL.SCIL.Debugging;

package body BE.BE_Messages is

   use Utils.Output.Streams;

   Generate_Msg_File : Boolean := False;
   --  Should the messages be generated on file?

   Compiler_Mode_Flag : Boolean := False;
   --  Should the messages be generated on stdout right away?

   Empty_Spelling : constant Utils.Spellings.Spelling :=
      Utils.Spellings.Intern ("");  --  TBD: Should be in Spellings

   procedure Sum
     (Into         : in out Check_Count_Array;
      Increment_By : Check_Count_Array)
   is
   begin
      for Subkind in Into'Range loop
         if Increment_By (Subkind) > (Natural'Last - Into (Subkind)) then
            --  Avoid overflow
            Into (Subkind) := Natural'Last;
         else
            Into (Subkind) := Into (Subkind) + Increment_By (Subkind);
         end if;
      end loop;
   end Sum;

   function Total_Count_Of_Checks
     (Checks : Check_Count_Array)
      return   Natural
   is
      Total_Checks : Natural := 0;
   begin
      for I in Checks'Range loop
         --  Note: They were already canonicalized in PVP, so the
         --  noncanonical ones will be zero.
         Total_Checks := Total_Checks + Checks (I);
      end loop;
      return Total_Checks;
   end Total_Count_Of_Checks;

   function Current_Language return  ST.Languages.Source_Language renames
     ST.Languages.Current_Language;

   function Use_Dots (S : String) return String is
   begin
      return Strings.Replace_Char (S, Old => '/', Becomes => '.');
   end Use_Dots;

   function Procedure_Name
     (Ident            : Spelling;
      Include_Profile  : Boolean;
      For_Html_Display : Boolean := False;
      Module_Name      : String)
      return             String
   is
   --  Prefix Ident with Module_Name if any.
   --  Strip out "(...)" part if Include_Profile = False.

   begin

      if Current_Language = Ada_Language then
         --  We do not want to add module_name or massage names generated
         --  by gnat2scil in general, except for unchecked conversions and
         --  postcondition procedures.

         return Gnat2IL.SCIL.Debugging.Fixup_Unchecked_Conversions
           (Gnat2IL.SCIL.Debugging.Strip_Postconditions
              (String_Ref (Ident).S));

      else
         return Java2IL.JUtils.Method_Identifier_To_Java
                  (Java2IL.JUtils.Strip_Frame
                      (Use_Dots (String_Ref (Ident).S)),
                   Include_Profile,
                   For_Html_Display,
                   Module_Name);
      end if;
   end Procedure_Name;

   function Procedure_Name
     (Kind             : BE_Message_Kind'Class;
      Language         : Source_Language;
      Include_Module   : Boolean := False;
      Include_Profile  : Boolean := False;
      For_Html_Display : Boolean := False) --  modifier on Include_Profile
      return String
   is
   begin
      case Kind.Subkind is
         when Module_Annotation | End_Module_Annotation =>
            --  These don't have a procedure_name field
            if Include_Module then
               --  Just return the module name
               return Use_Dots (String_Ref (Kind.Module_Name).S);
            else
               return "";
            end if;
         when others =>
            if Include_Module and then Language /= Ada_Language then
               return Procedure_Name
                        (Kind.Procedure_Name,
                         Include_Profile,
                         For_Html_Display,
                         Module_Name =>
                           Use_Dots (String_Ref (Kind.Module_Name).S));
            else
               return Procedure_Name
                        (Kind.Procedure_Name,
                         Include_Profile,
                         For_Html_Display,
                         Module_Name => "");
            end if;
      end case;
   end Procedure_Name;

   function Callee_Procedure_Name
     (Kind            : BE_Message_Kind'Class;
      Language        : Source_Language; --  Ignore for now; assume Java.
      Include_Module  : Boolean := False;
      Include_Profile : Boolean := False)
      return            String
   is
   begin
      if Include_Module and then Language /= Ada_Language then
         return Procedure_Name
                  (Kind.Callee,
                   Include_Profile,
                   Module_Name =>
                      Use_Dots (String_Ref (Kind.Callee_Module).S));
      else
         return Procedure_Name
                  (Kind.Callee,
                   Include_Profile,
                   Module_Name => "");
      end if;
   end Callee_Procedure_Name;

   function Callee_Module_Name
     (Kind     : BE_Message_Kind'Class;
      Language : Source_Language --  Ignore for now; assume Java.
        )
      return     String
   is
      pragma Unreferenced (Language);
   --  N.b. This returns slashes in the
   --  module name because at the time of writing
   --  it's being used for the html file names
   begin
      return String_Ref (Kind.Callee_Module).S;
   end Callee_Module_Name;

   function Module_Name
     (Mod_Name : Spelling;
      Language : Source_Language --  Ignore for now; assume Java.
        )
      return     String
   is
      pragma Unreferenced (Language);
   --  N.b. This returns slashes in the
   --  module name because at the time of writing
   --  it's being used for the html file names
   begin
      return String_Ref (Mod_Name).S;
   end Module_Name;

   function Chop_Off_Tail (S : String) return String is
   begin
      for i in reverse S'Range loop
         if S (i) = '/' then
            return S (S'First .. i - 1);
         end if;
      end loop;
      return "";
   end Chop_Off_Tail;

   function Callee_Package_Name
     (Callee_Module : Spelling;
      Language      : Source_Language)
      return          String
   is
      pragma Unreferenced (Language);
   --  Return a human-readable version of the callee's Package name.
   --  TBD: This is no longer used
   begin
      pragma Assert (False);
      return Chop_Off_Tail (String_Ref (Callee_Module).S);
   end Callee_Package_Name;

   function Package_Name
     (Kind     : BE_Message_Kind'Class;
      Language : Source_Language)
      return     String
   is
   --  Return a human-readable version of the Package name.
   begin
      case Language is
         when Ada_Language =>
            --  TBD TBD This code handles Ada 83
            --  but not Ada 95+
            --  We need to compute the common prefix and
            --  store it somewhere, possibly the DB
            return "";
         when C_Language =>
            --  TBD
            return "";
         when Java_Like_Language =>
            return Chop_Off_Tail (String_Ref (Kind.Module_Name).S);
      end case;
   end Package_Name;

   function Canonicalize
     (Subkind : BE_Message_Subkind)
      return    BE_Message_Subkind
   is
   --  Two different Subkinds may have the same Readable_Subkind
   --  (see below).  We want to maintain the distinction, internally,
   --  but want the errors to look "the same" in the method summary
   --  table.
   begin
      case Subkind is
         when Assign_Stm_Check | Numeric_Overflow_Check =>
            return User_Assign_Stm_Check;
         when others =>
            return Subkind;
      end case;
   end Canonicalize;

   function Readable_Subkind (Subkind : BE_Message_Subkind) return String is
   --  Return user-interpretable image associated with message sub kind,
   begin
      case Subkind is
      when Module_Annotation =>
         case Current_Language is
            when Ada_Language =>
               return "module";
            when C_Language =>
               return "module";  --  TBD
            when Java_Like_Language =>
               return "class";
         end case;
      when Procedure_Annotation =>
         case Current_Language is
            when Ada_Language =>
               return "subp";
            when C_Language =>
               return "function";
            when Java_Like_Language =>
               return "method";
         end case;
      when End_Module_Annotation =>
         case Current_Language is
            when Ada_Language | C_Language =>
               return "end of module";
            when Java_Like_Language =>
               return "end of class";
         end case;
      when End_Procedure_Annotation =>
         case Current_Language is
            when Ada_Language =>
               return "end of subp";
            when C_Language =>
               return "end of function";
            when Java_Like_Language =>
               return "end of method";
         end case;

      when Input_Annotation =>
         return "global inputs";
      when Output_Annotation =>
         return "global outputs";
      when New_Obj_Annotation =>
         return "new obj";
      when Precondition_Annotation =>
         return "pre";
      when Presumption_Annotation =>
         return "presumption";
      when Postcondition_Annotation =>
         return "post";
      when Unknown_Call_Annotation =>
         return "unanalyzed";
      when Non_Analyzed_Call_Warning =>
         return "call too complex - analysis skipped";
      when Unknown_Call_Warning =>
         return Format
                  ("%1 not available",
                   Arg1 => Readable_Subkind (Procedure_Annotation));
      when Test_Vector_Annotation =>
         return "test_vector";
      when Suspicious_Precondition_Subkind =>
         return "suspicious precondition";
      when Suspicious_Input_Warning =>
         return "suspicious input";
      when Suspicious_Constant_Operation_Warning =>
         return "suspicious constant operation";
      when Unread_In_Out_Parameter_Warning =>
         return "unread parameter";
      when Unassigned_In_Out_Parameter_Warning =>
         return "unassigned parameter";
      when Aliasing_Check =>
         return "aliasing check";

      --  TBD: Change some check names; make more specific.
      --  Non_Neg_Check-->...logical.

      when Invalid_Check =>
         case Current_Language is
            when Ada_Language =>
               return "validity check";
            when C_Language =>
               return "validity check"; --  TBD
            when Java_Like_Language =>
               return "use of default init";
         end case;
      when Invalid_Or_Null_Check =>
         case Current_Language is
            when Ada_Language =>
               return "access check";
            when C_Language =>
               return "null dereference";  --  TBD
            when Java_Like_Language =>
               return "null dereference";
         end case;
      when Freed_Check =>
         return "freed check"; -- check for Unchecked_Deallocation misuse
      when Divide_By_Zero_Check =>
         return "divide by zero";
      when Boolean_Check =>
         return "boolean not 0 or 1";
      when Non_Neg_Check =>
         return "negative operand of bit-wise operator";
      when Negative_Exponent_Check =>
         return "negative operand of shift or exponentiation";
      when Assign_Stm_Check | Numeric_Overflow_Check =>
         case Current_Language is
            when Ada_Language =>
               return "overflow check";
            when C_Language | Java_Like_Language =>
               return "overflow";
         end case;
      when Floating_Point_Underflow_Check =>
         case Current_Language is
            when Ada_Language =>
               return "floating point underflow";
            when C_Language | Java_Like_Language =>
               return "underflow";
         end case;
      when User_Precondition_Check =>
         case Current_Language is
            when Ada_Language =>
               return "user precondition";
            when C_Language | Java_Like_Language =>
               return "precondition failure";
         end case;
      when Precondition_Check =>
         case Current_Language is
            when Ada_Language =>
               return "precondition";
            when C_Language | Java_Like_Language =>
               return "precondition failure";
         end case;
      when Postcondition_Check =>
         case Current_Language is
            when Ada_Language =>
               return "postcondition";
            when C_Language | Java_Like_Language =>
               return "postcondition failure";
         end case;
      when Raise_Check =>
         case Current_Language is
            when Ada_Language =>
               return "raise exception";
            when C_Language | Java_Like_Language =>
               return "throw exception";
         end case;
      when Conditional_Raise_Check =>
         case Current_Language is
            when Ada_Language =>
               return "conditional check";
            when C_Language | Java_Like_Language =>
               return "conditional throw";
         end case;
      when Array_Indexing_Check =>
         case Current_Language is
            when Ada_Language =>
               return "array index check";
            when C_Language | Java_Like_Language =>
               return "array index out of bounds";
         end case;
      when Assertion_Check =>
         case Current_Language is
            when Ada_Language =>
               return "assertion";
            when C_Language | Java_Like_Language =>
               return "assert failure";
         end case;
      when Numeric_Range_Check =>
         case Current_Language is
            when Ada_Language =>
               return "range check";
            when C_Language | Java_Like_Language =>
               return "out of range";
         end case;
      when Type_Variant_Check =>
         case Current_Language is
            when Ada_Language =>
               return "discriminant check";
            when C_Language | Java_Like_Language =>
               return "incorrect variant";
         end case;
      when Tag_Check =>
         case Current_Language is
            when Ada_Language =>
               return "tag check";
            when Java_Like_Language =>
               return "incorrect type tag";
            when C_Language =>
               pragma Assert (False);
               return "tag check";
         end case;
      when Procedure_Does_Not_Return_Error =>
         case Current_Language is
            when Ada_Language =>
               return "subp never returns";
            when C_Language =>
               return "function never returns";
            when Java_Like_Language =>
               return "never returns";
         end case;
      when Check_Fails_On_Every_Call_Error =>
         --  This can be terse because "text" part says more
         return "subp always fails";
      when Incompletely_Analyzed_Procedure_Warning   =>
         case Current_Language is
            when Ada_Language =>
               return "subp incompletely analyzed";
            when C_Language =>
               return "function incompletely analyzed";
            when Java_Like_Language =>
               return "incompletely analyzed";
         end case;
      when Non_Analyzed_Procedure_Warning   =>
         case Current_Language is
            when Ada_Language =>
               return "subp not analyzed";
            when C_Language =>
               return "function not analyzed";
            when Java_Like_Language =>
               return "not analyzed";
         end case;
      when Non_Analyzed_Module_Warning =>
         case Current_Language is
            when Ada_Language =>
               return "module not analyzed";
            when C_Language =>
               return "module not analyzed";  --  TBD
            when Java_Like_Language =>
               return "class not analyzed";
         end case;
      when Analyzed_Module_Warning =>
         case Current_Language is
            when Ada_Language =>
               return "module analyzed";
            when C_Language =>
               return "module analyzed";  --  TBD
            when Java_Like_Language =>
               return "class analyzed";
         end case;
      when Unlocked_Reentrant_Update_Error =>
         case Current_Language is
            when Ada_Language =>
               return "unprotected access";
            when C_Language =>
               return "unprotected access"; --  TBD
            when Java_Like_Language =>
               return "unlocked reentrant update";
         end case;
      when Unlocked_Shared_Daemon_Update_Error =>
         case Current_Language is
            when Ada_Language =>
               return "unprotected shared access";
            when C_Language =>
               return "unprotected shared access"; --  TBD
            when Java_Like_Language =>
               return "unlocked shared daemon update";
         end case;
      when Mismatched_Locked_Update_Error =>
         case Current_Language is
            when Ada_Language =>
               return "mismatched protected access";
            when C_Language =>
               return "mismatched protected access"; --  TBD
            when Java_Like_Language =>
               return "mismatched locked update";
         end case;
      when Dead_Store_Warning =>
         return "unused assignment";
      when Dead_Outparam_Store_Warning =>
         return "unused out parameter";
      when Potentially_Dead_Store_Warning =>
         return "unused assignment to global";
      when Same_Value_Dead_Store_Warning =>
         return "useless self assignment";
      when Dead_Block_Warning =>
         return "dead code";
      when Infinite_Loop_Warning =>
         return "loop does not complete normally";
      when Dead_Block_Continuation_Warning =>
         return "dead code continues";
      when Dead_Edge_Warning | Plain_Dead_Edge_Warning =>
         case Current_Language is
            when Ada_Language | C_Language =>
               return "test predetermined";
            when Java_Like_Language =>
               return "test always goes same way";
         end case;
      when True_Dead_Edge_Warning =>
         case Current_Language is
            when Ada_Language | C_Language =>
               return "test always true";
            when Java_Like_Language =>
               return "test always goes same way (T)";
         end case;
      when False_Dead_Edge_Warning =>
         case Current_Language is
            when Ada_Language | C_Language =>
               return "test always false";
            when Java_Like_Language =>
               return "test always goes same way (F)";
         end case;
      when True_Condition_Dead_Edge_Warning =>
         case Current_Language is
            when Ada_Language | C_Language =>
               return "condition predetermined";
            when Java_Like_Language =>
               return "condition always goes same way (T)";
         end case;
      when False_Condition_Dead_Edge_Warning =>
         case Current_Language is
            when Ada_Language | C_Language =>
               return "condition predetermined";
            when Java_Like_Language =>
               return "condition always goes same way (F)";
         end case;
      when Unrepeatable_While_Loop_Warning =>
         return "while loop never repeats";
      when Locally_Unused_Store_Annotation =>
         return "potentially unused assignment";
      when Local_Lock_Of_Global_Object =>
         return "local lock of global object";
      when SQL_Injection_Check =>
         return "possible SQL injection";
      when XSS_Check =>
         return "cross-site scripting potential";
      end case;
   end Readable_Subkind;

   procedure Put_Readable_Message_Kind
     (Stream                : in out Output.Streams.Stream'Class;
      Kind                  : BE_Message_Kind'Class;
      Include_Precond_Index : Boolean := False)
   is
   begin
      case Kind.Subkind is
         when Check_Kind_Enum =>
            if Kind.Subkind = Array_Indexing_Check
              or else Kind.Subkind = Raise_Check
            then
               null;
            else
               if Is_Proper_Spelling (Kind.Exception_Id) then
                  if Kind.Subkind = Conditional_Raise_Check then
                     Put (Stream, String_Ref (Kind.Exception_Id).S);
                     Put (Stream, " check");
                  else
                     case Current_Language is
                        when Ada_Language =>
                           Put (Stream, "raise ");
                        when C_Language =>
                           Put (Stream, "throw "); --  TBD
                        when Java_Like_Language =>
                           Put (Stream, "throw ");
                     end case;
                     Put (Stream, String_Ref (Kind.Exception_Id).S);
                  end if;
                  return; ----------------
               end if;
            end if;

         --  Do the default action

         when Security_Check_Subkind =>
            --  Do the default action
            null;

         when others =>
            --  Do the default action
            null;
      end case;

      --  Just pass the buck to the "Readable_Subkind" function
      Put (Stream, Readable_Subkind (Kind.Subkind));

      --  If it's a precondition, tack on the index as well
      if Include_Precond_Index
        and then Kind.Subkind = Precondition_Annotation
      then
         Format (Stream, " #%1", Image (Integer (Kind.Index)));
      end if;
   end Put_Readable_Message_Kind;

   function Readable_Message_Kind
     (Kind                  : BE_Message_Kind'Class;
      Include_Precond_Index : Boolean := False)
      return                  String
   is
      Stream : Output.Streams.Storage.Stream;
   begin
      Put_Readable_Message_Kind (Stream, Kind, Include_Precond_Index);
      return Output.Streams.Storage.Contents (Stream);
   end Readable_Message_Kind;

   --------------------------
   -- Put_Readable_Ranking --
   --------------------------

   procedure Put_Readable_Ranking
     (Stream : in out Output.Streams.Stream'Class;
      Rank   : Message_Ranking_Level)
   is

   begin
      Put (Stream, Capitalize (Rank'Img));
   end Put_Readable_Ranking;

   ----------------------
   -- Readable_Ranking --
   ----------------------

   function Readable_Ranking
     (Rank : Message_Ranking_Level) return String
   is
      --  A user-readable indication of the ranking of an error.
      Stream : Output.Streams.Storage.Stream;

   begin
      Put_Readable_Ranking (Stream, Rank);

      return Output.Streams.Storage.Contents (Stream);
   end Readable_Ranking;

   -----------------------
   -- Put_Readable_Text --
   -----------------------

   procedure Put_Readable_Text
     (Stream : in out Output.Streams.Stream'Class;
      M      : Message) is
   begin
      if M.Kind.all not in BE_Message_Kind'Class then
         Put_Message (Stream, M);
      end if;
      declare
         Kind : BE_Message_Kind renames BE_Message_Kind (M.Kind.all);
      begin
         case Kind.Subkind is
         when Annotation_Subkind              |
              Locally_Unused_Store_Annotation |
              Test_Vector_Annotation          |
              Unknown_Call_Annotation         =>
            case Kind.Subkind is
               when Module_Annotation | End_Module_Annotation =>
                  pragma Assert
                    (M.Text = No_Spelling or else String_Ref (M.Text).S = "");
                  Format
                    (Stream,
                     ": %1",
                     Use_Dots (String_Ref (Kind.Module_Name).S));
               when Procedure_Annotation | End_Procedure_Annotation =>
                  pragma Assert
                    (M.Text = No_Spelling or else String_Ref (M.Text).S = "");
                  Format
                    (Stream,
                     " %1",
                     Procedure_Name
                        (Kind,
                         Current_Language,
                         Include_Module  => False,
                         Include_Profile => True));
               when Unknown_Call_Annotation =>
                  pragma Assert
                    (M.Text /= No_Spelling
                    and then Is_Prefix
                                ("Effects-of-calling:",
                                 String_Ref (M.Text).S));
                  Format
                    (Stream,
                     "call on %1",
                     Remove_Prefix
                        ("Effects-of-calling:",
                         String_Ref (M.Text).S));

               when others =>
                  Format (Stream, "%1", String_Ref (M.Text).S);
            end case;

         when Local_Check | Security_Check_Subkind =>
            Format (Stream, "%1", String_Ref (Kind.Assertion_Image).S);
            pragma Assert
              (M.Text = No_Spelling or else String_Ref (M.Text).S = "");
            null;
         --  No need to print M.Text.

         when Pre_Or_Post_Check =>
            --  TBD: The callee Procedure_Name should be removed from this,
            --  and put in its own separate (clickable?) column in the table.

            if Current_Language = Ada_Language then
               Format
                 (Stream,
                  "on call to %1: requires %2",
                  Procedure_Name
                     (Kind.Callee,
                      Include_Profile => False,
                      Module_Name     => String_Ref (Kind.Callee_Module).S),
                  String_Ref (Kind.Callee_Assertion).S
                    --  If a precondition violation occurs in an unrolled
                    --  for-loop then Assertion_Image may contain iteration
                    --  info which we want to include.
                    & Iteration_Identification.Image (
                        Iteration_Identification.Trailing_Iteration_Id
                        (String_Ref (Kind.Assertion_Image).S)));
            else
               Format
                 (Stream,
                  "%1: %2",
                  Procedure_Name
                     (Kind.Callee,
                      Include_Profile => False,
                      Module_Name     => String_Ref (Kind.Callee_Module).S),
                  String_Ref (Kind.Callee_Assertion).S);
            end if;

            pragma Assert
              (M.Text = No_Spelling or else String_Ref (M.Text).S = "");
            null;
         --  No need to print M.Text.

         when Suspicious_Precondition_Warning       |
              Suspicious_Range_Precondition_Warning =>
            declare
               VN_Name : String renames
                 String_Ref (Kind.Suspicious_VN_Image).S;
            begin
               if Current_Language = Ada_Language then
                  Format
                    (Stream,
                     "for %1: not a contiguous range of values",
                     VN_Name);
               else
                  Format
                    (Stream,
                     "the precondition for %1 is not a " &
                     "contiguous range of values",
                     VN_Name);
               end if;
            end;

         when Suspicious_First_Precondition_Warning =>
            declare
               VN_Name : String renames
                 String_Ref (Kind.Suspicious_VN_Image).S;
            begin
               pragma Assert (Is_Suffix (VN_Name, Suffix => "'First"));
               Format
                 (Stream,
                  "on %1: might assume lower bound of %2",
                  VN_Name,
                  Image (Kind.Max_Value));
            end;

         when Suspicious_Input_Warning =>
            declare
               Obj_Name : String renames
                 String_Ref (Kind.Suspicious_Obj_Image).S;
            begin
               Format
                 (Stream,
                  "%1: depends on input value of out-parameter",
                  Obj_Name);
            end;

         when Unread_In_Out_Parameter_Warning =>
            declare
               Obj_Name : String renames
                 String_Ref (Kind.Suspicious_Obj_Image).S;
            begin
               Format
                 (Stream,
                  "%1: could have mode out",
                  Obj_Name);
            end;

         when Unassigned_In_Out_Parameter_Warning =>
            declare
               Obj_Name : String renames
                 String_Ref (Kind.Suspicious_Obj_Image).S;
            begin
               Format
                 (Stream,
                  "%1: could have mode in",
                  Obj_Name);
            end;

         when Suspicious_Constant_Operation_Warning =>
            declare
               VN_Name    : String renames
                 String_Ref (Kind.Suspicious_Constant_VN_Image).S;
               Value_Name : String renames
                 String_Ref (Kind.Suspicious_Constant_Value_Image).S;
            begin
               Format
                 (Stream,
                  "%1 always evaluates to %2",
                  VN_Name,
                  Value_Name);
            end;

         when Unknown_Call_Warning | Non_Analyzed_Call_Warning =>
            --  Note: if changing this message, also need to change the
            --  MessagePatterns.xml entry that recognizes it.
            Format
              (Stream,
               "-- call on %1",
               Procedure_Name
                  (Kind.Unknown_Callee,
                   Include_Profile => True,
                   Module_Name     =>
                     String_Ref (Kind.Unknown_Callee_Module).S));

         when Procedure_Does_Not_Return_Error |
              Check_Fails_On_Every_Call_Error =>
            Format (Stream, "%1", String_Ref (M.Text).S);

         when Analyzed_Module_Warning |
           Non_Analyzed_Module_Warning =>
            pragma Assert
               (M.Text = No_Spelling or else String_Ref (M.Text).S = "");
            Format
               (Stream,
               ": %1",
               Use_Dots (String_Ref (Kind.Module_Name).S));
         when Non_Analyzed_Procedure_Warning |
           Incompletely_Analyzed_Procedure_Warning =>
            pragma Assert
               (M.Text = No_Spelling or else String_Ref (M.Text).S = "");
            Format
               (Stream,
               ": %1",
               Procedure_Name
                  (Kind,
                     Current_Language,
                     Include_Module  => False,
                     Include_Profile => True));
         when Unlocked_Reentrant_Update_Error =>
            if Current_Language = Ada_Language then
               Format
                 (Stream,
                  "of %1 via %2",
                  Arg1 => To_String (Kind.Obj_In_Trouble_Name),
                  Arg2 => To_String (Kind.Updater_Name));
            else
               Format
                 (Stream,
                  "usage of %1 via reentrant %3 %2; " &
                  "unlocked usages found",
                  Arg1 => To_String (Kind.Obj_In_Trouble_Name),
                  Arg2 => To_String (Kind.Updater_Name),
                  Arg3 => Readable_Subkind (Procedure_Annotation));
            end if;

         when Unlocked_Shared_Daemon_Update_Error =>
            if Current_Language = Ada_Language then
               Format
                 (Stream,
                  "of %1 via %2",
                  Arg1 => To_String (Kind.Obj_In_Trouble_Name),
                  Arg2 => To_String (Kind.Updater_Name));
            else
               Format
                 (Stream,
                  "usage of shared object %1 via %2; " &
                  "unlocked usages found",
                  To_String (Kind.Obj_In_Trouble_Name),
                  To_String (Kind.Updater_Name));
            end if;

         when Mismatched_Locked_Update_Error =>
            if Current_Language = Ada_Language then
               Format
                 (Stream,
                  "of shared object %1 via %2",
                  Arg1 => To_String (Kind.Obj_In_Trouble_Name),
                  Arg2 => To_String (Kind.Updater_Name));
            else
               Format
                 (Stream,
                  "usage of shared object %1 via %2; " &
                  "mismatched locking found",
                  To_String (Kind.Obj_In_Trouble_Name),
                  To_String (Kind.Updater_Name));
            end if;

         when Dead_Store_Warning =>
            Format
              (Stream,
               "into %1",
               String_Ref (M.Text).S);

         when Dead_Outparam_Store_Warning =>
            Format
              (Stream,
               "%1",
               String_Ref (M.Text).S);
         when Potentially_Dead_Store_Warning =>
            Format
              (Stream,
               "%1",
               String_Ref (M.Text).S);
         when Same_Value_Dead_Store_Warning =>
            Format
              (Stream,
               "into %1",
               String_Ref (M.Text).S);

         when Dead_Block_Warning =>
            if M.Text /= No_Spelling
              and then M.Text /= Empty_Spelling
              and then String_Ref (M.Text).S /= "true"
            then
               Format
                 (Stream,
                  "because %1",
                  Arg1 => String_Ref (M.Text).S);
            else
               Format (Stream, "begins here");
            end if;

         when Infinite_Loop_Warning =>
            if M.Text /= No_Spelling
              and then M.Text /= Empty_Spelling
              and then String_Ref (M.Text).S /= "true"
            then
               Format
                 (Stream,
                  "because %1",
                  Arg1 => String_Ref (M.Text).S);
            end if;

         when Dead_Block_Continuation_Warning =>
            if M.Text /= No_Spelling and then M.Text /= Empty_Spelling then
               Format
                 (Stream,
                  "because %1",
                  Arg1 => String_Ref (M.Text).S);
            end if;

         when True_Condition_Dead_Edge_Warning =>
            Format
              (Stream,
               "because %1 is always true",
               String_Ref (M.Text).S);

         when False_Condition_Dead_Edge_Warning =>
            Format
              (Stream,
               "because %1 is always false",
               String_Ref (M.Text).S);

         when Unrepeatable_While_Loop_Warning =>
            Format
              (Stream,
               "because after first iteration %1",
               String_Ref (M.Text).S);

         when Decision_Dead_Edge_Subkind =>
            if M.Text /= No_Spelling
              and then M.Text /= Empty_Spelling
              and then String_Ref (M.Text).S /= "true"
            then
               Format
                 (Stream,
                  "because %1",
                  Arg1 => String_Ref (M.Text).S);
            end if;

         when Local_Lock_Of_Global_Object =>
            Format
              (Stream,
               "global %1 has a local lock",
               String_Ref (Kind.Updated_Obj).S);

         end case;
      end;
   end Put_Readable_Text;

   function Readable_Text (M : Message) return String is
      Stream : Output.Streams.Storage.Stream;
   begin
      Put_Readable_Text (Stream, M);
      return Output.Streams.Storage.Contents (Stream);
   end Readable_Text;

   procedure Put_Readable_Likelihood
     (Stream : in out Output.Streams.Stream'Class;
      L      : Error_Likelihood_Enum)
   is
   begin
      Put (Stream, Error_Likelihood_Enum'Image (L));
   end Put_Readable_Likelihood;

   function Readable_Likelihood (L : Error_Likelihood_Enum) return String is
      --  A user-readable indication of the Likelihood of an error.
      Stream : Output.Streams.Storage.Stream;
   begin
      Put_Readable_Likelihood (Stream, L);
      return Output.Streams.Storage.Contents (Stream);
   end Readable_Likelihood;

   function Is_Error (Kind : BE_Message_Kind) return Boolean is
   --  Return True if Message is considered an "error" as opposed
   --  to a "warning" or some kind of informational message or annotation.
   --  These are the "check-related" messages, that indicate a particular
   --  check failed, or might have failed.
   begin
      return Kind.Subkind in Error_Subkind
            or else Kind.Subkind in Security_Check_Subkind;
   end Is_Error;

   function Strip_Line_Number_From_Key (Key : String) return String
      --  some unknown_call return values include the call's source_position,
      --  which we do not want in a key. We'll remove it directly from the key,
      --  and leave the vn_image and message text intact.
        is
      Result          : String (1 .. Key'Length);
      Last            : Natural          := 0;
      Unknown_Pos_Str : constant String  := "@line ??";
      Unknown_Pos_Len : constant Natural := Unknown_Pos_Str'Length;
   begin
      if Contains (Key, '@') then
         --  remove any line numbers
         declare
            I : Natural := Key'First;
         begin
            while I <= Key'Last loop
               if Key (I) /= '@' then
                  --  Nothing to strip
                  Last          := Last + 1;
                  Result (Last) := Key (I);
                  I             := I + 1;
               elsif (I - 1) + Unknown_Pos_Len <= Key'Last
                 and then Key (I .. I + Unknown_Pos_Len - 1) =
                          Unknown_Pos_Str
               then
                  --  Recognize special case of unknown line number
                  I := I + Unknown_Pos_Len + 1;

               elsif I < Key'Last and then Key (I + 1) in '0' .. '9' then
                  --  @ could be part of a regular string. at least make
                  --  sure it is followed by some number before stripping
                  --  it.
                  I := I + 1;
                  while I <= Key'Last
                    and then (Key (I) in '0' .. '9' or else Key (I) = ':')
                  loop
                     --  skip digits and ':' separating line from column
                     I := I + 1;
                     --  skip the numbers
                  end loop;
               else
                  --  '@' not followed by line number
                  Last          := Last + 1;
                  Result (Last) := '@';
                  I             := I + 1;
               end if;
            end loop;
         end;
         return Result (1 .. Last);
      else
         return Key;
      end if;
   end Strip_Line_Number_From_Key;

   function Create_Message_Key
     (Kind : BE_Message_Kind;
      M    : Message)
      return Spelling
   is
      Stream : Output.Streams.Storage.Stream;
   begin
      --  The key includes the full procedure name (including module name).
      --  For failing checks, it also includes the VN and the Bad value set.
      --  This should be enough to make them unique.

      case Kind.Subkind is
         when Module_Annotation | End_Module_Annotation =>
            return Intern ("");
         when Check_Kind_Enum | Security_Check_Subkind =>
            --  remove line number associated with unknown_calls
            if Inspection.Old_Style_Keys then
               Format
                 (Stream,
                  "%1 could be in %2",
                  Strip_Line_Number_From_Key (String_Ref (Kind.VN_Image).S),
                  String_Ref (Kind.Bad).S);
            else
               Format
                 (Stream,
                  "%1 could be in %2",
                  Strip_Line_Number_From_Key
                     (String_Ref (Kind.Assertion_Image).S),
                  String_Ref (Kind.Bad).S);
            end if;
         when Method_Annotation_Subkind       |
              Locally_Unused_Store_Annotation |
              Unknown_Call_Annotation         |
              Test_Vector_Annotation          =>
            --  all annotations, including input, output and new_obj
            Format (Stream, "%1", To_String (M.Text));

            --  Note: cannot remove line_numbers associated with unknown_calls
            --  because we can have multiple presumptions calling the same
            --  unknown methos on different lines
            --  no longer needed, the message itself has "soft"

         when Suspicious_Precondition_Subkind       |
              Suspicious_Input_Warning              |
              Suspicious_Constant_Operation_Warning |
              Unread_In_Out_Parameter_Warning       |
              Unassigned_In_Out_Parameter_Warning   =>
            null;
         --  we do not want the index, as it is dependent on the version of
         --  the source
         --  it must have been needed to differentiate 2 suspicious
         --  preconditions with the same vn_image and trying to link the
         --  error message and the precondition.
         --  the warning to the actual precond. The index number is still in
         --  the suspicious_preconditions table
         --
         when Unknown_Call_Warning | Non_Analyzed_Call_Warning =>
            Format (Stream, "%1", To_String (Kind.Unknown_Callee));
            --  module is included in the callee name for unknown_calls
            if Kind.Subkind = Unknown_Call_Warning then
               Format (Stream, "%1", "--call unknown");
            else
               Format (Stream, "%1", "--call too_complex");
            end if;
            return Intern (Output.Streams.Storage.Contents (Stream));
         when Non_Analyzed_Procedure_Warning |
            Incompletely_Analyzed_Procedure_Warning |
            Analyzed_Module_Warning          |
            Non_Analyzed_Module_Warning =>
            Format (Stream, "%1", To_String (M.Text));
         when Procedure_Annotation            |
              End_Procedure_Annotation        |
              Procedure_Does_Not_Return_Error |
              Check_Fails_On_Every_Call_Error =>
            null;
         when Race_Condition_Subkind =>
            Format (Stream, "%1", To_String (Kind.Obj_In_Trouble_Name));
            Format (Stream, "%1", To_String (Kind.Updater_Name));
         when Dead_Store_Warning            |
              Dead_Outparam_Store_Warning   |
              Same_Value_Dead_Store_Warning =>
            --  include object assigned into
            Format
              (Stream,
               "%1",
               Strip_Line_Number_From_Key (To_String (M.Text)));
         when Potentially_Dead_Store_Warning =>
            null;
         when Dead_Block_Warning              |
              Dead_Block_Continuation_Warning |
              Infinite_Loop_Warning           =>
            --  include basic block number
            Format (Stream, " bb:%1", To_String (Kind.Dead_BB));
         when Dead_Edge_Subkind =>
            --  include from be and only live edge
            Format
              (Stream,
               " bb:%1 => %2",
               To_String (Kind.From_BB),
               To_String (Kind.Only_Live_Edge));
         when Local_Lock_Of_Global_Object =>
            Format (Stream, "%1", To_String (Kind.Updated_Obj));
      end case;

      --  Format(Stream, "-- (");
      --  no need to add module or procedure to the key. We'll use
      --  key+module+procedure for hashing purpose

      if Kind.Subkind in Pre_Or_Post_Check then
         Format
           (Stream,
            "-%1-",
            Procedure_Name
               (Kind.Callee,
                Include_Profile => False,
                Module_Name     => String_Ref (Kind.Callee_Module).S));

         if Kind.Callee_Precondition_Index /= No_Precondition_Index then
            Format
            (Stream,
               "%1",
               Precondition_Index'Image (Kind.Callee_Precondition_Index));

         else
            --  use assertion image instead
            Format
            (Stream,
               "%1",
               To_String (Kind.Callee_Assertion));
         end if;

         if Kind.Soft_Check then
            Format (Stream, "%1", "S");
         end if;
      end if;

      return Intern (Output.Streams.Storage.Contents (Stream));
   end Create_Message_Key;

   procedure Put_Boolean_Attributes
     (Stream : in out Output.Streams.Stream'Class;
      Kind   : BE_Message_Kind)
   is
   --  Put out boolean "precomputed" attributes of message, if
   --  they exist and are "interesting" (i.e. not equal to default)
   begin
      if not Is_Annotation (Kind.Subkind)
        and then Kind.Precomputed_Attributes /=
                 Precomputed_Attributes_Default
      then
         declare
            Attribs : Precomputed_Attributes_Array renames
              Kind.Precomputed_Attributes.Boolean_Attributes;
         begin
            Format (Stream, "%n    Attribs:");
            for I in Attribs'Range loop
               if Attribs (I) then
                  case I is
                  when Is_Big_Int =>
                     Format (Stream, "  Int");
                  when Is_Big_Rat =>
                     Format (Stream, "  Rat");
                  when Is_Side_Effect =>
                     Format (Stream, "  Side-effect");
                  when Is_Pointer =>
                     Format (Stream, "  Ptr");
                  when Bad_Set_Includes_Null_Literal =>
                     Format (Stream, "  null in Bad");
                  when Bad_Set_Only_Invalid =>
                     Format (Stream, "  Bad only invalid");
                  when Expected_Set_Is_Subset_Plus_Minus_1000 =>
                     Format (Stream, "  Exp in +/-1000");
                  when Expected_Set_Is_Singleton_Set =>
                     Format (Stream, "  Exp singleton");
                  when Valid_Bad_Set_Is_Singleton_Set =>
                     Format (Stream, "  Bad singleton");
                  when Valid_Bad_Set_Overlaps_Plus_Minus_1000 =>
                     Format (Stream, "  Bad overlaps +/-1000");
                  when Check_Is_Soft =>
                     Format (Stream, "  Soft");
                  when Check_Is_Loop_Bound_Check =>
                     Format (Stream, "  Loop bnd");
                  when Bad_Values_Below_Expected =>
                     Format (Stream, "  Bad < Exp");
                  when Bad_Values_Above_Expected =>
                     Format (Stream, "  Bad > Exp");

                  --  NOTE: The following are not used yet;
                  --       they are for evaluating whether a
                  --       precondition is "suspicious"
                  when Precondition_Set_Contains_Holes =>
                     Format (Stream, "  Pre has holes");
                  when Precondition_Set_Has_Singleton_Hole_At_Zero =>
                     Format (Stream, "  Pre has hole at 0");
                  when Precondition_Expr_Is_Holey_Arithmetic =>
                     Format (Stream, "  Pre expr is holey");
                  when Message_Is_Uncertain =>
                     Format (Stream, "  Uncertain");
                  when  Message_Depends_On_Unknown_Call =>
                     Format (Stream, "  Depends on unknown call");
                  when Exception_Handler_Present =>
                     Format (Stream, "  Exception handler present");
                  when Uninteresting_RHS =>
                     Format (Stream, "  Uninteresting RHS");
                  when  Message_About_Implicit_Call =>
                     Format (Stream, "  About implicit call");
                  when Message_Depends_On_Object_Merging =>
                     Format (Stream, "  Depends on object merging");
                  when Message_Involves_Loop_Imprecision =>
                     Format (Stream, "  Involves loop imprecision");
                  when Message_Likely_False_Positive =>
                     Format (Stream, "  Likely false positive");
                  end case;
               end if;
            end loop;
         end;
      end if;
   end Put_Boolean_Attributes;

   procedure Put_Original_Checks_Of_Precondition
     (Stream : in out Output.Streams.Stream'Class;
      Kind : BE_Message_Kind)
     with Pre => Kind.Subkind in Precondition_Check
   is
   --  Puts original checks that imply the precondition check.
   begin
      if Kind.Callee_Original_Checks /= Check_Kinds_Array_Default
      then
         declare
            First : Boolean := True;
         begin
            for Check in Kind.Callee_Original_Checks'Range
            loop
               if Kind.Callee_Original_Checks (Check) then
                  if First then
                     First := False;
                  else
                     Format (Stream, ", ");
                  end if;
                  Format (Stream, "%1",
                          Readable_Subkind (Check));
               end if;
            end loop;
         end;
      end if;
   end Put_Original_Checks_Of_Precondition;

   function Original_Checks (Kind : BE_Message_Kind)
                                              return String
   is
   --  For a precondition check, returns original checks that imply this check.
   --  For other checks, returns an empty string.

      Stream : Output.Streams.Storage.Stream;
   begin
      if Kind.Subkind in Precondition_Check then
         Put_Original_Checks_Of_Precondition (Stream, Kind);
         return " (" & Output.Streams.Storage.Contents (Stream) & ")";
      else
         return "";
      end if;
   end Original_Checks;

   ------------------------
   -- Error_Message_Text --
   ------------------------

   function Error_Message_Text
     (Kind : BE_Message_Kind; M : Message) return String
   is
      use ST.BE_Message_Leveling_And_Suppression;

      Rank : constant Message_Ranking_Level := Calculate_Message_Rank (M);

   begin
      if Rank not in Error_Ranking_Level
        or else not Is_Warning_Or_Check (Kind.Subkind)
      then
         return "";
      end if;

      if Is_Warning (Kind.Subkind) then
         return
           To_Lower
             (Message_Ranking_Level'Image
               (Calculate_Message_Rank (M))) & " warning: " &
           Readable_Subkind (Kind.Subkind) & Original_Checks (Kind) & " " &
           ST.Html_Output.Improve_Number_Readability
             (Readable_Text (M), ST.Html_Output.Text);
      end if;

      pragma Assert (Is_Check (Kind.Subkind));

      declare
         Message : constant String :=
           ST.Html_Output.Improve_Error_Assertion
             (Kind.Subkind,
              Rank,
              --                String_Ref (Kind.Assertion_Image).S);
              Readable_Text (M));

      begin
         return
           To_Lower
             (Message_Ranking_Level'Image (Rank)) & ": " &
           Readable_Subkind (Kind.Subkind) & Original_Checks (Kind) &
           (if Message'Last > 0 and then Message (1) /= ':' then " " else "") &
           ST.Html_Output.Improve_Number_Readability
             (Message, ST.Html_Output.Text);
      end;
   end Error_Message_Text;

   procedure Put_Message_Method
     (Stream     : in out Output.Streams.Stream'Class;
      Kind       : BE_Message_Kind;
      M          : Message;
      Ignore_Key : Boolean := False)
   is
      use ST.BE_Message_Leveling_And_Suppression;
   begin
      case Kind.Subkind is
         when Annotation_Subkind =>
            Put_Readable_Message_Kind (Stream, Kind);
            case Annotation_Subkind'(Kind.Subkind) is
               when Module_Annotation | End_Module_Annotation =>
                  pragma Assert
                    (M.Text = No_Spelling or else String_Ref (M.Text).S = "");
                  Format
                    (Stream,
                     ": %1",
                     Use_Dots (String_Ref (Kind.Module_Name).S));
               when Procedure_Annotation | End_Procedure_Annotation =>
                  pragma Assert
                    (M.Text = No_Spelling or else String_Ref (M.Text).S = "");
                  Format
                    (Stream,
                     ": %1",
                     Procedure_Name
                        (Kind,
                         Current_Language,
                         Include_Module  => True,
                         Include_Profile => True));
               when Precondition_Annotation =>
                  --  Include the precondition index
                  Format (Stream, "[%1] ", Image (Integer (Kind.Index)));
                  Format
                    (Stream,
                     "(%1): ",
                     Procedure_Name
                        (Kind,
                         Current_Language,
                         Include_Profile => True));
                  if Kind.Pre_Original_Checks /= Check_Kinds_Array_Default
                  then
                     Format (Stream, "(");
                     declare
                        First : Boolean := True;
                     begin
                        for Check in Kind.Pre_Original_Checks'Range loop
                           if Kind.Pre_Original_Checks (Check) then
                              if First then
                                 First := False;
                              else
                                 Format (Stream, ", ");
                              end if;
                              Format (Stream, "%1",
                              Readable_Subkind (Check));
                           end if;
                        end loop;
                     end;
                     Format (Stream, ") ");
                  end if;
                  Format
                    (Stream,
                     "%1",
                     String_Ref (M.Text).S);
               when Input_Annotation         |
                    Output_Annotation        |
                    New_Obj_Annotation       |
                    Presumption_Annotation   |
                    Postcondition_Annotation =>
                  --  No index required for these
                  Format
                    (Stream,
                     "(%1): %2",
                     Procedure_Name
                        (Kind,
                         Current_Language,
                         Include_Profile => True),
                     String_Ref (M.Text).S);
            end case;
         when Non_Analyzed_Module_Warning |
           Analyzed_Module_Warning =>
            Put_Readable_Message_Kind (Stream, Kind);
            Format
               (Stream,
               ": %1",
               Use_Dots (String_Ref (Kind.Module_Name).S));
         when Unknown_Call_Annotation =>
            Put_Readable_Message_Kind (Stream, Kind);
            Format
              (Stream,
               "(%1): %2",
               Procedure_Name
                  (Kind,
                   Current_Language,
                   Include_Profile => True),
               String_Ref (M.Text).S);
         when Test_Vector_Annotation =>
            Put_Readable_Message_Kind (Stream, Kind);
            Format
              (Stream,
               "(%1): %2",
               Procedure_Name
                  (Kind,
                   Current_Language,
                   Include_Profile => True),
               String_Ref (M.Text).S);
         when Non_Analyzed_Procedure_Warning |
           Incompletely_Analyzed_Procedure_Warning =>
            Put_Readable_Message_Kind (Stream, Kind);
            Format
              (Stream,
               "(%1): %2",
               Procedure_Name
                  (Kind,
                   Current_Language,
                   Include_Profile => True),
               String_Ref (M.Text).S);
         when Locally_Unused_Store_Annotation =>
            Put_Readable_Message_Kind (Stream, Kind);
            Format
              (Stream,
               "(%1): %2",
               Procedure_Name
                  (Kind,
                   Current_Language,
                   Include_Profile => True),
               String_Ref (M.Text).S);
         when Check_Kind_Enum | Security_Check_Subkind =>
            case Kind.Likelihood is
               when Check_Will_Fail =>
                  Put (Stream, "?!");
               when Check_Might_Fail =>
                  Put (Stream, "?");
               when Check_Cannot_Fail =>
                  Put (Stream, "OK: ");
            end case;
            Put_Readable_Message_Kind (Stream, Kind);
            Format (Stream, "%n    %1", Readable_Text (M));
            Format
              (Stream,
               "%n    severity: %1",
               Message_Ranking_Level'Image (Calculate_Message_Rank (M)));
            Format
              (Stream,
               "%n    %2: %1",
               Arg1 => Use_Dots (String_Ref (Kind.Module_Name).S),
               Arg2 => Readable_Subkind (Module_Annotation));
            Format
              (Stream,
               "%n    %2: %1",
               Procedure_Name
                  (Kind,
                   Current_Language,
                   Include_Profile => True),
               Arg2 => Readable_Subkind (Procedure_Annotation));
            Format
              (Stream,
               "%n    basic block: %1",
               String_Ref (Kind.Basic_Block_Id).S);
            Format
              (Stream,
               "%n    assertion: %1",
               String_Ref (Kind.Assertion_Image).S);
            if Kind.Subkind in Pre_Or_Post_Check then
               Format
                 (Stream,
                  "%n    callee: %1",
                  Procedure_Name
                     (Kind.Callee,
                      Include_Profile => True,
                      Module_Name     => String_Ref (Kind.Callee_Module).S));
               Format
                 (Stream,
                  "%n    callee assertion: %1",
                  String_Ref (Kind.Callee_Assertion).S);
               Format
                 (Stream,
                  "%n    callee file: %1",
                  Files.Get_File_Name (String_Ref (Kind.Callee_File_Name).S));
               if Kind.Callee_Precondition_Index = Precondition_Index'Last
               then
                  Format
                   (Stream,
                     "%n    callee implicit precondition");
               else
                  Format
                   (Stream,
                     "%n    callee precondition index: [%1]",
                     Image (Integer (Kind.Callee_Precondition_Index)));
               end if;
               Format
                 (Stream,
                  "%n    callee srcpos: %1",
                  Srcpos_Image (Kind.Callee_Srcpos));
               if Kind.Subkind in Precondition_Check then
                  Format (Stream, "%n    checks: {");
                  Put_Original_Checks_Of_Precondition (Stream, Kind);
                  Format (Stream, "}");
               end if;
            end if;
            Format (Stream, "%n    VN: %1", String_Ref (Kind.VN_Image).S);
            if False then  --  stopped working with split output.
                           --  it does not seem to be very useful anyway now
                           --  that we use the SCIL to print the message text
               Format
                 (Stream,
                  "%n    SCIL: %1",
                  String_Ref (Kind.SCIL_Image).S);
            end if;
            Format
              (Stream,
               "%n    Expected: %1",
               String_Ref (Kind.Expected).S);
            Format (Stream, "%n    Bad: %1", String_Ref (Kind.Bad).S);
            Put_Boolean_Attributes (Stream, Kind);
            pragma Assert
              (M.Text = No_Spelling or else String_Ref (M.Text).S = "");
            null;
         --  No need to print M.Text.

         when Non_Analyzed_Call_Warning             |
              Unknown_Call_Warning                  |
              Suspicious_Precondition_Subkind       |
              Suspicious_Input_Warning              |
              Suspicious_Constant_Operation_Warning |
              Unread_In_Out_Parameter_Warning       |
              Unassigned_In_Out_Parameter_Warning   |
              Dead_Store_Subkind                    |
              Dead_Control_Flow_Subkind             |
              Dead_Block_Continuation_Warning       =>

            Put (Stream, "Warning: ");
            Put_Readable_Message_Kind (Stream, Kind);
            Format (Stream, "%n    %1", Readable_Text (M));
            Format
              (Stream,
               "%n    severity: %1",
               Message_Ranking_Level'Image (Calculate_Message_Rank (M)));
            Format
              (Stream,
               "%n    %2: %1",
               Use_Dots (String_Ref (Kind.Module_Name).S),
               Arg2 => Readable_Subkind (Module_Annotation));
            Format
              (Stream,
               "%n    %2: %1",
               Arg1 =>
                  Procedure_Name
                    (Kind,
                     Current_Language,
                     Include_Profile => True),
               Arg2 => Readable_Subkind (Procedure_Annotation));

            case Kind.Subkind is
               when Suspicious_Precondition_Subkind =>
                  Format
                    (Stream,
                     "%n    suspicious precondition index: [%1]",
                     Image (Integer (Kind.Suspicious_Precondition_Index)));
               when Suspicious_Input_Warning              |
                    Suspicious_Constant_Operation_Warning |
                    Unread_In_Out_Parameter_Warning       |
                    Unassigned_In_Out_Parameter_Warning   =>
                  null;
               when Unknown_Call_Warning | Non_Analyzed_Call_Warning =>
                  Format
                    (Stream,
                     "%n    unanalyzed callee: %1",
                     Procedure_Name
                        (Kind.Unknown_Callee,
                         Include_Profile => True,
                         Module_Name     =>
                           String_Ref (Kind.Unknown_Callee_Module).S));
               when Dead_Store_Subkind =>
                  null;
               when Dead_Block_Warning              |
                    Dead_Block_Continuation_Warning |
                    Infinite_Loop_Warning           =>
                  Format
                    (Stream,
                     "%n    dead bb: %1",
                     To_String (Kind.Dead_BB));
               when Dead_Edge_Subkind =>
                  Format
                    (Stream,
                     "%n    from bb: %1",
                     To_String (Kind.From_BB));
                  Format
                    (Stream,
                     "%n    live edge: %1",
                     To_String (Kind.Only_Live_Edge));
                  Format
                    (Stream,
                     "%n    tested vn: %1",
                     To_String (Kind.Tested_VN));
                  Format
                    (Stream,
                     "%n    tested vn values: %1",
                     To_String (Kind.Tested_VN_Values));
               when others =>
                  pragma Assert (False);
                  null;
            end case;

            Put_Boolean_Attributes (Stream, Kind);

         when Race_Condition_Subkind =>
            Put (Stream, "?");
            Put_Readable_Message_Kind (Stream, Kind);
            Format (Stream, ": %1", String_Ref (M.Text).S);
            Format
              (Stream,
               "%n    severity: %1",
               Message_Ranking_Level'Image (Calculate_Message_Rank (M)));
            Format
              (Stream,
               "%n    %2: %1",
               Use_Dots (String_Ref (Kind.Module_Name).S),
               Arg2 => Readable_Subkind (Module_Annotation));
            Format
              (Stream,
               "%n    %2: %1",
               Arg1 =>
                  Procedure_Name
                    (Kind,
                     Current_Language,
                     Include_Profile => True),
               Arg2 => Readable_Subkind (Procedure_Annotation));

            Put_Boolean_Attributes (Stream, Kind);

         when Procedure_Does_Not_Return_Error |
              Check_Fails_On_Every_Call_Error =>
            Put (Stream, "?");
            Put_Readable_Message_Kind (Stream, Kind);
            Format (Stream, ": %1", String_Ref (M.Text).S);
            Put_Boolean_Attributes (Stream, Kind);
         when Local_Lock_Of_Global_Object =>
            Put (Stream, "?");
            Put_Readable_Message_Kind (Stream, Kind);
            Format
              (Stream,
               "%n    Object_Being_Updated: %1",
               String_Ref (Kind.Updated_Obj).S);
            Format
              (Stream,
               "%n    severity: %1",
               Message_Ranking_Level'Image (Calculate_Message_Rank (M)));
            Put_Boolean_Attributes (Stream, Kind);

      end case;
      if Is_Proper_Spelling (M.Message_Key)
        and then (Kind.Subkind in Warning_Or_Error_Subkind
                 or else Kind.Subkind in Security_Check_Subkind)
      then
         if Ignore_Key then
            --  TBD: 5/3/2006 - I want to remove the key from the text
            --  listings, but I am not sure why
            --  we were still print something. If not needed, we can
            --  remove this
            --  Format(Stream, "%nignore: %1", String_Ref(M.Message_Key).S);
            null;
         else
            Format (Stream, "%nkey: %1", String_Ref (M.Message_Key).S);
         end if;
      end if;
   end Put_Message_Method;

   function Convert_Precomputed_Attributes_To_String
     (Attributes : Precomputed_Record)
      return       String
   is
      First_Char : constant := 1;     --  Lower bound of String
      Char_Attr  : String (
         First_Char +
         Precomputed_Message_Attribute_Kind'Pos
            (Precomputed_Message_Attribute_Kind'First) ..
         First_Char +
         Precomputed_Message_Attribute_Kind'Pos
            (Precomputed_Message_Attribute_Kind'Last));
      type Bool_Conversion is array (Boolean) of Character;
      Bool_Char : constant Bool_Conversion := ('F', 'T');
   begin
      for i in Precomputed_Message_Attribute_Kind loop
         Char_Attr (First_Char + Precomputed_Message_Attribute_Kind'Pos (i))
            := Bool_Char (Attributes.Boolean_Attributes (i));
      end loop;
      return Char_Attr;
   end Convert_Precomputed_Attributes_To_String;

   function Retrieve_Precomputed_Attributes_From_String
     (S    : String)
      return Precomputed_Record
   is
      Result     : Precomputed_Record :=
        ST.Message_Attributes.Precomputed_Attributes_Default;
      Char_Attr  : String renames S;
      First_Char : constant Integer   := Char_Attr'First;
   begin
      for i in Precomputed_Message_Attribute_Kind loop
         declare
            Str_Index : constant Integer :=
               First_Char + Precomputed_Message_Attribute_Kind'Pos (i);
         begin

            exit when Str_Index > Char_Attr'Last;  --  Rest are defaulted
            --  If we have a short string, presume new attributes
            --  have been added since this database entry was written.

            case Char_Attr (Str_Index) is
               when 'T' =>
                  Result.Boolean_Attributes (i) := True;
               when 'F' =>
                  Result.Boolean_Attributes (i) := False;
               when others =>
                  pragma Assert (False);
                  null;
            end case;
         end;
      end loop;
      return Result;
   end Retrieve_Precomputed_Attributes_From_String;

   procedure Convert_Precomputed_Attributes_To_Database_Form
     (Attributes    : Precomputed_Record;
      Database_Form : out Spelling)
   is
   begin
      Database_Form :=
         Intern (Convert_Precomputed_Attributes_To_String (Attributes));
   end Convert_Precomputed_Attributes_To_Database_Form;

   function Retrieve_Precomputed_Attributes_From_Database_Form
     (Database_Form : Spelling)
      return          Precomputed_Record
   is
   begin
      if Database_Form = No_Spelling then
         --  Must be an old database record; just use default.
         return ST.Message_Attributes.Precomputed_Attributes_Default;
      else
         return Retrieve_Precomputed_Attributes_From_String
                  (To_String (Database_Form));
      end if;
   end Retrieve_Precomputed_Attributes_From_Database_Form;

   function Retrieve_Precomputed_Attribute_From_Message
     (M    : Message;
      A    : Precomputed_Message_Attribute_Kind)
      return Boolean
   is
      pragma Unreferenced (M, A);
   begin
      --  TBD, easy: just a combination of stuff we already know how to do
      pragma Assert (False);
      return False;
   end Retrieve_Precomputed_Attribute_From_Message;

   function Convert_Check_Kinds_Array_To_String
     (Check_Kinds : Check_Kinds_Array)
      return       String is
      First_Pos : constant Integer :=
                      Check_Kind_Enum'Pos (Check_Kind_Enum'First);
      Result : String (1 ..
         Check_Kind_Enum'Pos (Check_Kind_Enum'Last) - First_Pos + 1);
                                 --  check of a "soft" precond
      type Bool_Conversion is array (Boolean) of Character;
      Bool_Char : constant Bool_Conversion := ('F', 'T');
   begin
      if Check_Kinds = Check_Kinds_Array_Default then
         return Check_Kinds_String_Default;
      end if;
      for Check in Check_Kinds'Range loop
         Result (Check_Kind_Enum'Pos (Check) - First_Pos + 1)
            := Bool_Char (Check_Kinds (Check));
      end loop;
      return Result;
   end Convert_Check_Kinds_Array_To_String;

   function Retrieve_Check_Kinds_Array_From_String
     (S    : String)
      return Check_Kinds_Array is
      Result : Check_Kinds_Array := Check_Kinds_Array_Default;
      First_Pos : constant Integer :=
                      Check_Kind_Enum'Pos (Check_Kind_Enum'First);
   begin
      if S = Check_Kinds_String_Default then
         return Check_Kinds_Array_Default;
      end if;
      for I in S'Range loop
         declare
            Ck_Index : constant Integer := I - S'First + First_Pos;
         begin
            case S (I) is
               when 'T' =>
                  Result (Check_Kind_Enum'Val (Ck_Index)) := True;
               when 'F' =>
                  Result (Check_Kind_Enum'Val (Ck_Index)) := False;
               when others =>
                  pragma Assert (False);
                  null;
            end case;
         end;
      end loop;
      return Result;
   end Retrieve_Check_Kinds_Array_From_String;

   ------------

   function Is_Warning (Subkind : BE_Message_Subkind) return Boolean is
   --  Return True if message is not considered a check, which
   --  means there is no reason to add "requires ..." in front
   --  of the text of the message.  Also, these messages are
   --  *not* counted as one of the "check-related" messages.
   begin
      return Subkind in Suspicious_Precondition_Subkind
            or else Subkind = Suspicious_Input_Warning
            or else Subkind = Suspicious_Constant_Operation_Warning
            or else Subkind = Unread_In_Out_Parameter_Warning
            or else Subkind = Unassigned_In_Out_Parameter_Warning
            or else Subkind = Non_Analyzed_Call_Warning
            or else Subkind = Procedure_Does_Not_Return_Error
            or else Subkind = Check_Fails_On_Every_Call_Error
            or else Subkind = Unknown_Call_Warning
            or else Subkind in Race_Condition_Subkind
            or else Subkind in Dead_Store_Subkind
            or else Subkind in Dead_Control_Flow_Subkind
            or else Subkind = Dead_Block_Continuation_Warning
            or else Subkind = Local_Lock_Of_Global_Object
            or else Subkind in Analyzed_Module_Warning ..
              Incompletely_Analyzed_Procedure_Warning;
   end Is_Warning;

   function Is_Check (Subkind : BE_Message_Subkind) return Boolean is
   --  Returns True is messages is a check.
   begin
      return Subkind in Check_Kind_Enum
            or else Subkind in Security_Check_Subkind;
   end Is_Check;

   function Is_Warning_Or_Check
     (Subkind : BE_Message_Subkind)
      return    Boolean
   is
   --  Return True if message is to be counted in the count of
   --  *all* messages.  This includes race condition messages,
   --  dead stores, etc.
   --  NOTE: This *also* includes "informational" messages.
   --  TBD: The name of this routine is somewhat misleading.
   begin
      return Is_Check (Subkind) or else Is_Warning (Subkind);
   end Is_Warning_Or_Check;

   function Is_Informational (Subkind : BE_Message_Subkind) return Boolean is
   --  Return True if "Subkind" is an informational message.
   --  An informational message is NOT counted in the error counts,
   --  nor is it part of the next/prev chain in the message-window.
   --  However, it is "printable", and if you click on it, in the
   --  source window, an informational message will be printed in the
   --  message window.
   begin
      return Subkind = Non_Analyzed_Call_Warning
            or else Subkind = Unknown_Call_Warning
            or else Subkind = Dead_Block_Continuation_Warning
            or else Subkind = Analyzed_Module_Warning
            or else Subkind = Non_Analyzed_Module_Warning
            or else Subkind = Non_Analyzed_Procedure_Warning
            or else Subkind = Incompletely_Analyzed_Procedure_Warning;
   end Is_Informational;

   function Is_Annotation (Subkind : BE_Message_Subkind) return Boolean is
   --  true if subkind is in annotation subkinds, or locally_unused_assignment
   begin
      return Subkind in Annotation_Subkind
            or else Subkind = Locally_Unused_Store_Annotation
            or else Subkind = Unknown_Call_Annotation
            or else Subkind = Test_Vector_Annotation;
   end Is_Annotation;

   function Is_Method_Annotation
     (Subkind : BE_Message_Subkind)
      return    Boolean
      --  true if subkind is in annotation subkinds, or
      --  locally_unused_assignment
   is
   begin
      return Subkind in Method_Annotation_Subkind
        or else Subkind = Locally_Unused_Store_Annotation
        or else Subkind = Unknown_Call_Annotation
        or else Subkind = Test_Vector_Annotation;
   end Is_Method_Annotation;

   function Is_Stored_In_DB_Method_Annotation
     (Subkind : BE_Message_Subkind)
      return    Boolean
   is
   --  same as above, but only keep certain messages
   begin
      return Subkind = Procedure_Annotation
        or else Subkind = End_Procedure_Annotation
        or else Subkind in Pre_Post_Annotation_Subkind
        or else Subkind in In_Out_Annotation_Subkind
        or else Subkind = Locally_Unused_Store_Annotation
        or else Subkind = Unknown_Call_Annotation
        or else Subkind = Test_Vector_Annotation;
   end Is_Stored_In_DB_Method_Annotation;

   procedure Set_Db_Msgs (Value : Boolean) is
   begin
      Generate_Msg_File := Value;
   end Set_Db_Msgs;

   function Db_Msgs return Boolean is
   begin
      return Generate_Msg_File;
   end Db_Msgs;

   procedure Set_Compiler_Mode (Value : Boolean) is
   begin
      Compiler_Mode_Flag := Value;
   end Set_Compiler_Mode;

   function Compiler_Mode return Boolean is
   begin
      return Compiler_Mode_Flag;
   end Compiler_Mode;

end BE.BE_Messages;
