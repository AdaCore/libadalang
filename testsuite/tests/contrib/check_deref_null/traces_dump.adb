------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Coverage;
with Binary_Files; use Binary_Files;
with Disa_Symbolize;
with Elf_Disassemblers; use Elf_Disassemblers;
with Hex_Images;
with Traces_Names; use Traces_Names;
with Traces_Dbase; use Traces_Dbase;
with Traces_Disa;
with Traces_Lines; use Traces_Lines;
with Traces_Elf; use Traces_Elf;

package body Traces_Dump is

   --------------------------
   -- Dump_Routines_Traces --
   --------------------------

   procedure Dump_Routines_Traces (Output_Name : String_Access)
   is
      use Traces_Disa;

      Routine_Seen : Boolean := False;

      procedure Process_One
        (Key  : Subprogram_Key;
         Info : in out Subprogram_Info);
      --  Display traces for one routine

      -----------------
      -- Process_One --
      -----------------

      procedure Process_One
        (Key  : Subprogram_Key;
         Info : in out Subprogram_Info)
      is
         use Hex_Images;

         I_Ranges : Insn_Set_Ranges renames
           Get_Insn_Set_Ranges (Info.Exec.all, Info.Section).all;

      begin
         Routine_Seen := True;
         Put (Key_To_Name (Key).all);

         if Info.Traces /= null then
            Put (' ');
            Put (State_Char (Compute_Routine_State (Info.Insns, Info.Traces)));
         end if;

         if Is_Loaded (Info.Insns) then
            Put (": " & Hex_Image (Info.Insns.First)
                 & '-' & Hex_Image (Info.Insns.Last));
         end if;
         New_Line;

         if Info.Traces /= null then
            if Flag_Show_Asm then
               if Info.Exec = null then
                  Disp_Assembly_Lines
                    (Info.Insns, I_Ranges,
                     Info.Traces.all,
                     Textio_Disassemble_Cb'Access,
                     Disa_Symbolize.Nul_Symbolizer);

               else
                  Disp_Assembly_Lines
                    (Info.Insns, I_Ranges,
                     Info.Traces.all,
                     Textio_Disassemble_Cb'Access,
                     Info.Exec.all);
               end if;
            end if;
         end if;
      end Process_One;

      --  We resort to a number of routines and internal iterators that use
      --  Text_IO.Put to dump stuff without any consideration for a custom
      --  output stream. To support '--output' for the Asm format, we just
      --  temporarily redirect the default Output stream here.
      --
      --  The --annotate=asm + --output combination is infrequent and not
      --  worth the burden of a more general solution with invasive intrusions
      --  in internal services.

      Entry_Output : constant File_Access := Current_Output;
      Output_File : File_Type;

   --  Start of processing for Dump_Routines_Traces

   begin
      if Output_Name /= null then
         Create (Output_File, Out_File, Output_Name.all);
         Set_Output (Output_File);
      end if;

      Put_Line ("Coverage level: " & Coverage.Coverage_Option_Value);
      Iterate (Process_One'Access);

      if not Routine_Seen then
         Put_Line ("*** No routine of interest");
      end if;

      Set_Output (Entry_Output.all);
   end Dump_Routines_Traces;

   -----------------------------
   -- Dump_Uncovered_Routines --
   -----------------------------

   procedure Dump_Uncovered_Routines (Report : File_Access) is
      use Traces_Disa;

      procedure Process_One
        (Key  : Subprogram_Key;
         Info : in out Subprogram_Info);
      --  Report information for the given routine

      -----------------
      -- Process_One --
      -----------------

      procedure Process_One
        (Key  : Subprogram_Key;
         Info : in out Subprogram_Info)
      is
         Routine_State : constant Line_State :=
                           Compute_Routine_State (Info.Insns, Info.Traces);
      begin
         if not Is_Loaded (Info.Insns) then
            Put_Line
              (Report.all,
               Key_To_Name (Key).all & " not found in executable(s)");

         elsif Routine_State /= Covered
           and then Routine_State /= No_Code
         then
            Put
              (Report.all,
               Key_To_Name (Key).all & " not fully covered : ");
            Put (Report.all, State_Char (Routine_State));
            New_Line (Report.all);
         end if;
      end Process_One;

   --  Start of processing for Dump_Uncovered_Routines

   begin
      Put_Line (Report.all, "ERRORS BY ROUTINE:");
      New_Line (Report.all);
      Iterate (Process_One'Access);
      New_Line (Report.all);
   end Dump_Uncovered_Routines;

end Traces_Dump;
