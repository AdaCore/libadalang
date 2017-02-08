------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2016, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with System.Machine_Code; use System.Machine_Code;

--  This is the bare board version of this package for PowerPC targets

package body System.Traceback is

   procedure Call_Chain
     (Traceback   : in out System.Traceback_Entries.Tracebacks_Array;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1)
   is
      type PPC_Stack_Frame;
      --  Layout of the PowerPC stack frame, according to the ELF ABI

      type PPC_Stack_Frame_Acc is access PPC_Stack_Frame;
      pragma Convention (C, PPC_Stack_Frame_Acc);

      type PPC_Stack_Frame is record
         Back_Chain : PPC_Stack_Frame_Acc;
         --  Pointer to previous frame

         Saved_LR   : System.Address;
         --  LR save word
      end record;
      pragma Convention (C, PPC_Stack_Frame);

      Frame : PPC_Stack_Frame_Acc;
      --  Frame being processed

      LR : System.Address;
      --  Link register (return address)

      Last : Integer := Traceback'First - 1;
      --  Index of last traceback entry written to the buffer

      function builtin_frame_address
        (Level : Integer) return PPC_Stack_Frame_Acc;
      pragma Import
        (Intrinsic, builtin_frame_address, "__builtin_frame_address");

   begin
      --  We haven't filled any entry so far:

      Len := 0;

      --  Fetch pointer to the current frame:

      Frame := builtin_frame_address (0);

      --  Skip the current frame

      Frame := Frame.Back_Chain;

      --  Exclude Skip_Frames frames from the traceback. ABI has
      --  System.Null_Address as the back pointer of the shallowest frame in
      --  the stack.

      for J in 1 .. Skip_Frames loop
         Frame := Frame.Back_Chain;
         LR := Frame.Saved_LR;

         if LR = System.Null_Address or else Frame = null then

            --  Something is wrong.  Skip_Frames is greater than the number of
            --  frames on the current stack. Do not return a trace.

            return;
         end if;
      end loop;

      while Frame /= null
        and then Last < Traceback'Last
        and then Len < Max_Len
      loop
         if LR not in Exclude_Min .. Exclude_Max then

            --  Skip specified routines, if any (e.g. Ada.Exceptions)

            Last := Last + 1;
            Len := Len + 1;
            Traceback (Last) := LR;
         end if;

         Frame := Frame.Back_Chain;
         LR := Frame.Saved_LR;
      end loop;
   end Call_Chain;

end System.Traceback;
