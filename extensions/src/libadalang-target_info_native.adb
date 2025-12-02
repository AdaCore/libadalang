--
--  Copyright (C) 2022-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

package body Libadalang.Target_Info_Native is

   -----------------------------
   -- Get_Builtin_Target_Info --
   -----------------------------

  procedure Get_Builtin_Target_Info
     (Target_Name     : String;
      Target_Info     : out Target_Information;
      Has_Target_Info : out Boolean)
   is
      type Builtin_Target is
        (Unknown,
         AArch64_Linux,
         X86_Linux,
         x86_64_Linux,
         X86_Windows,
         x86_64_Windows);
      Target      : constant Builtin_Target :=
        (if Target_Name = "aarch64-linux" then AArch64_Linux
         elsif Target_Name = "x86-linux" then X86_Linux
         elsif Target_Name = "x86_64-linux" then x86_64_Linux
         elsif Target_Name = "x86_64-windows" then x86_64_Windows
         elsif Target_Name = "x86-windows" then X86_Windows
         else Unknown);

      function FT
        (Digs      : Digits_Type;
         Size      : Positive;
         Alignment : Natural) return Present_Floating_Point_Type_Information
      is (Present        => True,
          Digs           => Digs,
          Representation => IEEE_754_Binary,
          Size           => Size,
          Alignment      => Alignment);

      Is_32 : constant Boolean := Target in X86_Linux | X86_Windows;
   begin
      if Target = Unknown then
         Target_Info := Placeholder_Target_Info;
         Has_Target_Info := False;
         return;
      end if;

      Target_Info :=
        (Bits_BE                    => False,
         Bits_Per_Unit              => 8,
         Bits_Per_Word              => (if Is_32 then 32 else 64),
         Bytes_BE                   => False,
         Char_Size                  => 8,
         Double_Float_Alignment     => 0,
         Double_Scalar_Alignment    =>
           (if Target = X86_Linux then 4 else 0),
         Double_Size                => 64,
         Float_Size                 => 32,
         Float_Words_BE             => False,
         Int_Size                   => 32,
         Long_Double_Size           =>
           (case Target is
            when AArch64_Linux           => 64,
            when X86_Linux | X86_Windows => 96,
            when others                  => 128),
         Long_Long_Long_Size        => (if Is_32 then 64 else 128),
         Long_Long_Size             => 64,
         Long_Size                  =>
           (if Target in AArch64_Linux | x86_64_Linux then 64 else 32),
         Maximum_Alignment          => 16,
         Max_Unaligned_Field        => 1,
         Pointer_Size               => (if Is_32 then 32 else 64),
         Short_Enums                => False,
         Short_Size                 => 16,
         Strict_Alignment           => False,
         System_Allocator_Alignment => (if Is_32 then 8 else 16),
         Wchar_T_Size               =>
           (if Target in X86_Windows | x86_64_Windows then 16 else 32),
         Words_BE                   => False,
         Floating_Point_Types       =>
           (Float_Id       => FT (6, 32, 32),
            Double_Id      => FT (15, 64, 64),
            Long_Double_Id =>
              (if Target = AArch64_Linux
               then FT (33, 128, 128)
               else FT (18, 80, (if Is_32 then 32 else 128))),
            HF_Id          => FT (3, 16, 16),
            BF_Id          => FT (2, 16, 16),
            TF_Id          =>
              (if Target = AArch64_Linux
               then Absent_Floating_Point_Type_Info
               else FT (33, 128, 128)),
            others         => Absent_Floating_Point_Type_Info));
      Has_Target_Info := True;
   end Get_Builtin_Target_Info;

end Libadalang.Target_Info_Native;
