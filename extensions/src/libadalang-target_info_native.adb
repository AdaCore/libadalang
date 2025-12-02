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
        (Unknown, Linux_32, Linux_64, Windows_32, Windows_64);
      Target      : constant Builtin_Target :=
        (if Target_Name = "x86-linux" then Linux_32
         elsif Target_Name = "x86_64-linux" then Linux_64
         elsif Target_Name = "x86_64-windows" then Windows_64
         elsif Target_Name = "x86-windows" then Windows_32
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

      Is_32 : constant Boolean := Target in Linux_32 | Windows_32;
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
         Double_Scalar_Alignment    => (if Target = Linux_32 then 4 else 0),
         Double_Size                => 64,
         Float_Size                 => 32,
         Float_Words_BE             => False,
         Int_Size                   => 32,
         Long_Double_Size           => (if Is_32 then 96 else 128),
         Long_Long_Long_Size        => (if Is_32 then 64 else 128),
         Long_Long_Size             => 64,
         Long_Size                  => (if Target = Linux_64 then 64 else 32),
         Maximum_Alignment          => 16,
         Max_Unaligned_Field        => 1,
         Pointer_Size               => (if Is_32 then 32 else 64),
         Short_Enums                => False,
         Short_Size                 => 16,
         Strict_Alignment           => False,
         System_Allocator_Alignment => (if Is_32 then 8 else 16),
         Wchar_T_Size               =>
           (if Target in Windows_32 | Windows_64 then 16 else 32),
         Words_BE                   => False,
         Floating_Point_Types       =>
           (Float_Id       => FT (6, 32, 32),
            Double_Id      => FT (15, 64, 64),
            Long_Double_Id => FT (18, 80, (if Is_32 then 32 else 128)),
            HF_Id          => FT (3, 16, 16),
            BF_Id          => FT (2, 16, 16),
            TF_Id          => FT (33, 128, 128),
            others         => Absent_Floating_Point_Type_Info));
      Has_Target_Info := True;
   end Get_Builtin_Target_Info;

end Libadalang.Target_Info_Native;
