--
--  Copyright (C) 2022-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides support for GNAT's target dependent information files
--  (see GNAT's ``-gnatet`` and ``gnateT`` switches).

with Langkit_Support.Text; use Langkit_Support.Text;

package Libadalang.Target_Info is

   type Float_Representation is (IEEE_754_Binary);

   subtype Digits_Type is Positive range 1 .. 33;
   --  Number of digits supported in a floating point type

   type Floating_Point_Type_Information (Present : Boolean := False) is record
      case Present is
         when False =>
            null;

         when True =>
            Digs : Digits_Type;
            --  Number of digits for the floating-point type

            Representation : Float_Representation;
            --  Machine representation for the floating-point type

            Size : Positive;
            --  Size in bits for the floating-point type

            Alignment : Natural;
            --  Alignment for the floating-point type
      end case;
   end record;
   subtype Present_Floating_Point_Type_Information is
     Floating_Point_Type_Information (True);

   Absent_Floating_Point_Type_Info : constant Floating_Point_Type_Information
     := (Present => False);

   function Ada_Type_Definition
     (Self : Present_Floating_Point_Type_Information) return Text_Type;
   --  Return Ada code excerpt that can be used to define a floating point type
   --  with the given characteristics.

   type Floating_Point_Type_Id is
     (Float_Id,
      Double_Id,
      Long_Double_Id,
      HF_Id,
      BF_Id,
      TF_Id);
   --  List of all the floating-point types that can be described in a
   --  Target_Information record.

   function C_Name (Self : Floating_Point_Type_Id) return String
   is (case Self is
       when Float_Id       => "float",
       when Double_Id      => "double",
       when Long_Double_Id => "long double",
       when HF_Id          => "HF",
       when BF_Id          => "BF",
       when TF_Id          => "TF");

   subtype Mandatory_Floating_Point_Type_Id is
     Floating_Point_Type_Id range Float_Id .. Long_Double_Id;
   --  Floating-point type that are required in a target information file

   type Floating_Point_Types_Information is
     array (Floating_Point_Type_Id) of Floating_Point_Type_Information;

   type Target_Information is record
      Bits_BE : Boolean;
      --  Bits stored big-endian?

      Bits_Per_Unit : Positive;
      --  Bits in a storage unit

      Bits_Per_Word : Positive;
      --  Bits in a word

      Bytes_BE : Boolean;
      --  Bytes stored big-endian?

      Char_Size : Positive;
      --  Standard.Character'Size

      Double_Float_Alignment : Natural;
      --  Alignment of double float

      Double_Scalar_Alignment : Natural;
      --  Alignment of double length scalar

      Double_Size : Positive;
      --  Standard.Long_Float'Size

      Float_Size : Positive;
      --  Standard.Float'Size

      Float_Words_BE : Boolean;
      --  Float words stored big-endian?

      Int_Size : Positive;
      --  Standard.Integer'Size

      Long_Double_Size : Positive;
      --  Standard.Long_Long_Float'Size

      Long_Long_Long_Size : Positive;
      --  Standard.Long_Long_Long_Integer'Size

      Long_Long_Size : Positive;
      --  Standard.Long_Long_Integer'Size

      Long_Size : Positive;
      --  Standard.Long_Integer'Size

      Maximum_Alignment : Positive;
      --  Maximum permitted alignment

      Max_Unaligned_Field : Positive;
      --  Maximum size for unaligned bit field

      Pointer_Size : Positive;
      --  System.Address'Size

      Short_Enums : Boolean;
      --  Foreign enums use short size?

      Short_Size : Positive;
      --  Standard.Short_Integer'Size

      Strict_Alignment : Boolean;
      --  Strict alignment?

      System_Allocator_Alignment : Natural;
      --  Alignment for malloc calls

      Wchar_T_Size : Positive;
      --  Interfaces.C.wchar_t'Size

      Words_BE : Boolean;
      --  Words stored big-endian?

      Floating_Point_Types : Floating_Point_Types_Information;
      --  Information about floating-point types
   end record;

   Placeholder_Target_Info : constant Target_Information :=
     (Bits_BE                    => False,
      Bits_Per_Unit              => 8,
      Bits_Per_Word              => 64,
      Bytes_BE                   => False,
      Char_Size                  => 8,
      Double_Float_Alignment     => 0,
      Double_Scalar_Alignment    => 0,
      Double_Size                => 64,
      Float_Size                 => 32,
      Float_Words_BE             => False,
      Int_Size                   => 32,
      Long_Double_Size           => 128,
      Long_Long_Long_Size        => 128,
      Long_Long_Size             => 64,
      Long_Size                  => 64,
      Maximum_Alignment          => 16,
      Max_Unaligned_Field        => 1,
      Pointer_Size               => 64,
      Short_Enums                => False,
      Short_Size                 => 16,
      Strict_Alignment           => False,
      System_Allocator_Alignment => 16,
      Wchar_T_Size               => 32,
      Words_BE                   => False,
      Floating_Point_Types       =>
        (Float_Id       => (Present        => True,
                            Digs           => 6,
                            Representation => IEEE_754_Binary,
                            Size           => 32,
                            Alignment      => 32),
         Double_Id      => (Present        => True,
                            Digs           => 15,
                            Representation => IEEE_754_Binary,
                            Size           => 64,
                            Alignment      => 64),
         Long_Double_Id => (Present        => True,
                            Digs           => 18,
                            Representation => IEEE_754_Binary,
                            Size           => 80,
                            Alignment      => 128),
         HF_Id          => Absent_Floating_Point_Type_Info,
         BF_Id          => Absent_Floating_Point_Type_Info,
         TF_Id          => Absent_Floating_Point_Type_Info));
   --  Default target information to use when no specific values were provided.
   --  This is just a placeholder: actual values are not guaranteed to remain
   --  unchanged across versions.

   function Load (Filename : String) return Target_Information;
   --  Read target information from ``Filename`` and return it. Raise an
   --  ``Ada.IO_Exceptions.Name_Error`` or ``Ada.IO_Exceptions.Use_Error`` if
   --  ``Filename`` cannot be read, and raise an
   --  ``Langkit_Support.Errors.Invalid_Input`` exception if there is any
   --  trouble decoding it.

   procedure Dump (Self : Target_Information);
   --  Debug helper: dump the infomation contained in ``Self`` to the standard
   --  output.

end Libadalang.Target_Info;
