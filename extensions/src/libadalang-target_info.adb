--
--  Copyright (C) 2022-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;

with GNATCOLL.GMP;                  use GNATCOLL.GMP;
with GNATCOLL.GMP.Integers;         use GNATCOLL.GMP.Integers;
with GNATCOLL.GMP.Rational_Numbers; use GNATCOLL.GMP.Rational_Numbers;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Libadalang.Sources;     use Libadalang.Sources;

package body Libadalang.Target_Info is

   Variable_Line_Pattern : constant Pattern_Matcher :=
     Compile ("^([a-zA-Z_]+)  +([0-9]+)$");

   Floating_Point_Line_Pattern : constant Pattern_Matcher :=
     Compile ("^([a-zA-Z_](?: ?[a-zA-Z_])+)"  --  Type name
              & "  +"
              & "([0-9]+)"                    --  Digits
              & " +"
              & "I"                           --  Representation
              & " +"
              & "([0-9]+)"                    --  Size (in bits)
              & " +"
              & "([0-9]+)"                    --  Alignment (in bits)
              & "$");

   -------------------------
   -- Ada_Type_Definition --
   -------------------------

   function Ada_Type_Definition
     (Self : Present_Floating_Point_Type_Information) return Text_Type
   is
      --  Floating point bounds computation below is adapted from similar code
      --  in GNAT's Einfo.Utils (Set_Float_Bounds) and Cstand
      --  (Machine_Emax_Value, Machine_Mantissa_Value) units.

      Radix       : constant Big_Integer := Make ("2");
      Significand : Big_Integer;
      Exponent    : Positive;

      First_Int, Last_Int : Big_Integer;
      First_Real, Last_Real : Rational;
   begin
      --  The number of digits in a floating point determines the number of
      --  bits in the mantissa and in the exponent (and thus their maximum
      --  values).

      declare
         Mantissa_Bits : Positive;
         Exponent_Max  : Positive;
      begin
         case Self.Digs is
            when 1 .. 6   =>
               Mantissa_Bits := 24;
               Exponent_Max := 128;

            when 7 .. 15  =>
               Mantissa_Bits := 53;
               Exponent_Max := 2 ** 10;

            when 16 .. 18 =>
               Mantissa_Bits := 64;
               Exponent_Max := 2 ** 14;

            when 19 .. 33 =>
               Mantissa_Bits := 113;
               Exponent_Max := 2 ** 14;
         end case;
         Exponent := Exponent_Max - Mantissa_Bits;
         Significand.Set (Radix ** Unsigned_Long (Mantissa_Bits) - 1);
      end;

      Last_Int.Set (Significand * Radix ** Unsigned_Long (Exponent));
      First_Int.Set (-Last_Int);

      First_Real.Set (1, 1);
      First_Real.Set_Num (First_Int);
      Last_Real.Set (1, 1);
      Last_Real.Set_Num (Last_Int);

      return
        "digits" & To_Text (Self.Digs'Image) & " range "
        & Encode_Real (First_Real, Base => 16) & " .. "
        & Encode_Real (Last_Real, Base => 16);
   end Ada_Type_Definition;

   ----------
   -- Load --
   ----------

   function Load (Filename : String) return Target_Information is
      F    : File_Type;
      Line : Positive := 1;

      --  Helpers to implement Put_Line_Back

      Last_Line : Unbounded_String;
      --  Track the content of the previous line that was read (if available)

      Has_Last_Line : Boolean := False;
      --  Whether Last_Line is available

      Has_Put_Last_Line_Back : Boolean := False;
      --  Whether Put_Line_Back was called since the last call to Read_Line

      function Read_Line return String;
      --  Read a line from F and return it. If the line is too long, raise an
      --  Invalid_Input exception. Propagate the End_Error exception if there
      --  was no content before the end of file.

      procedure Put_Line_Back
      with Pre => Has_Last_Line and not Has_Put_Last_Line_Back;
      --  Cancel the previous line read, so that the next call to Read_Line
      --  returns the same as the previous call.

      procedure Read_Natural (Name : String; Value : out Natural);
      --  Read the next line, expected to contain a name/value association with
      --  for the given Name. Put the associated Natural value in Value in case
      --  of success, raise an Invalid_Input exception otherwise.

      function Read_Natural
        (Name     : String;
         Value    : out Natural;
         Optional : Boolean) return Boolean;
      --  Like the homonym procedure, but return whether the next line read had
      --  the expected association name (if Optional is true) or raise an
      --  Invalid_Input exception (if Optional is false).

      procedure Read_Boolean (Name : String; Value : out Boolean);
      --  Likewise, but to read a Boolean specifically

      procedure Read_Positive (Name : String; Value : out Positive);
      --  Likewise, but to read a Positive specifically

      function Read_Positive
        (Name     : String;
         Value    : out Positive;
         Optional : Boolean) return Boolean;
      --  Like the Read_Natural function, but to read a Positive specifically

      procedure Read_Floating_Point_Type
        (Buffer : String;
         Id     : out Floating_Point_Type_Id;
         Info   : out Floating_Point_Type_Information);
      --  Extract the floating point type information from Buffer and assign it
      --  to Id/Info. Raise an Invalid_Input exception if anything goes wrong.

      ---------------
      -- Read_Line --
      ---------------

      function Read_Line return String is
         Buffer  : String (1 .. 81);
         Last    : Natural;
      begin
         if Has_Put_Last_Line_Back then
            Has_Put_Last_Line_Back := False;
            return To_String (Last_Line);
         end if;

         Get_Line (F, Buffer, Last);
         if Last = Buffer'Last then
            raise Invalid_Input with "line" & Line'Image & " is too long";
         end if;
         return Result : constant String := Buffer (1 .. Last) do
            Last_Line := To_Unbounded_String (Result);
            Has_Last_Line := True;
         end return;
      end Read_Line;

      -------------------
      -- Put_Line_Back --
      -------------------

      procedure Put_Line_Back is
      begin
         Has_Put_Last_Line_Back := True;
      end Put_Line_Back;

      ------------------
      -- Read_Natural --
      ------------------

      procedure Read_Natural (Name : String; Value : out Natural) is
         Dummy : constant Boolean :=
           Read_Natural (Name, Value, Optional => False);
      begin
         null;
      end Read_Natural;

      ------------------
      -- Read_Natural --
      ------------------

      function Read_Natural
        (Name     : String;
         Value    : out Natural;
         Optional : Boolean) return Boolean is
      begin
         declare
            Buffer  : constant String := Read_Line;
            Matches : Match_Array (0 .. 5);
         begin
            Match (Variable_Line_Pattern, Buffer, Matches);
            if Matches (0) = No_Match then
               raise Invalid_Input with "invalid format at line" & Line'Image;
            end if;

            declare
               Read_Name : String renames
                 Buffer (Matches (1).First .. Matches (1).Last);
               Read_Value : String renames
                 Buffer (Matches (2).First .. Matches (2).Last);
            begin
               if Read_Name /= Name then
                  if Optional then
                     Value := 0;
                     Put_Line_Back;
                     return False;
                  else
                     raise Invalid_Input with
                       Name & " expected at line" & Line'Image & " but got "
                       & Read_Name;
                  end if;
               end if;
               begin
                  Value := Natural'Value (Read_Value);
               exception
                  when Constraint_Error =>
                     raise Invalid_Input with Name & ": out of range value";
               end;
            end;
            Line := Line + 1;
            return True;
         end;
      exception
         when End_Error =>
            if Optional then
               Value := 0;
               Put_Line_Back;
               return False;
            else
               raise Invalid_Input with "entry missing for " & Name;
            end if;
      end Read_Natural;

      ------------------
      -- Read_Boolean --
      ------------------

      procedure Read_Boolean (Name : String; Value : out Boolean) is
         V : Natural;
      begin
         Read_Natural (Name, V);
         case V is
            when 0 =>
               Value := False;
            when 1 =>
               Value := True;
            when others =>
               raise Invalid_Input with Name & ": boolean value expected";
         end case;
      end Read_Boolean;

      -------------------
      -- Read_Positive --
      -------------------

      procedure Read_Positive (Name : String; Value : out Positive) is
         Dummy : constant Boolean :=
           Read_Positive (Name, Value, Optional => False);
      begin
         null;
      end Read_Positive;

      -------------------
      -- Read_Positive --
      -------------------

      function Read_Positive
        (Name     : String;
         Value    : out Positive;
         Optional : Boolean) return Boolean
      is
         V : Natural;
      begin
         if not Read_Natural (Name, V, Optional) then
            Value := 1;
            return False;
         elsif V in Positive then
            Value := V;
            return True;
         else
            raise Invalid_Input with Name & ": positive value expected";
         end if;
      end Read_Positive;

      ------------------------------
      -- Read_Floating_Point_Type --
      ------------------------------

      procedure Read_Floating_Point_Type
        (Buffer : String;
         Id     : out Floating_Point_Type_Id;
         Info   : out Floating_Point_Type_Information)
      is
         Matches : Match_Array (0 .. 4);
      begin
         Match (Floating_Point_Line_Pattern, Buffer, Matches);
         if Matches (0) = No_Match then
            raise Invalid_Input with
              "invalid floating point type description at line" & Line'Image;
         end if;

         declare
            N : String renames Buffer (Matches (1).First .. Matches (1).Last);
            D : String renames Buffer (Matches (2).First .. Matches (2).Last);
            S : String renames Buffer (Matches (3).First .. Matches (3).Last);
            A : String renames Buffer (Matches (4).First .. Matches (4).Last);

            Digits_Count : Digits_Type;
            Size         : Positive;
            Alignment    : Natural;
         begin
            if N = "float" then
               Id := Float_Id;
            elsif N = "double" then
               Id := Double_Id;
            elsif N = "long double" then
               Id := Long_Double_Id;
            elsif N = "HF" then
               Id := HF_Id;
            elsif N = "BF" then
               Id := BF_Id;
            elsif N = "TF" then
               Id := TF_Id;
            else
               raise Invalid_Input with "unknown floating point type: " & N;
            end if;
            pragma Assert (C_Name (Id) = N);

            Info := (Present => True, others => <>);

            begin
               Digits_Count := Digits_Type'Value (D);
            exception
               when Constraint_Error =>
                  raise Invalid_Input with "out of range digits for " & N;
            end;

            begin
               Size := Positive'Value (S);
            exception
               when Constraint_Error =>
                  raise Invalid_Input with "out of range size for " & N;
            end;

            begin
               Alignment := Natural'Value (A);
            exception
               when Constraint_Error =>
                  raise Invalid_Input with "out of range alignment for " & N;
            end;

            Info :=
              (Present        => True,
               Digs           => Digits_Count,
               Representation => IEEE_754_Binary,
               Size           => Size,
               Alignment      => Alignment);
         end;
      end Read_Floating_Point_Type;

      Result                  : Target_Information;
      Has_Long_Long_Long_Size : Boolean;
   begin
      Open (F, In_File, Filename);

      --  Read all name/value parameter associations

      Read_Boolean ("Bits_BE", Result.Bits_BE);
      Read_Positive ("Bits_Per_Unit", Result.Bits_Per_Unit);
      Read_Positive ("Bits_Per_Word", Result.Bits_Per_Word);
      Read_Boolean ("Bytes_BE", Result.Bytes_BE);
      Read_Positive ("Char_Size", Result.Char_Size);
      Read_Natural ("Double_Float_Alignment", Result.Double_Float_Alignment);
      Read_Natural ("Double_Scalar_Alignment", Result.Double_Scalar_Alignment);
      Read_Positive ("Double_Size", Result.Double_Size);
      Read_Positive ("Float_Size", Result.Float_Size);
      Read_Boolean ("Float_Words_BE", Result.Float_Words_BE);
      Read_Positive ("Int_Size", Result.Int_Size);
      Read_Positive ("Long_Double_Size", Result.Long_Double_Size);

      --  Like GNAT, tolerate a missing entry for Long_Long_Long_Size, and
      --  default to Long_Long_Size when it is missing.

      Has_Long_Long_Long_Size :=
        Read_Positive
           ("Long_Long_Long_Size",
            Result.Long_Long_Long_Size,
            Optional => True);

      Read_Positive ("Long_Long_Size", Result.Long_Long_Size);
      Read_Positive ("Long_Size", Result.Long_Size);
      Read_Positive ("Maximum_Alignment", Result.Maximum_Alignment);
      Read_Positive ("Max_Unaligned_Field", Result.Max_Unaligned_Field);
      Read_Positive ("Pointer_Size", Result.Pointer_Size);
      Read_Boolean ("Short_Enums", Result.Short_Enums);
      Read_Positive ("Short_Size", Result.Short_Size);
      Read_Boolean ("Strict_Alignment", Result.Strict_Alignment);
      Read_Natural
        ("System_Allocator_Alignment", Result.System_Allocator_Alignment);
      Read_Positive ("Wchar_T_Size", Result.Wchar_T_Size);
      Read_Boolean ("Words_BE", Result.Words_BE);

      if not Has_Long_Long_Long_Size then
         Result.Long_Long_Long_Size := Result.Long_Long_Size;
      end if;

      --  Read the empty line

      declare
         Success : Boolean := True;
      begin
         begin
            if Read_Line /= "" then
               Success := False;
            end if;
         exception
            when End_Error =>
               Success := False;
         end;
         if not Success then
            raise Invalid_Input with
              "empty line expected at line" & Line'Image;
         end if;
      end;

      --  Read descriptions for floating point types

      Result.Floating_Point_Types := (others => (Present => False));
      loop
         declare
            Id   : Floating_Point_Type_Id;
            Info : Floating_Point_Type_Information;
         begin
            Read_Floating_Point_Type (Read_Line, Id, Info);
            Result.Floating_Point_Types (Id) := Info;
         exception
            when End_Error =>
               exit;
         end;
      end loop;
      for Id in Mandatory_Floating_Point_Type_Id loop
         if not Result.Floating_Point_Types (Id).Present then
            raise Invalid_Input with
              "floating point type description missing for """ & C_Name (Id)
              & """";
         end if;
      end loop;

      Close (F);
      return Result;
   end Load;

   ----------
   -- Dump --
   ----------

   procedure Dump (Self : Target_Information) is
      procedure Print (Name : String; Value : String);
      procedure Print (Name : String; Value : Natural);
      procedure Print (Name : String; Value : Boolean);
      --  Print the Name/Value association

      -----------
      -- Print --
      -----------

      procedure Print (Name : String; Value : String) is
         Alignment : constant Natural := 31 - Name'Length - Value'Length;
      begin
         Put_Line (Name & (1 .. Alignment => ' ') & Value);
      end Print;

      -----------
      -- Print --
      -----------

      procedure Print (Name : String; Value : Natural) is
      begin
         Print (Name, Value'Image);
      end Print;

      -----------
      -- Print --
      -----------

      procedure Print (Name : String; Value : Boolean) is
      begin
         Print (Name, (if Value then "1" else "0"));
      end Print;
   begin
      Print ("Bits_BE", Self.Bits_BE);
      Print ("Bits_Per_Unit", Self.Bits_Per_Unit);
      Print ("Bits_Per_Word", Self.Bits_Per_Word);
      Print ("Bytes_BE", Self.Bytes_BE);
      Print ("Char_Size", Self.Char_Size);
      Print ("Double_Float_Alignment", Self.Double_Float_Alignment);
      Print ("Double_Scalar_Alignment", Self.Double_Scalar_Alignment);
      Print ("Double_Size", Self.Double_Size);
      Print ("Float_Size", Self.Float_Size);
      Print ("Float_Words_BE", Self.Float_Words_BE);
      Print ("Int_Size", Self.Int_Size);
      Print ("Long_Double_Size", Self.Long_Double_Size);
      Print ("Long_Long_Long_Size", Self.Long_Long_Long_Size);
      Print ("Long_Long_Size", Self.Long_Long_Size);
      Print ("Long_Size", Self.Long_Size);
      Print ("Maximum_Alignment", Self.Maximum_Alignment);
      Print ("Max_Unaligned_Field", Self.Max_Unaligned_Field);
      Print ("Pointer_Size", Self.Pointer_Size);
      Print ("Short_Enums", Self.Short_Enums);
      Print ("Short_Size", Self.Short_Size);
      Print ("Strict_Alignment", Self.Strict_Alignment);
      Print
        ("System_Allocator_Alignment", Self.System_Allocator_Alignment);
      Print ("Wchar_T_Size", Self.Wchar_T_Size);
      Print ("Words_BE", Self.Words_BE);
      New_Line;

      for Id in Floating_Point_Type_Id loop
         declare
            Info : Floating_Point_Type_Information renames
              Self.Floating_Point_Types (Id);
         begin
            if Info.Present then
               declare
                  Name : constant String :=
                    (case Id is
                     when Float_Id       => "float",
                     when Double_Id      => "double",
                     when Long_Double_Id => "long double",
                     when HF_Id          => "HF",
                     when BF_Id          => "BF",
                     when TF_Id          => "TF");

                  Digs      : constant String := Info.Digs'Image;
                  Size      : constant String := Info.Size'Image;
                  Alignment : constant String := Info.Alignment'Image;
               begin
                  Put (Name);
                  Put ((Name'Length .. 12 => ' '));
                  Put ((Digs'Length .. 2 => ' '));
                  Put (Digs);
                  Put ("  I");
                  Put ((Size'Length .. 3 => ' '));
                  Put (Size);
                  Put ((Alignment'Length .. 3 => ' '));
                  Put (Alignment);
                  New_Line;
               end;
            end if;
         end;
      end loop;
   end Dump;

end Libadalang.Target_Info;
