--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Libadalang.Callback_Provider is

   function Address_To_Chars_Ptr is new Ada.Unchecked_Conversion
     (System.Address, chars_ptr);

   function Chars_Ptr_To_Address is new Ada.Unchecked_Conversion
     (chars_ptr, System.Address);

   --  Import C's free() to properly free malloc'd strings
   procedure C_Free (Ptr : System.Address)
     with Import, Convention => C, External_Name => "free";

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Callback_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String
   is
      --  Convert unit name to UTF-8 string
      Name_UTF8 : constant String := To_UTF8 (Name);
      Name_C    : chars_ptr := New_String (Name_UTF8);

      --  Convert kind to integer (0 = spec, 1 = body)
      Kind_Int  : constant int :=
        (if Kind = Unit_Specification then 0 else 1);

      --  Call Python callback
      Result_Addr : System.Address;
      Result_Str  : Unbounded_String;
      Result_C    : chars_ptr;

      use type System.Address;
   begin
      --  Call callback with the C string pointer converted to address
      Result_Addr := Provider.Callback
        (Provider.Data, Chars_Ptr_To_Address (Name_C), Kind_Int);
      Free (Name_C);

      --  If callback returned null, unit not found
      if Result_Addr = System.Null_Address then
         return "";
      end if;

      --  Convert C string address to Ada string
      --  Use unchecked conversion to convert Address to chars_ptr
      Result_C := Address_To_Chars_Ptr (Result_Addr);
      Result_Str := To_Unbounded_String (Value (Result_C));

      --  Memory Management:
      --  Free the returned string. The callback must allocate with malloc().
      --  The Python bindings use ctypes.malloc to ensure correct memory sharing
      --  between Python and Ada/C.
      C_Free (Result_Addr);

      return To_String (Result_Str);
   end Get_Unit_Filename;

   -----------------------
   -- Get_Unit_Location --
   -----------------------

   overriding procedure Get_Unit_Location
     (Provider       : Callback_Unit_Provider;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : in out Unbounded_String;
      PLE_Root_Index : in out Natural)
   is
      Fn : constant String := Provider.Get_Unit_Filename (Name, Kind);
   begin
      if Fn = "" then
         Filename := Null_Unbounded_String;
         PLE_Root_Index := 1;
      else
         Filename := To_Unbounded_String (Fn);
         --  Limitation: PLE_Root_Index is hardcoded to 1, which assumes
         --  exactly one compilation unit per file starting at the root.
         --  Files with multiple compilation units are not supported.
         PLE_Root_Index := 1;
      end if;
   end Get_Unit_Location;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Callback_Unit_Provider;
      Context     : Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit'Class
   is
      Fn : constant String := Provider.Get_Unit_Filename (Name, Kind);
      Actual_Charset : constant String :=
        (if Charset'Length = 0
         then To_String (Provider.Charset)
         else Charset);
   begin
      if Fn = "" then
         --  Return an empty unit if not found
         declare
            Empty_Unit : Analysis_Unit'Class :=
              Get_From_Buffer
                (Context => Context,
                 Filename => To_UTF8 (Name),
                 Buffer => "",
                 Charset => Actual_Charset);
         begin
            return Empty_Unit;
         end;
      else
         return Context.Get_From_File (Fn, Actual_Charset, Reparse);
      end if;
   end Get_Unit;

   ---------------------------
   -- Get_Unit_And_PLE_Root --
   ---------------------------

   overriding procedure Get_Unit_And_PLE_Root
     (Provider       : Callback_Unit_Provider;
      Context        : Analysis_Context'Class;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Charset        : String := "";
      Reparse        : Boolean := False;
      Unit           : in out Analysis_Unit'Class;
      PLE_Root_Index : in out Natural)
   is
   begin
      Unit := Provider.Get_Unit (Context, Name, Kind, Charset, Reparse);
      PLE_Root_Index := 1;
   end Get_Unit_And_PLE_Root;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Provider : in out Callback_Unit_Provider) is
   begin
      --  Nothing to release - Python manages the callback and data
      null;
   end Release;

   ------------------------------
   -- Create_Callback_Provider --
   ------------------------------

   function Create_Callback_Provider
     (Callback : Get_Unit_Filename_Callback;
      Data     : System.Address;
      Charset  : String := Default_Charset) return Callback_Unit_Provider
   is
   begin
      return Provider : Callback_Unit_Provider do
         Provider.Callback := Callback;
         Provider.Data := Data;
         Provider.Charset := To_Unbounded_String (Charset);
      end return;
   end Create_Callback_Provider;

end Libadalang.Callback_Provider;
