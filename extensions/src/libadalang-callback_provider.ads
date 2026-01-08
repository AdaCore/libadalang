--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--
--  This package provides a unit provider that calls back into Python
--  to resolve unit names to filenames. This allows Python code to
--  implement custom unit resolution logic without modifying libadalang.
--
--  Limitation: This provider assumes one compilation unit per file.
--  Files with multiple compilation units are not supported.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with System;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

package Libadalang.Callback_Provider is

   use Support.Text;

   type Callback_Unit_Provider is
      new Libadalang.Analysis.Unit_Provider_Interface with private;
   --  Unit provider that calls back to Python for unit filename resolution

   overriding function Get_Unit_Filename
     (Provider : Callback_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;

   overriding procedure Get_Unit_Location
     (Provider       : Callback_Unit_Provider;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : in out Unbounded_String;
      PLE_Root_Index : in out Natural);

   overriding function Get_Unit
     (Provider    : Callback_Unit_Provider;
      Context     : Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit'Class;

   overriding procedure Get_Unit_And_PLE_Root
     (Provider       : Callback_Unit_Provider;
      Context        : Analysis_Context'Class;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Charset        : String := "";
      Reparse        : Boolean := False;
      Unit           : in out Analysis_Unit'Class;
      PLE_Root_Index : in out Natural);

   overriding procedure Release (Provider : in out Callback_Unit_Provider);

   --  Callback function type for language bindings to implement
   --  Parameters:
   --    Data: Opaque pointer to user data (passed through from Create)
   --    Name: Unit name as UTF-8 null-terminated string
   --    Kind: 0 for spec, 1 for body
   --  Returns: Filename as UTF-8 null-terminated string, or null for "not found"
   --
   --  Memory Ownership:
   --    The returned string MUST be allocated with malloc(). Ada will call
   --    free() on the returned pointer after copying the string value.
   --    Returning NULL (System.Null_Address) indicates unit not found.
   type Get_Unit_Filename_Callback is access function
     (Data      : System.Address;
      Name      : System.Address;
      Kind      : int) return System.Address
     with Convention => C;

   function Create_Callback_Provider
     (Callback : Get_Unit_Filename_Callback;
      Data     : System.Address;
      Charset  : String := Default_Charset) return Callback_Unit_Provider;
   --  Create a unit provider that calls back to Python.
   --
   --  Callback: Function pointer to Python callback
   --  Data: Opaque pointer to Python object (passed to callback)
   --  Charset: Character set for source files

   function Create_Callback_Provider_Reference
     (Callback : Get_Unit_Filename_Callback;
      Data     : System.Address;
      Charset  : String := Default_Charset) return Unit_Provider_Reference;
   --  Wrapper around Create_Callback_Provider to create a unit provider reference

private

   type Callback_Unit_Provider is
      new Libadalang.Analysis.Unit_Provider_Interface
   with record
      Callback : Get_Unit_Filename_Callback;
      Data     : System.Address;
      Charset  : Unbounded_String;
   end record;

   function Create_Callback_Provider_Reference
     (Callback : Get_Unit_Filename_Callback;
      Data     : System.Address;
      Charset  : String := Default_Charset) return Unit_Provider_Reference
   is (Create_Unit_Provider_Reference
         (Create_Callback_Provider (Callback, Data, Charset)));

end Libadalang.Callback_Provider;
