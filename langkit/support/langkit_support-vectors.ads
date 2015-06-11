with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System;

--  This package implements a very simple Vector type. It has the following
--  attributes:
--
--  - Very lightweight implementation, very few primitives.
--  - Not controlled (manual memory management).
--  - Ada 2012-like iteration via the Iterate aspect, so read-only access to
--    elements in for .. of loops
--  - Uses realloc for resize, so faster, but won't be correct on every type.
--  - Not tagged, so no dot notation on primitives
--  - Small vector optimization: can store a number of the elements inline
--

generic
   type Element_Type is private;
   Small_Vector_Capacity : Natural := 0;
package Langkit_Support.Vectors is

   type Elements_Array is array (Natural) of Element_Type;
   type Elements_Array_Access is access all Elements_Array;

   function To_Pointer is
     new Ada.Unchecked_Conversion (System.Address, Elements_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Elements_Array, Elements_Array_Access);

   type Small_Array_Type
   is array (0 .. Small_Vector_Capacity - 1) of Element_Type;

   type Vector is private
     with Iterable =>
       (First       => First_Index,
        Next        => Next,
        Has_Element => Has_Element,
        Element     => Get);

   procedure Append (Self : in out Vector; Element : Element_Type);
   pragma Inline_Always (Append);
   --  Appends Element to Self

   function Get (Self : Vector; Index : Natural) return Element_Type;
   pragma Inline_Always (Get);
   --  Get the element at Index

   procedure Destroy (Self : in out Vector);
   pragma Inline_Always (Destroy);
   --  Destroy this vector

   procedure Clear (Self : in out Vector);
   pragma Inline_Always (Clear);
   --  Remove every element in this vector.
   --  NOTICE: this function does not actually free the memory of the vector!

   function First_Element (Self : Vector) return Element_Type;
   --  Return the first element in this vector

   function Last_Element (Self : Vector) return Element_Type;
   --  Return the last element in this vector

   function Length (Self : Vector) return Natural;
   pragma Inline_Always (Length);
   --  Return the Length of the vector, ie. the number of elements it contains

   function First_Index (Self : Vector) return Natural is (0);
   pragma Inline_Always (First_Index);
   --  Return the first index, only used for the Iterable aspect

   function Last_Index (Self : Vector) return Integer
   is (Length (Self) - 1);
   pragma Inline_Always (Last_Index);
   --  Return the index of the last element in this vector

   function Next (Self : Vector; N : Natural) return Natural is (N + 1);
   pragma Inline_Always (Next);
   --  Given a vector and an index, return the next index. Only used for the
   --  iterable aspect

   function Has_Element (Self : Vector; N : Natural) return Boolean;
   pragma Inline_Always (Has_Element);
   --  Given a vector and an index, return True if the index is in the vector
   --  range. Only used for the iterable aspect.

private

   type Vector is record
      E        : Elements_Array_Access := null;
      Size     : Natural := 0;
      Capacity : Natural := Small_Vector_Capacity;
      SV       : Small_Array_Type;
   end record;

   procedure Reserve (Self : in out Vector; Capacity : Positive);
   pragma Inline_Always (Reserve);
   --  Reserve Capacity elements

   function Has_Element (Self : Vector; N : Natural) return Boolean is
     (N < Self.Size);

end Langkit_Support.Vectors;
