with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with Langkit_Support.Array_Utils;

--  This package implements a very simple Vector type. It has the following
--  attributes:
--
--  - Very lightweight implementation, very few primitives.
--  - Not controlled (manual memory management).
--  - Ada 2012-like iteration via the Iterate aspect, so read-only access to
--    elements in for .. of loops.
--  - Uses realloc for resize, so faster, but won't be correct on every type.
--  - Not tagged, so no dot notation on primitives.
--  - Small vector optimization: can store a number of the elements inline.

generic
   type Element_Type is private;
   Small_Vector_Capacity : Natural := 0;
package Langkit_Support.Vectors is

   package Elements_Arrays is new Array_Utils (Element_Type);
   subtype Elements_Array is Elements_Arrays.Array_Type;

   type Vector is private
     with Iterable =>
       (First       => First_Index,
        Next        => Next,
        Has_Element => Has_Element,
        Element     => Get);

   Empty_Vector : constant Vector;

   type Element_Access is not null access all Element_Type;

   procedure Append (Self : in out Vector; Element : Element_Type)
     with Inline_Always;
   --  Appends Element to Self

   function Get (Self : Vector; Index : Natural) return Element_Type
     with Inline_Always;
   --  Get the element at Index

   function Get_Access (Self : Vector; Index : Natural) return Element_Access
     with Inline_Always;
   --  Get an access to the element at Index
   --  NOTICE: This access is unsafe, and might get invalidated if the Vector
   --  is reallocated. Hence, its lifetime is considered to be as long as the
   --  vector is not modified.

   procedure Destroy (Self : in out Vector)
     with Inline_Always;
   --  Destroy this vector

   procedure Clear (Self : in out Vector)
     with Inline_Always;
   --  Remove every element in this vector.
   --  NOTICE: this function does not actually free the memory of the vector!

   function First_Element (Self : Vector) return Element_Type;
   --  Return the first element in this vector

   function Last_Element (Self : Vector) return Element_Type;
   --  Return the last element in this vector

   function Last_Element (Self : Vector) return Element_Access;
   --  Return an access to the last element in this vector
   --  NOTICE: Read Get_Access's documentation.

   function Length (Self : Vector) return Natural
     with Inline_Always;
   --  Return the Length of the vector, ie. the number of elements it contains

   function First_Index (Self : Vector) return Natural is (0)
     with Inline_Always;
   --  Return the first index, only used for the Iterable aspect

   function Last_Index (Self : Vector) return Integer
   is (Length (Self) - 1)
     with Inline_Always;
   --  Return the index of the last element in this vector

   function Next (Self : Vector; N : Natural) return Natural is (N + 1)
     with Inline_Always;
   --  Given a vector and an index, return the next index. Only used for the
   --  iterable aspect.

   function Pop (Self : in out Vector) return Element_Type
     with Pre => Length (Self) > 0;
   --  Pop the last element from vector and return it

   procedure Pop (Self : in out Vector)
     with Pre => Length (Self) > 0;
   --  Pop the last element from vector

   function Has_Element (Self : Vector; N : Natural) return Boolean
     with Inline_Always;
   --  Given a vector and an index, return True if the index is in the vector
   --  range. Only used for the iterable aspect.

   function To_Array
     (Self : Vector) return Elements_Array;
   --  Return a (shallow) copy of the current vector as an array

   function Slice
     (Self : Vector; First, Last : Natural) return Elements_Array;
   --  Return a (shallow) copy of Self (First .. Last) as an array

   generic
      with function Image (El : Element_Type) return String;
   function Image (Self : Vector) return String;
   --  Generic function, that, given a Image function on the element type,
   --  returning the representation of an instance of Element_Type as a string,
   --  will return a string representation of the vector.

private

   subtype Internal_Elements_Array is Elements_Arrays.Array_Type (Natural);
   type Elements_Array_Access is access all Internal_Elements_Array;

   function To_Pointer is
     new Ada.Unchecked_Conversion (System.Address, Elements_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Elements_Array, Elements_Array_Access);

   subtype Small_Array_Type
     is Elements_Arrays.Array_Type (0 .. Small_Vector_Capacity - 1);

   type Vector is record
      E        : Elements_Array_Access := null;
      Size     : Natural := 0;
      Capacity : Natural := Small_Vector_Capacity;
      SV       : Small_Array_Type;
   end record;

   Empty_Vector : constant Vector := (E => null, Size => 0, others => <>);

   procedure Reserve (Self : in out Vector; Capacity : Positive)
     with Inline_Always;
   --  Reserve Capacity elements

   function Has_Element (Self : Vector; N : Natural) return Boolean is
     (N < Self.Size);

end Langkit_Support.Vectors;
