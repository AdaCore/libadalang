--  This package provides the user of bump pointer pools with a Vector like
--  container. Traditional vectors reallocate their whole storage when we
--  outgrow their current capacity, but this is not possible without a very
--  large memory cost with bump pointer pools, since you cannot free individual
--  chunks of memory.
--
--  So in this package, we have a vector that is constituted of exponentially
--  growing chunks of memory. Since the maximum number of chunks is a constant
--  fixed by the memory of the system (32 being the limit on a modern machine
--  with 16gb of ram), random element access is still amortized O(1), with a
--  large constant.

--  Beware though, random access is still on the average of 100x slower than in
--  order iteration, so *never* use Get_At_Index to iterate over the vector!

generic
   type Element_Type is private;
package Langkit_Support.Bump_Ptr.Vectors is

   type Vector is private
     with Iterable =>
       (First       => First,
        Next        => Next,
        Has_Element => Has_Element,
        Element     => Get);

   type Cursor is private;

   Empty_Cursor : constant Cursor;

   type Element_Access is not null access all Element_Type;

   function Create (P : Bump_Ptr_Pool) return Vector;
   --  Returns a newly created vector using P as it's pool storage.

   procedure Append (Self : in out Vector; Element : Element_Type)
     with Inline_Always;
   --  Appends Element to Self

   function Get (Self : Vector; C : Cursor) return Element_Type
     with Inline_Always;
   --  Get the element at Index

   function Get_At_Index (Self : Vector; I : Natural) return Element_Type
     with
       Inline,
       Pre => I < Length (Self);
   --  Get the element at Index

   function Get_Access (Self : Vector; C : Cursor) return Element_Access
     with Inline_Always;
   --  Get an access to the element at Index. The lifetime of the access is the
   --  one of the vector

   function Length (Self : Vector) return Natural
     with Inline_Always;
   --  Return the Length of the vector, ie. the number of elements it contains

   function First (Self : Vector) return Cursor
     with Inline_Always;
   --  Return the first index, only used for the Iterable aspect

   function Next (Self : Vector; C : Cursor) return Cursor
     with Inline_Always;
   --  Given a vector and an index, return the next index. Only used for the
   --  iterable aspect

   function Has_Element (Self : Vector; C : Cursor) return Boolean
     with Inline_Always;
   --  Given a vector and an index, return True if the index is in the vector
   --  range. Only used for the iterable aspect.

private

   type Elements_Array is array (Natural range <>) of Element_Type;

   type Chunk;
   type Chunk_Access is access all Chunk;
   pragma No_Strict_Aliasing (Chunk_Access);

   type Chunk (Capacity : Natural) is record
      Elements   : Elements_Array (0 .. Capacity);
      Next_Chunk : Chunk_Access;
      Length     : Natural := 0;
   end record;

   type Vector is record
      Pool                       : Bump_Ptr_Pool;
      First_Chunk, Current_Chunk : Chunk_Access := null;
      Length                     : Natural := 0;
   end record;

   type Cursor is record
      Chunk          : Chunk_Access;
      Index_In_Chunk : Natural;
   end record;

   Empty_Cursor : constant Cursor := (null, 0);

end Langkit_Support.Bump_Ptr.Vectors;
