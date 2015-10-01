with Ada.Unchecked_Conversion;

package body Langkit_Support.Bump_Ptr.Vectors is

   function Alloc_Chunk (P : Bump_Ptr_Pool; S : Natural) return Chunk_Access;

   -----------------
   -- Alloc_Chunk --
   -----------------

   function Alloc_Chunk (P : Bump_Ptr_Pool; S : Natural) return Chunk_Access
   is
      subtype C is Chunk (S);

      function To_Pointer is
        new Ada.Unchecked_Conversion (System.Address, Chunk_Access);

      Ret_Memory : constant System.Address :=
        Allocate (P, C'Max_Size_In_Storage_Elements);
      --  Allocate a chunk of memory for the result
      --  discriminated record...

      Ret_Disc   : Natural;
      for Ret_Disc'Address use Ret_Memory;
      --  And initialize its discriminant properly as the
      --  runtime would do with regular allocation.
   begin
      Ret_Disc := S;
      return To_Pointer (Ret_Memory);
   end Alloc_Chunk;

   function Create (P : Bump_Ptr_Pool) return Vector
   is
     (Vector'(Pool => P, others => <>));

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Vector; Element : Element_Type)
   is

      procedure Init_Chunk (C : in out Chunk) with Inline_Always;

      ----------------
      -- Init_Chunk --
      ----------------

      procedure Init_Chunk (C : in out Chunk) is
      begin
         C.Next_Chunk := null;
         C.Length := 0;
      end Init_Chunk;

      Old_Chunk : Chunk_Access;
   begin
      --  First append, create a chunk and initialize it
      if Self.Length = 0 then
         Self.First_Chunk := Alloc_Chunk (Self.Pool, 2);
         Init_Chunk (Self.First_Chunk.all);
         Self.Current_Chunk := Self.First_Chunk;
      end if;

      --  We filled the current chunk completely, create a new chunk and
      --  initialize it, chain it with the previous chunk
      if Self.Current_Chunk.Length = Self.Current_Chunk.Capacity then
         Old_Chunk := Self.Current_Chunk;
         Self.Current_Chunk := Alloc_Chunk (Self.Pool, Old_Chunk.Capacity * 2);
         Init_Chunk (Self.Current_Chunk.all);
         Old_Chunk.Next_Chunk := Self.Current_Chunk;
      end if;

      --  At this stage we know the current chunk can contain element, insert
      --  it
      Self.Current_Chunk.Elements (Self.Current_Chunk.Length) := Element;
      Self.Current_Chunk.Length := Self.Current_Chunk.Length + 1;
      Self.Length := Self.Length + 1;
   end Append;

   ---------
   -- Get --
   ---------

   function Get (Self : Vector; C : Cursor) return Element_Type is
      pragma Unreferenced (Self);
   begin
      return C.Chunk.Elements (C.Index_In_Chunk);
   end Get;

   ------------------
   -- Get_At_Index --
   ------------------

   function Get_At_Index (Self : Vector; I : Natural) return Element_Type
   is
      Chunk_Start_Index : Natural := 0;
      Current_Chunk     : Chunk_Access := Self.First_Chunk;
   begin
      if I >= Self.Length - Self.Current_Chunk.Length then
         return Self.Current_Chunk.Elements
           (I - (Self.Length - Self.Current_Chunk.Length));
      end if;

      while Current_Chunk /= null
        and then I >= Chunk_Start_Index + Current_Chunk.Capacity
      loop

         Chunk_Start_Index := Chunk_Start_Index + Current_Chunk.Capacity;
         Current_Chunk := Current_Chunk.Next_Chunk;
      end loop;

      return Current_Chunk.Elements (I - Chunk_Start_Index);
   end Get_At_Index;

   ----------------
   -- Get_Access --
   ----------------

   function Get_Access (Self : Vector; C : Cursor) return Element_Access is
      pragma Unreferenced (Self);
   begin
      return C.Chunk.Elements (C.Index_In_Chunk)'Unrestricted_Access;
   end Get_Access;

   ------------
   -- Length --
   ------------

   function Length (Self : Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   -----------
   -- First --
   -----------

   function First (Self : Vector) return Cursor is
   begin
      return Cursor'(Chunk => Self.First_Chunk, Index_In_Chunk => 0);
   end First;

   ----------
   -- Next --
   ----------

   function Next (Self : Vector; C : Cursor) return Cursor is
      pragma Unreferenced (Self);
   begin
      if C.Index_In_Chunk = C.Chunk.Capacity - 1 then
         return Cursor'(C.Chunk.Next_Chunk, 0);
      else
         return Cursor'(C.Chunk, C.Index_In_Chunk + 1);
      end if;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : Vector; C : Cursor) return Boolean is
      pragma Unreferenced (Self);
   begin
      return C.Chunk /= null and then C.Index_In_Chunk < C.Chunk.Length;
   end Has_Element;

end Langkit_Support.Bump_Ptr.Vectors;
