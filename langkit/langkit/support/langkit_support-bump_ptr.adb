with Ada.Unchecked_Conversion;
with System.Memory; use System.Memory;
with System; use System;
with Ada.Unchecked_Deallocation;

package body Langkit_Support.Bump_Ptr is

   use Pages_Vector;

   procedure Dealloc is new Ada.Unchecked_Deallocation
     (Bump_Ptr_Pool_Type, Bump_Ptr_Pool);

   function Align (Size, Alignment : Storage_Offset) return Storage_Offset
     with Inline_Always;

   -----------
   -- Align --
   -----------

   function Align (Size, Alignment : Storage_Offset) return Storage_Offset is
      M : constant Storage_Offset := Size mod Alignment;
   begin
      if M = 0 then
         return Size;
      else
         return Size + (Alignment - M);
      end if;
   end Align;

   ------------
   -- Create --
   ------------

   function Create return Bump_Ptr_Pool is
   begin
      return new Bump_Ptr_Pool_Type;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (Pool : in out Bump_Ptr_Pool) is
   begin
      if Pool = No_Pool then
         return;
      end if;

      --  Free every page allocated

      --  TODO: Might be interresting at some point to keep a global cache of
      --  pages ourself, since we always use the same size.

      for PI in First_Index (Pool.Pages) .. Last_Index (Pool.Pages) loop
         Free (Get (Pool.Pages, PI));
      end loop;
      Destroy (Pool.Pages);
      Dealloc (Pool);
   end Free;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Pool : Bump_Ptr_Pool; S : Storage_Offset) return System.Address
   is
      Obj_Offset : Storage_Offset;
   begin

      --  If the required size is bigger than the page size, we'll allocate a
      --  special page the size of the required object. Basically we fall-back
      --  on regular alloc mechanism, but this ensures that we can handle all
      --  allocations transparently via this allocator

      if S > Page_Size then
         declare
            Mem : constant System.Address := System.Memory.Alloc (size_t (S));
         begin

            --  Append the allocated memory to the pool pages, so that it is
            --  freed on pool free, but don't touch at the current_page, so
            --  it can keep being used next time.

            Append (Pool.Pages, Mem);
            return Mem;
         end;
      end if;

      --  When we don't have enough space to allocate the chunk, allocate a new
      --  page

      if Page_Size - Pool.Current_Offset < S then
         Pool.Current_Page := System.Memory.Alloc (Page_Size);
         Append (Pool.Pages, Pool.Current_Page);
         Pool.Current_Offset := 0;
      end if;

      --  Allocation itself is as simple as bumping the offset pointer, and
      --  returning the old value.

      Obj_Offset := Pool.Current_Offset;
      Pool.Current_Offset := Pool.Current_Offset + S;
      return Pool.Current_Page + Obj_Offset;
   end Allocate;

   -----------
   -- Alloc --
   -----------

   Pointer_Size : constant Storage_Offset
     := System.Address'Size / Storage_Unit;

   package body Alloc is

      function To_Pointer is
        new Ada.Unchecked_Conversion (System.Address, Element_Access);

      function Alloc
        (Pool : Bump_Ptr_Pool) return Element_Access is
      begin

         --  This function just queries the proper size of the Element_T type,
         --  and converts the return value to the proper access type

         return To_Pointer
           (Allocate
              (Pool,
               Align (Element_T'Max_Size_In_Storage_Elements, Pointer_Size)));
      end Alloc;
   end Alloc;

   type Address_Access is access all System.Address;

   ------------------
   -- Tagged_Alloc --
   ------------------

   package body Tagged_Alloc is

      T : aliased Element_T;

      package Gen_Alloc is new
        Langkit_Support.Bump_Ptr.Alloc (Element_T, Element_Access);

      function Dirty_Conv
      is new Ada.Unchecked_Conversion (Element_Access, Address_Access);

      function Alloc
        (Pool : Bump_Ptr_Pool) return Element_Access
      is
         --  This bit of code is actually quite funny. It is born out of the
         --  conflation of several unfortunate design choices in Ada:

         --  1. The first one is that with Ada memory pools, you have to
         --  declare the memory pool of your access type at the point of
         --  definition of the access type. While this is fine and dandy for
         --  global pools, in our case, when we want a scoped pool that links a
         --  bunch of object's lifetime to another, this is highly unpractical.
         --  It would make us declare new types in the scopes where we create
         --  our object hierarchy, and do horrible casts everywhere.

         --  2. As a savvy Ada programmer, we then envision encapsulating this
         --  logic in a generic. But this is impossible, because you cannot
         --  declare a representation clause on a type that is declared in
         --  the scope of a generic.
         --   (╯°□°）╯︵ ┻━┻

         --  3. Ok, so you cannot encapsulate in a generic. Let's do a memory
         --  pool manually, like we'd do in C/C++. Problem is, you don't call
         --  the new operator to instantiate tagged objects, so your tagged
         --  objects are gonna have no tag ! (ﾉಥ益ಥ）ﾉ﻿ ┻━┻

         --  3. No problem, let's hack around it by creating a temp variable of
         --  the type, and assigning to the newly allocated instance, so that
         --  the tag is gonna be copied ! Except assignment doesn't copy tags
         --  in Ada ┻━┻ ︵ヽ(`Д´)ﾉ︵﻿ ┻━┻

         --  Hence we are reduced to this dirty hack, where we'll create a temp
         --  object, get the tag, and copy it manually in the newly created
         --  object. This is dirty and completely implementation dependent.
         --  So here be dragons

         --  Post-Scriptum: As it turns out, Ada 2012 memory subpools are
         --  designed for exactly that purpose. This code survives because
         --  GNAT's implementation of subpools is on average 10 times slower
         --  than the ad-hoc allocation.

         Ret      : constant Element_Access := Gen_Alloc.Alloc (Pool);
         T_Access : constant Element_Access := T'Unchecked_Access;

         Tag_From : constant Address_Access := Dirty_Conv (T_Access);
         Tag_To   : constant Address_Access := Dirty_Conv (Ret);
      begin
         Ret.all := T;
         Tag_To.all := Tag_From.all;
         return Ret;
      end Alloc;
   end Tagged_Alloc;

   ---------------------------
   -- Allocate_From_Subpool --
   ---------------------------

   overriding procedure Allocate_From_Subpool
     (Pool                     : in out Ada_Bump_Ptr_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count;
      Subpool                  : not null Subpool_Handle)
   is
      pragma Unreferenced (Pool);
   begin
      Storage_Address := Allocate
        (Bump_Ptr_Pool (Subpool), Align (Size_In_Storage_Elements, Alignment));
   end Allocate_From_Subpool;

   --------------------
   -- Create_Subpool --
   --------------------

   overriding function Create_Subpool
     (Pool : in out Ada_Bump_Ptr_Pool)
      return not null Subpool_Handle
   is
      Subpool : constant Subpool_Handle := Subpool_Handle (Create);
   begin
      Set_Pool_Of_Subpool (Subpool, Pool);
      return Subpool;
   end Create_Subpool;

   ------------------------
   -- Deallocate_Subpool --
   ------------------------

   overriding procedure Deallocate_Subpool
     (Pool    : in out Ada_Bump_Ptr_Pool;
      Subpool : in out Subpool_Handle)
   is
      pragma Unreferenced (Pool);
   begin
      Free (Bump_Ptr_Pool (Subpool));
   end Deallocate_Subpool;

end Langkit_Support.Bump_Ptr;
