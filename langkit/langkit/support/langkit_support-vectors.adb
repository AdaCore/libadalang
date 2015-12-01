with System; use type System.Address;
use System;
with System.Memory; use System.Memory;

-----------------------------
-- Langkit_Support.Vectors --
-----------------------------

package body Langkit_Support.Vectors is

   El_Size : constant size_t := Elements_Array'Component_Size / Storage_Unit;

   -------------
   -- Reserve --
   -------------

   procedure Reserve (Self : in out Vector; Capacity : Positive) is
      Siz : constant size_t :=
        size_t (Capacity) * El_Size;
   begin
      if Small_Vector_Capacity > 0 then
         if Self.Capacity = Small_Vector_Capacity then
            Self.E := To_Pointer (Alloc (Siz));
            for I in 0 .. Self.Size - 1 loop
               Self.E.all (I) := Self.SV (I);
            end loop;
         else
            Self.E := To_Pointer (Realloc (Self.E.all'Address, Siz));
         end if;
      else
         if Self.E /= null then
            Self.E := To_Pointer (Realloc (Self.E.all'Address, Siz));
         else
            Self.E := To_Pointer (Alloc (Siz));
         end if;
      end if;
      Self.Capacity := Capacity;
   end Reserve;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Vector; Element : Element_Type) is
   begin
      if Self.Capacity = Self.Size then
         Reserve (Self, (Self.Capacity * 2) + 1);
      end if;

      if Small_Vector_Capacity = 0 then
         Self.E.all (Self.Size) := Element;
      else
         if Self.Capacity = Small_Vector_Capacity then
            Self.SV (Self.Size) := Element;
         else
            Self.E.all (Self.Size) := Element;
         end if;
      end if;

      Self.Size := Self.Size + 1;
   end Append;

   ---------
   -- Get --
   ---------

   function Get (Self : Vector; Index : Natural) return Element_Type is
   begin
      if Small_Vector_Capacity = 0 then
         return Self.E (Index);
      else
         if Self.Capacity = Small_Vector_Capacity then
            return Self.SV (Index);
         else
            return Self.E (Index);
         end if;
      end if;
   end Get;

   ----------------
   -- Get_Access --
   ----------------

   function Get_Access
     (Self : Vector; Index : Natural) return Element_Access
   is
   begin
      if Small_Vector_Capacity = 0 then
         return Self.E (Index)'Unrestricted_Access;
      else
         if Self.Capacity = Small_Vector_Capacity then
            return Self.SV (Index)'Unrestricted_Access;
         else
            return Self.E (Index)'Unrestricted_Access;
         end if;
      end if;
   end Get_Access;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Vector) is
   begin
      Free (Self.E);
   end Destroy;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Vector) is
   begin
      Self.Size := 0;
   end Clear;

   ---------
   -- Pop --
   ---------

   function Pop (Self : in out Vector) return Element_Type is
   begin
      Self.Size := Self.Size - 1;
      return Get (Self, Self.Size);
   end Pop;

   ---------
   -- Pop --
   ---------

   procedure Pop (Self : in out Vector) is
      Discard : Element_Type := Pop (Self);
   begin
      null;
   end Pop;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Self : Vector) return Element_Access
   is
   begin
      return Get_Access (Self, Self.Size - 1);
   end Last_Element;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Self : Vector) return Element_Type
   is
   begin
      return Get (Self, Self.Size - 1);
   end Last_Element;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Self : Vector) return Element_Type
   is (Get (Self, 0));

   ------------
   -- Length --
   ------------

   function Length (Self : Vector) return Natural is (Self.Size);

   -----------
   -- Slice --
   -----------

   function Slice
     (Self : Vector; First, Last : Natural) return Elements_Array
   is
   begin
      if Small_Vector_Capacity = 0 then
         return Self.E (First .. Last);
      else
         if Self.Capacity = Small_Vector_Capacity then
            return Self.SV (First .. Last);
         else
            return Self.E (First .. Last);
         end if;
      end if;
   end Slice;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Self : Vector) return Elements_Array
   is
   begin
      if Self.Size = 0 then
         return Elements_Arrays.Empty_Array;
      else
         return Slice (Self, 0, Self.Size - 1);
      end if;
   end To_Array;

   -----------
   -- Image --
   -----------

   function Image (Self : Vector) return String is
      function Image (Self : Vector; I : Natural) return String
      is
        (if I < Length (Self) - 1 then Image (Get (Self, I))
         & ", " & Image (Self, I + 1)
         else Image (Get (Self, I)));
   begin
      return "[" & (if Self.Size > 0
                    then Image (Self, 0)
                    else "") & "]";
   end Image;

end Langkit_Support.Vectors;
