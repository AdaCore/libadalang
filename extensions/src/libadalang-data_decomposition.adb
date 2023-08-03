--
--  Copyright (C) 2022-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.Mmap;
with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;
with GNATCOLL.VFS;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;

with Libadalang.Auto_Provider; use Libadalang.Auto_Provider;
with Libadalang.Common;        use Libadalang.Common;
with Libadalang.GPR_Utils;     use Libadalang.GPR_Utils;

package body Libadalang.Data_Decomposition is

   Zero      : constant GMP_Int.Big_Integer := GMP_Int.Make ("0");
   One       : constant GMP_Int.Big_Integer := GMP_Int.Make ("1");
   Size_Last : constant GMP_Int.Big_Integer :=
     GMP_Int.Make (Size_Type'Last'Image);

   function Type_Kind_For (Decl : Base_Type_Decl'Class) return Type_Kind;
   --  Return the type kind for the given type declaration parse tree. Raise a
   --  ``Type_Mimatch_Error`` exception if it is not possible to determine this
   --  type kind.

   function Wrap
     (Self       : Numerical_Expression_Access;
      Collection : Repinfo_Collection)
      return Numerical_Expression;
   function Wrap
     (Self        : Type_Representation_Access;
      Declaration : Base_Type_Decl'Class;
      Collection  : Repinfo_Collection)
      return Type_Representation;
   function Wrap
     (Self        : Variant_Representation_Access;
      Declaration : Variant;
      Collection  : Repinfo_Collection)
      return Variant_Representation;
   function Wrap
     (Self        : Component_Representation_Access;
      Declaration : Defining_Name;
      Collection  : Repinfo_Collection)
      return Component_Representation;
   --  Converter between internal objects and public ones

   package Defining_Name_Vectors is new Ada.Containers.Vectors
     (Positive, Defining_Name);

   procedure Iterate_Derivations
     (Collection : Repinfo_Collection;
      Decl       : Base_Type_Decl'Class;
      Repr       : Type_Representation_Access;
      Process    : access procedure (Decl : Concrete_Type_Decl;
                                     Def  : Type_Def;
                                     Repr : Type_Representation_Access));
   --  Assuming that ``Self`` is a record type declaration and ``Repr`` the
   --  corresponding representation data, call ``Process`` on all types whose
   --  derivations yield to ``Self`` plus their corresponding representation
   --  data.

   function Get_Component_List (Def : Type_Def) return Component_List;
   --  Assuming that ``Self`` is a record type declaration, return its root
   --  component list (i.e. ignoring component lists for parents and variant
   --  parts).

   type Component_Association is record
      Decl : Defining_Name;
      Repr : Component_Representation_Access;
   end record;

   package Component_Association_Vectors is new Ada.Containers.Vectors
     (Positive, Component_Association);

   function Component_Associations
     (Collection : Repinfo_Collection;
      Decl       : Base_Type_Decl'Class;
      Repr       : Type_Representation_Access)
      return Component_Association_Vectors.Vector;
   --  Append to ``Decls`` all components from ``Self``, including
   --  discriminants, components defined in parent types, but excluding
   --  components from variant parts.

   ----------------------
   -- Extract_Variants --
   ----------------------

   function Extract_Variants
     (Collection : Repinfo_Collection;
      Decl       : Ada_Node'Class;
      Variants   : Variant_Representation_Access_Array;
      Part_Node  : Variant_Part) return Variant_Representation_Array;
   --  Return the array of ``Variant_Representation`` corresponding to
   --  ``Variants`` and ``Part_Node``. Raise a ``Type_Mismatch_Error`` if an
   --  inconstency is detected between the two: ``Part_Node`` being null or not
   --  having the right number of variants.
   --
   --  ``Collection`` is used to wrap variant representations, ``Decl`` is used
   --  for error message formatting

   function Load_JSON (Filename : String) return JSON_Value;
   --  Open ``Filename`` in read mode, parse its content and return the
   --  corresponding JSON document. Raise a ``Loading_Error`` exception in case
   --  of error.

   function Symbolize
     (Self : in out Repinfo_Collection_Data; Name : String) return Symbol_Type;
   function Symbolize
     (Self : in out Repinfo_Collection_Data;
      Name : Text_Type) return Symbol_Type;
   --  Turn the given name into a symbol for ``Self``

   function Load_Type
     (Self   : in out Repinfo_Collection_Data;
      Entity : JSON_Value;
      Name   : String) return Type_Representation_Access;
   --  Import the type representation corresponding to the description in
   --  ``Entity`` into ``Self``. Return null if this is not an entity we can
   --  handle.

   function Load_Components
     (Self       : in out Repinfo_Collection_Data;
      Components : JSON_Value;
      Context    : String) return Component_Representation_Access_Array_Access;
   --  Import the component representations from ``Components`` into ``Self``.
   --
   --  ``Context`` must be a human-readable description of the contanining
   --  entity being loaded, for error reporting purposes.

   function Load_Variants
     (Self     : in out Repinfo_Collection_Data;
      Variants : JSON_Value;
      Context  : String) return Variant_Representation_Access_Array_Access;
   --  Import the variant representations from ``Variants`` into ``Self``.
   --
   --  ``Context`` must be a human-readable description of the contanining
   --  entity being loaded, for error reporting purposes.

   function Load_Numerical_Expression
     (Self       : in out Repinfo_Collection_Data;
      Expression : JSON_Value;
      Context    : String) return Numerical_Expression_Access;
   --  Import the numerical expression that ``Expression`` denotes into
   --  ``Self``.  Return null if this expression cannot be evaluated.
   --
   --  ``Context`` must be a human-readable description of the expression being
   --  loaded, for error reporting purposes.

   function Load_Rational
     (Self       : in out Repinfo_Collection_Data;
      Expression : JSON_Value;
      Context    : String) return Rational_Access;
   --  Import the rational number that ``Expression`` denotes into ``Self``.
   --
   --  ``Context`` must be a human-readable description of the expression being
   --  loaded, for error reporting purposes.

   ----------------
   -- Allocators --
   ----------------

   function Allocate_Integer
     (Self : in out Repinfo_Collection_Data) return Integer_Access;
   --  Allocate a big integer for ``Self``

   function Allocate_Rational
     (Self : in out Repinfo_Collection_Data) return Rational_Access;
   --  Allocate a rational number for ``Self``

   ---------------------
   -- Pool allocators --
   ---------------------

   subtype Record_Type_Subtype is Type_Representation_Data (Record_Type);
   type Record_Type_Access is access all Record_Type_Subtype;
   pragma No_Strict_Aliasing (Record_Type_Access);
   package Record_Type_Allocator is new Alloc
     (Record_Type_Subtype, Record_Type_Access);
   function Allocate_Record_Type
     (Pool : Bump_Ptr_Pool) return Type_Representation_Access;

   subtype Array_Type_Subtype is Type_Representation_Data (Array_Type);
   type Array_Type_Access is access all Array_Type_Subtype;
   pragma No_Strict_Aliasing (Array_Type_Access);
   package Array_Type_Allocator is new Alloc
     (Array_Type_Subtype, Array_Type_Access);
   function Allocate_Array_Type
     (Pool : Bump_Ptr_Pool) return Type_Representation_Access;

   subtype Fixed_Type_Subtype is
     Type_Representation_Data (Internal_Fixed_Type);
   type Fixed_Type_Access is access all Fixed_Type_Subtype;
   pragma No_Strict_Aliasing (Fixed_Type_Access);
   package Fixed_Type_Allocator is new Alloc
     (Fixed_Type_Subtype, Fixed_Type_Access);
   function Allocate_Fixed_Type
     (Pool : Bump_Ptr_Pool) return Type_Representation_Access;

   subtype Other_Type_Subtype is Type_Representation_Data (Other_Type);
   type Other_Type_Access is access all Other_Type_Subtype;
   pragma No_Strict_Aliasing (Other_Type_Access);
   package Other_Type_Allocator is new Alloc
     (Other_Type_Subtype, Other_Type_Access);
   function Allocate_Other_Type
     (Pool : Bump_Ptr_Pool) return Type_Representation_Access;

   package Numerical_Expression_Allocator is new Alloc
     (Numerical_Expression_Data, Numerical_Expression_Access);
   function Allocate_Numerical_Expression
     (Pool : Bump_Ptr_Pool) return Numerical_Expression_Access
   renames Numerical_Expression_Allocator.Alloc;

   type Expr_Node_Wrapper is record
      Data : aliased Expr_Node_Data;
   end record;
   type Expr_Node_Wrapper_Access is access all Expr_Node_Wrapper;
   pragma No_Strict_Aliasing (Expr_Node_Wrapper_Access);
   package Expr_Node_Allocator is new Alloc
     (Expr_Node_Wrapper, Expr_Node_Wrapper_Access);
   function Allocate_Expr_Node (Pool : Bump_Ptr_Pool) return Expr_Node_Access;

   package Variant_Representation_Allocator is new Alloc
     (Variant_Representation_Data, Variant_Representation_Access);
   function Allocate_Variant_Representation
     (Pool : Bump_Ptr_Pool) return Variant_Representation_Access
   renames Variant_Representation_Allocator.Alloc;

   package Component_Representation_Allocator is new Alloc
     (Component_Representation_Data, Component_Representation_Access);
   function Allocate_Component_Representation
     (Pool : Bump_Ptr_Pool) return Component_Representation_Access
   renames Component_Representation_Allocator.Alloc;

   -----------------------------
   -- Constants for JSON keys --
   -----------------------------

   Key_Alignment   : constant String := "Alignment";
   Key_Object_Size : constant String := "Object_Size";
   Key_Size        : constant String := "Size";
   Key_Value_Size  : constant String := "Value_Size";

   -------------------
   -- Type_Kind_For --
   -------------------

   function Type_Kind_For (Decl : Base_Type_Decl'Class) return Type_Kind is
      Full : Base_Type_Decl := Decl.P_Full_View;
      Def  : Type_Def := No_Type_Def;
   begin
      --  Loop in order to traverse derived types until we can find the type
      --  kind.

      Def_Loop : loop
         if Full.Is_Null then
            raise Type_Mismatch_Error with
              "cannot find full view for " & Decl.Image;
         end if;

         --  First analyze the "type decl" layer

         Full_Loop : loop
            case Ada_Base_Type_Decl (Full.Kind) is
               when Ada_Type_Decl =>
                  Def := Full.As_Type_Decl.F_Type_Def;
                  if Full.Is_Null then
                     raise Type_Mismatch_Error with
                       "incomplete parse tree for " & Full.Image;
                  end if;
                  exit Full_Loop;

               when Ada_Subtype_Decl =>
                  Full :=
                    (Full.As_Subtype_Decl
                     .F_Subtype
                     .P_Designated_Type_Decl
                     .P_Full_View);

               when Ada_Task_Type_Decl_Range =>
                  return Task_Type;

               when Ada_Protected_Type_Decl =>
                  return Protected_Type;

               when Ada_Incomplete_Type_Decl_Range =>
                  raise Type_Mismatch_Error with
                    "cannot find full view for " & Full.Image;

               when Ada_Discrete_Base_Subtype_Decl
                  | Ada_Classwide_Type_Decl =>
                  Full := Full.Parent.As_Base_Type_Decl.P_Full_View;
            end case;
         end loop Full_Loop;

         --  Then analyze the "type def" layer, if any

         case Ada_Type_Def (Def.Kind) is
            when Ada_Access_Def               => return Access_Type;
            when Ada_Enum_Type_Def            => return Enumeration_Type;
            when Ada_Signed_Int_Type_Def      => return Signed_Type;
            when Ada_Mod_Int_Type_Def         => return Modular_Type;
            when Ada_Floating_Point_Def       => return Floating_Type;
            when Ada_Decimal_Fixed_Point_Def  => return Decimal_Type;
            when Ada_Ordinary_Fixed_Point_Def => return Ordinary_Type;
            when Ada_Array_Type_Def           => return Array_Type;
            when Ada_Interface_Type_Def       => return Interface_Type;
            when Ada_Record_Type_Def          => return Record_Type;

            when Ada_Private_Type_Def =>
               raise Type_Mismatch_Error with
                 "cannot find full view for " & Full.Image;

            when Ada_Formal_Discrete_Type_Def =>
               raise Type_Mismatch_Error with
                 "cannot resolve type in uninstantiated generic " & Full.Image;

            when Ada_Derived_Type_Def =>
               Full :=
                 (Def.As_Derived_Type_Def
                  .F_Subtype_Indication
                  .P_Designated_Type_Decl
                  .P_Full_View);
         end case;
      end loop Def_Loop;
   end Type_Kind_For;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (Self       : Numerical_Expression_Access;
      Collection : Repinfo_Collection)
      return Numerical_Expression is
   begin
      if Self = null then
         return (raise Unsupported_Expression);
      else
         return (Collection, Self);
      end if;
   end Wrap;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (Self        : Type_Representation_Access;
      Declaration : Base_Type_Decl'Class;
      Collection  : Repinfo_Collection)
      return Type_Representation
   is
      Variant_Part_Node : Variant_Part := No_Variant_Part;
      Variants          : Variant_Representation_Access_Array_Access;

      procedure Process
        (Decl : Concrete_Type_Decl;
         Def  : Type_Def;
         Repr : Type_Representation_Access);
      --  Ensure that ``Decl``/``Def`` and ``Repr`` are consistent regarding
      --  variant parts (either both have one, either one have it). Update the
      --  ``Variant_Part_Node`` and ``Variants`` local variables above
      --  accordingly. Since it is illegal in Ada for a record extension chain
      --  to have more than one variant part, also check that we do not exceed
      --  this here.

      -------------
      -- Process --
      -------------

      procedure Process
        (Decl : Concrete_Type_Decl;
         Def  : Type_Def;
         Repr : Type_Representation_Access)
      is
         VP : constant Variant_Part := Get_Component_List (Def).F_Variant_Part;
      begin
         --  Make sure that the presence of a variant part in ``Decl``/``Def``
         --  is consistent with ``Repr``.

         if VP.Is_Null then
            if Repr.Variants /= null then
               raise Type_Mismatch_Error with
                 Decl.Image & " has no variant, repinfo inconsistent";
            end if;

         else
            if Repr.Variants = null then
               raise Type_Mismatch_Error with
                 Decl.Image & " has a variant, repinfo inconsistent";

            --  Also make sure we have a single variant part in the whole
            --  extension chain.

            elsif not Variant_Part_Node.Is_Null then
               raise Type_Mismatch_Error with
                 VP.Image & " conflicts with " & Variant_Part_Node.Image;
            end if;

            Variant_Part_Node := VP;
            Variants := Repr.Variants;
         end if;
      end Process;

      Kind : Type_Kind;

   begin
      if Self = null then
         return No_Type_Representation;
      else
         if Self.Kind = Record_Type then
            Iterate_Derivations
              (Collection, Declaration, Self, Process'Access);
         end if;

         Kind := Type_Kind_For (Declaration);
         if (case Kind is
             when Record_Type => Self.Kind /= Record_Type,
             when Array_Type  => Self.Kind /= Array_Type,
             when Fixed_Type => Self.Kind /= Internal_Fixed_Type,
             when others => Self.Kind /= Other_Type)
         then
            raise Type_Mismatch_Error with
              Declaration.Image & " has unexpected type kind " & Kind'Image;
         end if;

         return (Collection,
                 Self,
                 Declaration.As_Base_Type_Decl,
                 Kind,
                 Variant_Part_Node,
                 Variants);
      end if;
   end Wrap;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (Self        : Variant_Representation_Access;
      Declaration : Variant;
      Collection  : Repinfo_Collection)
      return Variant_Representation is
   begin
      if Self = null then
         return No_Variant_Representation;
      else
         return (Collection, Self, Declaration);
      end if;
   end Wrap;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (Self        : Component_Representation_Access;
      Declaration : Defining_Name;
      Collection  : Repinfo_Collection)
      return Component_Representation is
   begin
      if Self = null then
         return No_Component_Representation;
      else
         return (Collection, Self, Declaration);
      end if;
   end Wrap;

   -------------------------
   -- Iterate_Derivations --
   -------------------------

   procedure Iterate_Derivations
     (Collection : Repinfo_Collection;
      Decl       : Base_Type_Decl'Class;
      Repr       : Type_Representation_Access;
      Process    : access procedure (Decl : Concrete_Type_Decl;
                                     Def  : Type_Def;
                                     Repr : Type_Representation_Access))
   is
      --  Get the full view for ``Self`` and ensure it is a record type, as
      --  well as the type that ``Repr`` describes.

      CTD : constant Concrete_Type_Decl :=
        Decl.P_Full_View.As_Concrete_Type_Decl;
      Def : constant Type_Def := CTD.F_Type_Def;
   begin
      if not CTD.P_Is_Record_Type then
         raise Type_Mismatch_Error with
           "record type expected, got " & CTD.Image;
      elsif Repr.Kind /= Record_Type then
         raise Type_Mismatch_Error with
           "record type repinfo expected, got " & Repr.Kind'Image;
      end if;

      --  If this is a derived type, first iterate on the type that is derived
      --  (the base type).

      if Def.Kind = Ada_Derived_Type_Def then
         declare
            --  Start resolving the base type

            Base_Decl_Ref : constant Subtype_Indication :=
              Def.As_Derived_Type_Def.F_Subtype_Indication;
            Base_Decl     : constant Base_Type_Decl :=
              Base_Decl_Ref.P_Designated_Type_Decl;
            Base_Repr     : Type_Representation;
         begin
            if Base_Decl.Is_Null then
               raise Type_Mismatch_Error with
                 "cannot resolve " & Base_Decl_Ref.Image;
            end if;

            --  Then look for its type representation information

            Base_Repr := Collection.Lookup (Base_Decl);
            if Is_Null (Base_Repr) then
               raise Type_Mismatch_Error with
                 "cannot find repinfo for " & Base_Decl.Image;
            end if;

            --  Continue the recursion on that base type

            Iterate_Derivations
              (Collection => Collection,
               Decl       => Base_Decl,
               Repr       => Base_Repr.Data,
               Process    => Process);
         end;
      end if;

      Process.all (CTD, Def, Repr);
   end Iterate_Derivations;

   ------------------------
   -- Get_Component_List --
   ------------------------

   function Get_Component_List (Def : Type_Def) return Component_List is
   begin
      return
        (case Def.Kind is
         when Ada_Record_Type_Def =>
            Def.As_Record_Type_Def.F_Record_Def.F_Components,
         when Ada_Derived_Type_Def =>
            Def.As_Derived_Type_Def.F_Record_Extension.F_Components,
         when others =>
            raise Program_Error);
   end Get_Component_List;

   ----------------------------
   -- Component_Associations --
   ----------------------------

   function Component_Associations
     (Collection : Repinfo_Collection;
      Decl       : Base_Type_Decl'Class;
      Repr       : Type_Representation_Access)
      return Component_Association_Vectors.Vector
   is
      Coll  : Repinfo_Collection_Data renames
        Collection.Unchecked_Get.all;

      Result : Component_Association_Vectors.Vector;

      package Symbol_Sets is new Ada.Containers.Hashed_Sets
        (Element_Type        => Symbol_Type,
         Hash                => Hash,
         Equivalent_Elements => "=");
      Artificial_Components : Symbol_Sets.Set;
      --  Set of component names for artificial components that were included
      --  in ``Result``. Unlike components that come from sources, which can be
      --  de-duplicated using the correspendance between parse trees and
      --  repinfo, artificial components are present only in repinfo: we need
      --  this set to de-duplicate them based on their names.

      function Register_Artificial
        (Repr : Component_Representation_Access) return Boolean;
      --  If ``Repr`` designates an artificial component and this is the first
      --  one we inspect with such a name, append it to ``Result``. Do nothing
      --  in all other cases.

      procedure Process
        (Decl : Concrete_Type_Decl;
         Def  : Type_Def;
         Repr : Type_Representation_Access);
      --  Process a record type declaration

      -------------------------
      -- Register_Artificial --
      -------------------------

      function Register_Artificial
        (Repr : Component_Representation_Access) return Boolean
      is
         use Symbol_Sets;
         Position : Cursor;
         Inserted : Boolean;

         --  If ``Repr`` does not designate an artificial component, do not
         --  register it.

         Name : Text_Type renames Repr.Name.all;
      begin
         if Name /= "" and then Name (Name'First) /= '_' then
            return False;
         end if;

         --  Otherwise try to register it. If there is already a component with
         --  the same name, just return False. Otherwise, register/append it
         --  and return True.

         Artificial_Components.Insert (Repr.Name, Position, Inserted);
         if Inserted then
            Result.Append ((No_Defining_Name, Repr));
         end if;
         return Inserted;
      end Register_Artificial;

      -------------
      -- Process --
      -------------

      procedure Process
        (Decl : Concrete_Type_Decl;
         Def  : Type_Def;
         Repr : Type_Representation_Access)
      is
         Comps : Component_Representation_Access_Array renames
           Repr.Components.all;

         --  Go through all discriminants/components in ``Decl`` and in
         --  ``Repr`` in parallel: their components are supposed to appear in
         --  the same order.
         --
         --  The lists in ``Decl`` are considered to be the reference (they are
         --  assumed correct) whereas the list in ``Repr`` is considered
         --  unreliably (the compiler mechanisms to produce has a very subtle
         --  behavior, and component lists may vary/be incorrect).
         --
         --  Try to find information for all components in ``Decl``, but
         --  discard entries in ``Repr`` that have no corresponding declaration
         --  *and* that are not artificial (artificial components have no
         --  declaration).

         Repr_Index : Positive := 1;
         --  Index of the next component in ``Repr`` to process

         function Next_Component_Repr
           (For_Comp : Defining_Name) return Component_Representation_Access;
         --  Return the next component representation (in ``Comps``) to process
         --  and increment ``Repr_Index``, or raise a ``Type_Mismatch_Error``
         --  exception if there is no component left.

         procedure Append (Decl : Base_Formal_Param_Decl);
         --  Append entries to ``Result`` for all defining names in ``Decl``

         -------------------------
         -- Next_Component_Repr --
         -------------------------

         function Next_Component_Repr
           (For_Comp : Defining_Name) return Component_Representation_Access is
         begin
            if Repr_Index > Comps'Last then
               raise Type_Mismatch_Error with
                 "cannot find repinfo for " & For_Comp.Image;
            end if;
            return Result : constant Component_Representation_Access :=
              Comps (Repr_Index)
            do
               Repr_Index := Repr_Index + 1;
            end return;
         end Next_Component_Repr;

         ------------
         -- Append --
         ------------

         procedure Append (Decl : Base_Formal_Param_Decl) is
         begin
            --  For each defining name in ``Decl``, look for a corresponding
            --  component representation info.

            Defining_Names : for DN of Decl.P_Defining_Names loop
               Search : loop
                  declare
                     Repr : constant Component_Representation_Access :=
                       Next_Component_Repr (DN);
                  begin
                     --  If ``Repr`` corresponds to an artificial component,
                     --  try to append it to ``Result`` and resume the search
                     --  for a repinfo for ``DN``.

                     if Register_Artificial (Repr) then
                        null;

                     --  Otherwise, skip entries until we find a match for
                     --  ``DN``.

                     elsif Symbolize (Coll, DN.Text) = Repr.Name then
                        Result.Append ((DN, Repr));
                        exit Search;
                     end if;
                  end;
               end loop Search;
            end loop Defining_Names;
         end Append;

         --  Collect discriminants, if any. Note that when there are
         --  discriminants, the full view should always have a list of known
         --  discriminants.

         DP : constant Discriminant_Part := Decl.F_Discriminants;

      begin
         if not DP.Is_Null then
            for D of DP.As_Known_Discriminant_Part.F_Discr_Specs loop
               Append (D.As_Base_Formal_Param_Decl);
            end loop;
         end if;

         --  Now collect regular components defined in ``Def`` itself

         for Comp of Get_Component_List (Def).F_Components loop
            if Comp.Kind = Ada_Component_Decl then
               Append (Comp.As_Base_Formal_Param_Decl);
            end if;
         end loop;

         --  Consume all remaining component repinfo, just in case they contain
         --  an artificial component that we have not registered yet.

         while Repr_Index in Repr.Components'Range loop
            declare
               Repr : constant Component_Representation_Access :=
                 Next_Component_Repr (No_Defining_Name);
               Dummy : constant Boolean := Register_Artificial (Repr);
            begin
               null;
            end;
         end loop;
      end Process;

   begin
      Iterate_Derivations (Collection, Decl, Repr, Process'Access);
      return Result;
   end Component_Associations;

   ----------------------
   -- Extract_Variants --
   ----------------------

   function Extract_Variants
     (Collection : Repinfo_Collection;
      Decl       : Ada_Node'Class;
      Variants   : Variant_Representation_Access_Array;
      Part_Node  : Variant_Part) return Variant_Representation_Array
   is
      List_Node : Variant_List;
   begin
      --  Make sure that the record type has a variant part, and that it has
      --  the correct number of variants.

      if Part_Node.Is_Null then
         raise Type_Mismatch_Error with
           "cannot find variant part in " & Decl.Image;
      end if;

      List_Node := Part_Node.F_Variant;
      if List_Node.Children_Count /= Variants'Length then
         raise Type_Mismatch_Error with
            Decl.Image & " has" & List_Node.Children_Count'Image
            & " variants, but" & Variants'Length'Image & " expected";
      end if;

      return Result : Variant_Representation_Array (Variants'Range) do
         for I in Result'Range loop
            Result (I) := Wrap
              (Self        => Variants (I),
               Declaration => List_Node.Child (I).As_Variant,
               Collection  => Collection);
         end loop;
      end return;
   end Extract_Variants;

   ---------------
   -- Load_JSON --
   ---------------

   function Load_JSON (Filename : String) return JSON_Value is
      use GNATCOLL.Mmap;

      File    : Mapped_File;
      Region  : Mapped_Region;
      Content : Short.Str_Access;
      Result  : Read_Result;
   begin
      --  Read the content of the file to parse through memory-mapping

      begin
         File := Open_Read (Filename);
      exception
         when Exc : Ada.IO_Exceptions.Name_Error =>
            raise Loading_Error with Exception_Message (Exc);
      end;
      Region := Read (File => File, Length => Length (File));
      Content := Short.Data (Region);

      --  Run the JSON parser, bail out on error

      Result := Read (Content.all (1 .. Short.Last (Region)));
      Free (Region);
      Close (File);
      if not Result.Success then
         raise Loading_Error with
           Filename & ": " & Format_Parsing_Error (Result.Error);
      end if;

      return Result.Value;
   end Load_JSON;

   ---------------
   -- Symbolize --
   ---------------

   function Symbolize
     (Self : in out Repinfo_Collection_Data; Name : String) return Symbol_Type
   is
   begin
      --  TODO??? This assumes that names are plain ASCII, since the encoding
      --  in JSON files created by -gnatRj seems to be arbitrary. We might have
      --  to fix this one day.

      return Symbolize (Self, To_Text (Name));
   end Symbolize;

   ---------------
   -- Symbolize --
   ---------------

   function Symbolize
     (Self : in out Repinfo_Collection_Data;
      Name : Text_Type) return Symbol_Type is
   begin
      return Find (Self.Symbols, To_Lower (Name));
   end Symbolize;

   ---------------
   -- Load_Type --
   ---------------

   function Load_Type
     (Self   : in out Repinfo_Collection_Data;
      Entity : JSON_Value;
      Name   : String) return Type_Representation_Access
   is
      function Load_Bit_Order (Field : String) return System.Bit_Order;
      --  Load a bit order attribute (``Field``) from ``Entity``

      --------------------
      -- Load_Bit_Order --
      --------------------

      function Load_Bit_Order (Field : String) return System.Bit_Order is
      begin
         if Entity.Has_Field (Field)
            and then Entity.Get (Field).Kind = JSON_String_Type
         then
            declare
               Value : constant String := Entity.Get (Field);
            begin
               if Value = "System.Low_Order_First" then
                  return System.Low_Order_First;
               elsif Value = "System.High_Order_First" then
                  return System.High_Order_First;
               end if;
            end;
         end if;

         return (raise Loading_Error with
                 Name & "/" & Field & ": invalid bit order");
      end Load_Bit_Order;

      Alignment               : Natural;
      Object_Size, Value_Size : Numerical_Expression_Access;
   begin
      --  The kind of type that is loaded is inferred from the JSON fields
      --  present, hence the heuristics below.
      --
      --  First try to load common attributes. If Alignment is missing, this is
      --  probably not an entity we are interested in.

      if not Entity.Has_Field (Key_Alignment) then
         Trace.Trace
           ("Missing mandatory attribute ("
            & Key_Alignment & "): skipping type");
         return null;
      elsif Entity.Get (Key_Alignment).Kind /= JSON_Int_Type then
         raise Loading_Error with Name & ": invalid alignment";
      end if;
      Alignment := Entity.Get (Key_Alignment);

      --  All types provide either:
      --
      --  * the Size attribute, when the Object_Size=Value_Size;
      --  * the couple Object_Size/Value_Size, when they are different;
      --  * none, when the size is unknown (for instance unconstrained array
      --    types).

      if Entity.Has_Field ("Size") then
         Object_Size := Load_Numerical_Expression
           (Self, Entity.Get (Key_Size), Name & "/" & Key_Size);
         Value_Size := Object_Size;
      elsif Entity.Has_Field ("Object_Size") then
         Object_Size := Load_Numerical_Expression
           (Self,
            Entity.Get (Key_Object_Size),
            Name & "/" & Key_Object_Size);
         Value_Size := Load_Numerical_Expression
           (Self,
            Entity.Get (Key_Value_Size),
            Name & "/" & Key_Value_Size);
      end if;

      --  Now infer the type of entity

      if Entity.Has_Field ("record") then

         --  This looks like a record type

         return Result : constant Type_Representation_Access :=
           Allocate_Record_Type (Self.Pool)
         do
            Result.Alignment := Alignment;
            Result.Object_Size := Object_Size;
            Result.Value_Size := Value_Size;
            Result.Scalar_Storage_Order :=
              Load_Bit_Order ("Scalar_Storage_Order");
            Result.Components :=
              Load_Components (Self, Entity.Get ("record"), Name);
            Result.Variants :=
              (if Entity.Has_Field ("variant")
               then Load_Variants
                      (Self, Entity.Get ("variant"), Name & "/variant")
               else null);
            Result.Bit_Order := Load_Bit_Order ("Bit_Order");

            Result.Discriminant_Count := 0;
            for C of Result.Components.all loop
               if C.Discriminant_Number > 0 then
                  Result.Discriminant_Count := Result.Discriminant_Count + 1;
               end if;
            end loop;
         end return;

      elsif Entity.Has_Field ("Component_Size") then

         --  This looks like an array type

         return Result : constant Type_Representation_Access :=
           Allocate_Array_Type (Self.Pool)
         do
            Result.Alignment := Alignment;
            Result.Object_Size := Object_Size;
            Result.Value_Size := Value_Size;
            Result.Scalar_Storage_Order :=
              Load_Bit_Order ("Scalar_Storage_Order");
            Result.Component_Size := Load_Numerical_Expression
              (Self, Entity.Get ("Component_Size"), Name & "/Component_Size");
         end return;

      elsif Entity.Has_Field ("Small") then

         --  This looks like a fixed point type

         return Result : constant Type_Representation_Access :=
           Allocate_Fixed_Type (Self.Pool)
         do
            Result.Alignment := Alignment;
            Result.Object_Size := Object_Size;
            Result.Value_Size := Value_Size;
            Result.Small :=
              Load_Rational (Self, Entity.Get ("Small"), Name & "/Small");

            if not Entity.Has_Field ("Range")
               or else Entity.Get ("Range").Kind /= JSON_Array_Type
               or else Length (JSON_Array'(Entity.Get ("Range"))) /= 2
            then
               raise Loading_Error with Name & ": invalid/missing Range";
            end if;
            declare
               Bounds_Object : constant JSON_Value := Entity.Get ("Range");
               Bounds        : constant JSON_Array := Bounds_Object.Get;
            begin
               Result.Range_First :=
                 Load_Rational (Self, Get (Bounds, 1), Name & "/Range_First");
               Result.Range_Last :=
                 Load_Rational (Self, Get (Bounds, 2), Name & "/Range_Last");
            end;
         end return;

      elsif Entity.Has_Field ("mechanism") then

         --  This entity describes a subprogram: we do not handle it

         return null;

      else
         --  Assume this entity describes a number type that is not a fixed
         --  point type.

         return Result : constant Type_Representation_Access :=
           Allocate_Other_Type (Self.Pool)
         do
            Result.Alignment := Alignment;
            Result.Object_Size := Object_Size;
            Result.Value_Size := Value_Size;
         end return;
      end if;
   end Load_Type;

   ---------------------
   -- Load_Components --
   ---------------------

   function Load_Components
     (Self       : in out Repinfo_Collection_Data;
      Components : JSON_Value;
      Context    : String) return Component_Representation_Access_Array_Access
   is
      package Component_Vectors is new Langkit_Support.Vectors
        (Component_Representation_Access);

      Result    : Component_Vectors.Vector;
      Comp_Repr : Component_Representation_Access;
   begin
      if Components.Kind /= JSON_Array_Type then
         raise Loading_Error with Context & ": invalid list of components";
      end if;

      for Comp of JSON_Array'(Components.Get) loop

         --  Try to get the name of this component

         if Comp.Kind /= JSON_Object_Type
            or else not Comp.Has_Field ("name")
            or else Comp.Get ("name").Kind /= JSON_String_Type
         then
            raise Loading_Error with Context & ": invalid component";
         end if;

         declare
            Name       : constant String := Comp.Get ("name");
            Key        : constant Symbol_Type := Symbolize (Self, Name);
            Subcontext : constant String := Context & "/" & Name;
         begin
            Comp_Repr := Allocate_Component_Representation (Self.Pool);
            Comp_Repr.Name := Key;

            --  Load mandatory fields

            if not Comp.Has_Field ("Position")
               or else not Comp.Has_Field ("First_Bit")
               or else Comp.Get ("First_Bit").Kind /= JSON_Int_Type
               or else not Comp.Has_Field ("Size")
            then
               raise Loading_Error with
                 Subcontext & ": missing/invalid attributes";
            end if;

            Comp_Repr.Position := Load_Numerical_Expression
              (Self, Comp.Get ("Position"), Subcontext & "/Position");
            Comp_Repr.First_Bit := Comp.Get ("First_Bit");
            Comp_Repr.Size := Load_Numerical_Expression
              (Self, Comp.Get ("Size"), Subcontext & "/Size");

            --  Import discriminant information

            if Comp.Has_Field ("discriminant") then
               if Comp.Get ("discriminant").Kind /= JSON_Int_Type then
                  raise Loading_Error with
                    Subcontext & ": invalid ""discriminant"" attribute";
               end if;
               Comp_Repr.Discriminant_Number := Comp.Get ("discriminant");

            else
               Comp_Repr.Discriminant_Number := 0;
            end if;

            Result.Append (Comp_Repr);
         end;
      end loop;

      --  Turn the ``Result`` vector into the array access we need to return,
      --  then free resources for it.

      return R : constant Component_Representation_Access_Array_Access :=
        new Component_Representation_Access_Array (1 .. Result.Last_Index)
      do
         Self.Component_Repinfo_Arrays.Append (R);
         for I in R.all'Range loop
            R.all (I) := Result.Get (I);
         end loop;
         Result.Destroy;
      end return;

   exception
      when others =>
         Result.Destroy;
         raise;
   end Load_Components;

   -------------------
   -- Load_Variants --
   -------------------

   function Load_Variants
     (Self     : in out Repinfo_Collection_Data;
      Variants : JSON_Value;
      Context  : String) return Variant_Representation_Access_Array_Access
   is
      package Variant_Vectors is new Langkit_Support.Vectors
        (Variant_Representation_Access);

      Result       : Variant_Vectors.Vector;
      Variant_Repr : Variant_Representation_Access;
   begin
      if Variants.Kind /= JSON_Array_Type then
         raise Loading_Error with Context & ": invalid list of variants";
      end if;

      for Variant of JSON_Array'(Variants.Get) loop

         --  All variants have a "present" expression and a "record" array

         if Variant.Kind /= JSON_Object_Type
            or else not Variant.Has_Field ("present")
            or else not Variant.Has_Field ("record")
         then
            raise Loading_Error with Context & ": invalid variant";
         end if;

         Variant_Repr := Allocate_Variant_Representation (Self.Pool);
         Variant_Repr.Present := Load_Numerical_Expression
           (Self, Variant.Get ("present"), Context & "/variant");
         Variant_Repr.Components :=
           Load_Components (Self, Variant.Get ("record"), Context & "/record");

         --  Load the sub-variant part, if present

         Variant_Repr.Subvariants :=
           (if Variant.Has_Field ("variant")
            then Load_Variants
                   (Self, Variant.Get ("variant"), Context & "/variant")
            else null);

         Result.Append (Variant_Repr);
      end loop;

      --  Turn the ``Result`` vector into the array access we need to return,
      --  then free resources for it.

      return R : constant Variant_Representation_Access_Array_Access :=
        new Variant_Representation_Access_Array (1 .. Result.Last_Index)
      do
         Self.Variant_Repinfo_Arrays.Append (R);
         for I in R.all'Range loop
            R.all (I) := Result.Get (I);
         end loop;
         Result.Destroy;
      end return;

   exception
      when others =>
         Result.Destroy;
         raise;
   end Load_Variants;

   -------------------------------
   -- Load_Numerical_Expression --
   -------------------------------

   function Load_Numerical_Expression
     (Self       : in out Repinfo_Collection_Data;
      Expression : JSON_Value;
      Context    : String) return Numerical_Expression_Access
   is
      Discriminant_Count : Natural := 0;
      --  Highest index for required discriminants

      procedure Error with No_Return;
      --  Raise a ``Loading_Error`` because the expression to load is invalid

      function Load_Expr (Subexpr : JSON_Value) return Expr_Node_Access;
      --  Convert the given numerical subexpression into our internal
      --  representation for it.

      -----------
      -- Error --
      -----------

      procedure Error is
      begin
         raise Loading_Error with Context & ": invalid expression";
      end Error;

      ---------------
      -- Load_Expr --
      ---------------

      function Load_Expr (Subexpr : JSON_Value) return Expr_Node_Access is
         Result_Access : constant Expr_Node_Access :=
           Allocate_Expr_Node (Self.Pool);
         Result        : Expr_Node_Data
         with Import, Address => Result_Access.all'Address;
      begin
         case Subexpr.Kind is
            when JSON_Int_Type =>
               declare
                  Value : constant Long_Long_Integer := Subexpr.Get;
               begin
                  Result :=
                    (Code => Literal, Value => Allocate_Integer (Self));

                  --  The largest value for Value (``Value'Last``) may be too
                  --  large to be passed as a scalar to any of the
                  --  ``GNATCOLL.GMP.Integers.Set`` overloads. Go through its
                  --  image to avoid range check failures.

                  Result.Value.Set (Value'Image);
               end;

            when JSON_Object_Type =>

               --  This subexpression is an operation: sanitize it and import
               --  it.

               if not Subexpr.Has_Field ("code")
                  or else Subexpr.Get ("code").Kind /= JSON_String_Type
                  or else not Subexpr.Has_Field ("operands")
                  or else Subexpr.Get ("operands").Kind /= JSON_Array_Type
               then
                  Error;
               end if;

               declare
                  Code : constant String := Subexpr.Get ("code");
                  Op   : Opcode_1;

                  JSON_Ops : constant JSON_Array := Subexpr.Get ("operands");
                  Operands : array (1 .. Length (JSON_Ops))
                             of Expr_Node_Access;
               begin
                  --  First, process special opcodes

                  if Code = "var" then

                     --  "Dynamic_Val" is a placeholder for "unsupported"

                     return null;

                  elsif Code = "#" then

                     --  This is the "get discriminant value" opcode. Decode it
                     --  fully and keep track of the highest discriminant
                     --  number.

                     if Length (JSON_Ops) /= 1
                        or else Get (JSON_Ops, 1).Kind /= JSON_Int_Type
                     then
                        Error;
                     end if;
                     Result :=
                       (Code                => Discrim_Val,
                        Discriminant_Number => Get (JSON_Ops, 1).Get);
                     Discriminant_Count := Natural'Max
                       (Discriminant_Count, Result.Discriminant_Number);
                     return Result_Access;
                  end if;

                  --  Decode all operands. If one of them is unsupported the
                  --  whole expression is unsupported.

                  for I in Operands'Range loop
                     Operands (I) := Load_Expr (Get (JSON_Ops, I));
                     if Operands (I) = null then
                        return null;
                     end if;
                  end loop;

                  if Code = "?<>" then
                     Op := Cond_Expr;
                  elsif Code = "+" then
                     Op := Plus_Expr;
                  elsif Code = "-" then
                     Op := (if Operands'Length = 1
                            then Negate_Expr
                            else Minus_Expr);
                  elsif Code = "*" then
                     Op := Mult_Expr;
                  elsif Code = "/t" then
                     Op := Trunc_Div_Expr;
                  elsif Code = "/c" then
                     Op := Ceil_Div_Expr;
                  elsif Code = "/f" then
                     Op := Floor_Div_Expr;
                  elsif Code = "modt" then
                     Op := Trunc_Mod_Expr;
                  elsif Code = "modc" then
                     Op := Ceil_Mod_Expr;
                  elsif Code = "modf" then
                     Op := Floor_Mod_Expr;
                  elsif Code = "/e" then
                     Op := Exact_Div_Expr;
                  elsif Code = "min" then
                     Op := Min_Expr;
                  elsif Code = "max" then
                     Op := Max_Expr;
                  elsif Code = "abs" then
                     Op := Abs_Expr;
                  elsif Code = "and" then
                     Op := Truth_And_Expr;
                  elsif Code = "or" then
                     Op := Truth_Or_Expr;
                  elsif Code = "xor" then
                     Op := Truth_Xor_Expr;
                  elsif Code = "not" then
                     Op := Truth_Not_Expr;
                  elsif Code = "<" then
                     Op := Lt_Expr;
                  elsif Code = "<=" then
                     Op := Le_Expr;
                  elsif Code = ">" then
                     Op := Gt_Expr;
                  elsif Code = ">=" then
                     Op := Ge_Expr;
                  elsif Code = "==" then
                     Op := Eq_Expr;
                  elsif Code = "!=" then
                     Op := Ne_Expr;
                  elsif Code = "&" then
                     Op := Bit_And_Expr;
                  else
                     Error;
                  end if;

                  --  Finally, check that we have the correct number of
                  --  operands.

                  declare
                     Result_Template   : Expr_Node_Data (Op);
                     Expected_Op_Count : constant Positive :=
                       (if Op in Opcode_3 then 3
                        elsif Op in Opcode_2 then 2
                        else 1);
                  begin
                     if Operands'Length /= Expected_Op_Count then
                        Error;
                     end if;

                     Result_Template.Op_1 := Operands (1);
                     if Op in Opcode_2 then
                        Result_Template.Op_2 := Operands (2);
                        if Op in Opcode_3 then
                           Result_Template.Op_3 := Operands (3);
                        end if;
                     end if;

                     Result := Result_Template;
                  end;
               end;

            when JSON_String_Type =>
               if String'(Subexpr.Get) = "??" then
                  return null;
               else
                  Error;
               end if;

            when others =>
               Error;
         end case;

         return Result_Access;
      end Load_Expr;

      Root : constant Expr_Node_Access := Load_Expr (Expression);
   begin
      if Root = null then
         return null;
      end if;

      return Result : constant Numerical_Expression_Access :=
        Allocate_Numerical_Expression (Self.Pool)
      do
         Result.Discriminant_Count := Discriminant_Count;
         Result.Root := Root;
      end return;
   end Load_Numerical_Expression;

   -------------------
   -- Load_Rational --
   -------------------

   function Load_Rational
     (Self       : in out Repinfo_Collection_Data;
      Expression : JSON_Value;
      Context    : String) return Rational_Access
   is
      procedure Set (Result : in out GMP_RN.Rational; Value : JSON_Value);
      --  Set ``Result`` to the given JSON decimal value

      procedure Error with No_Return;
      --  Raise a ``Loading_Error`` because the expression to load is invalid

      ---------
      -- Set --
      ---------

      procedure Set (Result : in out GMP_RN.Rational; Value : JSON_Value) is
      begin
         Result.Set (GNATCOLL.GMP.Double (Value.Get_Long_Float));
      end Set;

      -----------
      -- Error --
      -----------

      procedure Error is
      begin
         raise Loading_Error with Context & ": invalid expression";
      end Error;

   begin
      --  Rational numbers are encoded in two forms..

      case Expression.Kind is
         when JSON_Float_Type =>

            --  A straight float number in the JSON

            return Result : constant Rational_Access :=
              Allocate_Rational (Self)
            do
               Set (Result.all, Expression);
            end return;

         when JSON_Object_Type =>

            --  A simple expression: {"cond": "/", operands: [X, Y]}

            if not Expression.Has_Field ("code")
               or else Expression.Get ("code").Kind /= JSON_String_Type
               or else String'(Expression.Get ("code")) /= "/"
               or else not Expression.Has_Field ("operands")
               or else Expression.Get ("operands").Kind /= JSON_Array_Type
            then
               Error;
            end if;

            declare
               Operands : constant JSON_Array := Expression.Get ("operands");
               Num, Den : GMP_RN.Rational;
            begin
               if Length (Operands) /= 2
                  or else Get (Operands, 1).Kind /= JSON_Float_Type
                  or else Get (Operands, 2).Kind /= JSON_Float_Type
               then
                  Error;
               end if;

               Set (Num, Get (Operands, 1));
               Set (Den, Get (Operands, 2));

               return Result : constant Rational_Access :=
                 Allocate_Rational (Self)
               do
                  Result.Set (GMP_RN."/" (Num, Den));
               end return;
            end;

         when others =>
            Error;
      end case;
   end Load_Rational;

   ----------------------
   -- Allocate_Integer --
   ----------------------

   function Allocate_Integer
     (Self : in out Repinfo_Collection_Data) return Integer_Access
   is
   begin
      return Result : constant Integer_Access := new GMP_Int.Big_Integer do
         Self.Integers.Append (Result);
      end return;
   end Allocate_Integer;

   -----------------------
   -- Allocate_Rational --
   -----------------------

   function Allocate_Rational
     (Self : in out Repinfo_Collection_Data) return Rational_Access is
   begin
      return Result : constant Rational_Access := new GMP_RN.Rational do
         Self.Rationals.Append (Result);
      end return;
   end Allocate_Rational;

   --------------------------
   -- Allocate_Record_Type --
   --------------------------

   function Allocate_Record_Type
     (Pool : Bump_Ptr_Pool) return Type_Representation_Access
   is
      Result : constant Record_Type_Access :=
        Record_Type_Allocator.Alloc (Pool);
      Kind   : Internal_Type_Kind with Import, Address => Result.Kind'Address;
   begin
      Kind := Record_Type;
      return Type_Representation_Access (Result);
   end Allocate_Record_Type;

   -------------------------
   -- Allocate_Array_Type --
   -------------------------

   function Allocate_Array_Type
     (Pool : Bump_Ptr_Pool) return Type_Representation_Access
   is
      Result : constant Array_Type_Access := Array_Type_Allocator.Alloc (Pool);
      Kind   : Internal_Type_Kind with Import, Address => Result.Kind'Address;
   begin
      Kind := Array_Type;
      return Type_Representation_Access (Result);
   end Allocate_Array_Type;

   -------------------------
   -- Allocate_Fixed_Type --
   -------------------------

   function Allocate_Fixed_Type
     (Pool : Bump_Ptr_Pool) return Type_Representation_Access
   is
      Result : constant Fixed_Type_Access := Fixed_Type_Allocator.Alloc (Pool);
      Kind   : Internal_Type_Kind with Import, Address => Result.Kind'Address;
   begin
      Kind := Internal_Fixed_Type;
      return Type_Representation_Access (Result);
   end Allocate_Fixed_Type;

   -------------------------
   -- Allocate_Other_Type --
   -------------------------

   function Allocate_Other_Type
     (Pool : Bump_Ptr_Pool) return Type_Representation_Access
   is
      Result : constant Other_Type_Access := Other_Type_Allocator.Alloc (Pool);
      Kind   : Internal_Type_Kind with Import, Address => Result.Kind'Address;
   begin
      Kind := Other_Type;
      return Type_Representation_Access (Result);
   end Allocate_Other_Type;

   ------------------------
   -- Allocate_Expr_Node --
   ------------------------

   function Allocate_Expr_Node (Pool : Bump_Ptr_Pool) return Expr_Node_Access
   is
      Wrapper : constant Expr_Node_Wrapper_Access :=
        Expr_Node_Allocator.Alloc (Pool);
   begin
      return Wrapper.Data'Access;
   end Allocate_Expr_Node;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Numerical_Expression) return Boolean is
   begin
      return Self.Data = null;
   end Is_Null;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Type_Representation) return Boolean is
   begin
      return Self.Data = null;
   end Is_Null;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Component_Representation) return Boolean is
   begin
      return Self.Data = null;
   end Is_Null;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Variant_Representation) return Boolean is
   begin
      return Self.Data = null;
   end Is_Null;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Type_Representation) return Type_Kind is
   begin
      return Self.Kind;
   end Kind;

   ---------------
   -- Alignment --
   ---------------

   function Alignment (Self : Type_Representation) return Positive is
   begin
      return Self.Data.Alignment;
   end Alignment;

   --------------------------
   -- Scalar_Storage_Order --
   --------------------------

   function Scalar_Storage_Order
     (Self : Type_Representation) return System.Bit_Order is
   begin
      return Self.Data.Scalar_Storage_Order;
   end Scalar_Storage_Order;

   -----------------
   -- Object_Size --
   -----------------

   function Object_Size
     (Self : Type_Representation) return Numerical_Expression is
   begin
      return Wrap (Self.Data.Object_Size, Self.Collection);
   end Object_Size;

   ----------------
   -- Value_Size --
   ----------------

   function Value_Size
     (Self : Type_Representation) return Numerical_Expression
   is
   begin
      return Wrap (Self.Data.Value_Size, Self.Collection);
   end Value_Size;

   ----------------
   -- Components --
   ----------------

   function Components
     (Self : Type_Representation) return Component_Representation_Array
   is
      Assocs : constant Component_Association_Vectors.Vector :=
        Component_Associations (Self.Collection, Self.Declaration, Self.Data);
   begin
      return Result : Component_Representation_Array
                        (1 .. Natural (Assocs.Length))
      do
         for I in Result'Range loop
            declare
               A : Component_Association renames Assocs.Constant_Reference (I);
            begin
               Result (I) := Wrap (A.Repr, A.Decl, Self.Collection);
            end;
         end loop;
      end return;
   end Components;

   -------------------
   -- Discriminants --
   -------------------

   function Discriminants
     (Self : Type_Representation) return Component_Representation_Array
   is
      Comps : constant Component_Representation_Array := Components (Self);
      DN    : Natural;
   begin
      return Result : Component_Representation_Array
                        (1 .. Self.Data.Discriminant_Count)
      do
         for C of Comps loop
            DN := Discriminant_Number (C);
            if DN > 0 then
               Result (DN) := C;
            end if;
         end loop;
      end return;
   end Discriminants;

   ----------------------
   -- Has_Variant_Part --
   ----------------------

   function Has_Variant_Part (Self : Type_Representation) return Boolean is
   begin
      return Self.Variants /= null;
   end Has_Variant_Part;

   --------------
   -- Variants --
   --------------

   function Variants
     (Self : Type_Representation) return Variant_Representation_Array is
   begin
      return Extract_Variants
        (Collection => Self.Collection,
         Decl       => Self.Declaration,
         Variants   => Self.Variants.all,
         Part_Node  => Self.Variant_Part_Node);
   end Variants;

   ---------------
   -- Bit_Order --
   ---------------

   function Bit_Order (Self : Type_Representation) return System.Bit_Order is
   begin
      return Self.Data.Bit_Order;
   end Bit_Order;

   -------------
   -- Present --
   -------------

   function Present
     (Self : Variant_Representation) return Numerical_Expression is
   begin
      return Wrap (Self.Data.Present, Self.Collection);
   end Present;

   ----------------
   -- Components --
   ----------------

   function Components
     (Self : Variant_Representation) return Component_Representation_Array
   is
      Comps      : Component_Representation_Access_Array renames
        Self.Data.Components.all;
      Comp_Decls : Defining_Name_Vectors.Vector;
   begin
      --  Compute the list of defining names for all components in this
      --  variant. Raise an error if the count is not right.

      for Comp of Self.Declaration.F_Components.F_Components loop
         if Comp.Kind = Ada_Component_Decl then
            for DN of Comp.As_Component_Decl.P_Defining_Names loop
               Comp_Decls.Append (DN);
            end loop;
         end if;
      end loop;
      if Comps'Length /= Natural (Comp_Decls.Length) then
         raise Type_Mismatch_Error with
           Self.Declaration.Image & " has" & Comp_Decls.Length'Image
           & " components, but" & Comps'Length'Image & " expected";
      end if;

      return Result : Component_Representation_Array (Comps'Range) do
         for I in Result'Range loop
            Result (I) := Wrap
              (Self        => Comps (I),
               Declaration => Comp_Decls.Element (I),
               Collection  => Self.Collection);
         end loop;
      end return;
   end Components;

   --------------------
   -- Has_Subvariant --
   --------------------

   function Has_Subvariant_Part (Self : Variant_Representation) return Boolean
   is
   begin
      return Self.Data.Subvariants /= null;
   end Has_Subvariant_Part;

   -----------------
   -- Subvariants --
   -----------------

   function Subvariants
     (Self : Variant_Representation) return Variant_Representation_Array is
   begin
      return Extract_Variants
        (Collection => Self.Collection,
         Decl       => Self.Declaration,
         Variants   => Self.Data.Subvariants.all,
         Part_Node  => Self.Declaration.F_Components.F_Variant_Part);
   end Subvariants;

   -----------------
   -- Declaration --
   -----------------

   function Declaration
     (Self : Component_Representation) return Defining_Name is
   begin
      return Self.Declaration;
   end Declaration;

   --------------------
   -- Component_Name --
   --------------------

   function Component_Name (Self : Component_Representation) return Text_Type
   is
   begin
      return Self.Data.Name.all;
   end Component_Name;

   -------------------------
   -- Discriminant_Number --
   -------------------------

   function Discriminant_Number
     (Self : Component_Representation) return Natural is
   begin
      return Self.Data.Discriminant_Number;
   end Discriminant_Number;

   --------------
   -- Position --
   --------------

   function Position
     (Self : Component_Representation) return Numerical_Expression is
   begin
      return Wrap (Self.Data.Position, Self.Collection);
   end Position;

   ---------------
   -- First_Bit --
   ---------------

   function First_Bit (Self : Component_Representation) return Natural is
   begin
      return Self.Data.First_Bit;
   end First_Bit;

   ----------
   -- Size --
   ----------

   function Size (Self : Component_Representation) return Numerical_Expression
   is
   begin
      return Wrap (Self.Data.Size, Self.Collection);
   end Size;

   --------------------
   -- Component_Size --
   --------------------

   function Component_Size
     (Self : Type_Representation) return Numerical_Expression is
   begin
      return Wrap (Self.Data.Component_Size, Self.Collection);
   end Component_Size;

   -----------
   -- Small --
   -----------

   function Small (Self : Type_Representation) return GMP_RN.Rational is
   begin
      return Result : GMP_RN.Rational do
         Result.Set (Self.Data.Small.all);
      end return;
   end Small;

   -----------------
   -- Range_First --
   -----------------

   function Range_First (Self : Type_Representation) return GMP_RN.Rational is
   begin
      return Result : GMP_RN.Rational do
         Result.Set (Self.Data.Range_First.all);
      end return;
   end Range_First;

   ----------------
   -- Range_Last --
   ----------------

   function Range_Last (Self : Type_Representation) return GMP_RN.Rational is
   begin
      return Result : GMP_RN.Rational do
         Result.Set (Self.Data.Range_Last.all);
      end return;
   end Range_Last;

   ------------------------
   -- Discriminant_Count --
   ------------------------

   function Discriminant_Count (Self : Numerical_Expression) return Natural is
   begin
      return Self.Data.Discriminant_Count;
   end Discriminant_Count;

   ------------------------
   -- Discriminant_Value --
   ------------------------

   function Discriminant_Value
     (Result : Eval_Result) return GMP_Int.Big_Integer is
   begin
      case Result.Kind is
         when Enum_Lit =>
            return Result.Enum_Result.P_Enum_Rep;

         when Int =>
            return Value : GMP_Int.Big_Integer do
               Value.Set (Result.Int_Result);
            end return;

         when others =>
            raise Invalid_Discriminant;
      end case;
   end Discriminant_Value;

   ---------------
   -- Dump_Expr --
   ---------------

   procedure Dump_Expr (Node : Expr_Node_Access) is

      --  Operator associativity: higher priority number means higher
      --  associativity: we needs parens around an operand when its priority is
      --  lower than its embedding expression's.
      --
      --  * Cond_Expr: lowest priority (1)
      --  * Logic binary operators (and then, or else, xor): 2
      --  * Relational operators (<, <=, ...): 3
      --  * Addition, substraction: 4
      --  * Multiplications, divisions, remainders: 5
      --  * Bitwise operators (and, or): 6
      --  * Unary operators (not, abs, -), discriminants and literals: 7
      --
      --  Min and max have special treatment since they already contain parens:
      --  we never need extra parens to disambiguate.

      Priorities : constant array (Opcode) of Natural :=
        (Cond_Expr      => 1,

         Plus_Expr      => 4,
         Minus_Expr     => 4,

         Mult_Expr      => 5,
         Trunc_Div_Expr => 5,
         Ceil_Div_Expr  => 5,
         Floor_Div_Expr => 5,
         Trunc_Mod_Expr => 5,
         Ceil_Mod_Expr  => 5,
         Floor_Mod_Expr => 5,
         Exact_Div_Expr => 5,

         Min_Expr       => 0,
         Max_Expr       => 0,

         Truth_And_Expr => 1,
         Truth_Or_Expr  => 1,
         Truth_Xor_Expr => 1,

         Lt_Expr        => 3,
         Le_Expr        => 3,
         Gt_Expr        => 3,
         Ge_Expr        => 3,
         Eq_Expr        => 3,
         Ne_Expr        => 3,

         Bit_And_Expr   => 6,
         Negate_Expr    => 7,
         Abs_Expr       => 7,
         Truth_Not_Expr => 7,
         Discrim_Val    => 7,
         Literal        => 7);

      procedure Dump (Node : Expr_Node_Access; Outer_Priority : Natural);
      --  Dump the ``Node`` subexpression. ``Outer_Priority`` is the priority
      --  of the parent expression: it is used to determine if parens are
      --  needed around this subexpression.

      ----------
      -- Dump --
      ----------

      procedure Dump (Node : Expr_Node_Access; Outer_Priority : Natural) is
         Inner_Priority : constant Natural := Priorities (Node.Code);
         Parens_Needed : constant Boolean := Inner_Priority < Outer_Priority;

         procedure D
           (Node           : Expr_Node_Access;
            Outer_Priority : Natural := Inner_Priority);
         --  Shortcut for ``Dump (Node, Inner_Priority)``

         -------
         -- D --
         -------

         procedure D
           (Node           : Expr_Node_Access;
            Outer_Priority : Natural := Inner_Priority) is
         begin
            Dump (Node, Outer_Priority);
         end D;
      begin
         if Parens_Needed then
            Put ("(");
         end if;
         case Node.Code is
            when Cond_Expr =>
               --  To help readability, force the use of parens if operands are
               --  Cond_Expr themselves.

               Put ("if ");
               D (Node.Op_1, 2);
               Put (" then ");
               D (Node.Op_2, 2);
               Put (" else ");
               D (Node.Op_3);
            when Plus_Expr =>
               D (Node.Op_1);
               Put (" + ");
               D (Node.Op_2);
            when Minus_Expr =>
               D (Node.Op_1);
               Put (" - ");
               D (Node.Op_2);
            when Mult_Expr =>
               D (Node.Op_1);
               Put (" * ");
               D (Node.Op_2);
            when Trunc_Div_Expr =>
               D (Node.Op_1);
               Put (" /t ");
               D (Node.Op_2);
            when Ceil_Div_Expr =>
               D (Node.Op_1);
               Put (" /c ");
               D (Node.Op_2);
            when Floor_Div_Expr =>
               D (Node.Op_1);
               Put (" /f ");
               D (Node.Op_2);
            when Trunc_Mod_Expr =>
               D (Node.Op_1);
               Put (" modt ");
               D (Node.Op_2);
            when Ceil_Mod_Expr =>
               D (Node.Op_1);
               Put (" modc ");
               D (Node.Op_2);
            when Floor_Mod_Expr =>
               D (Node.Op_1);
               Put (" modf ");
               D (Node.Op_2);
            when Exact_Div_Expr =>
               D (Node.Op_1);
               Put (" /e ");
               D (Node.Op_2);
            when Min_Expr =>
               Put ("min(");
               D (Node.Op_1, 0);
               Put (", ");
               D (Node.Op_2, 0);
               Put (")");
            when Max_Expr =>
               Put ("max(");
               D (Node.Op_1);
               Put (", ");
               D (Node.Op_2);
               Put (")");
            when Truth_And_Expr =>
               D (Node.Op_1);
               Put (" and then ");
               D (Node.Op_2);
            when Truth_Or_Expr =>
               D (Node.Op_1);
               Put (" or else ");
               D (Node.Op_2);
            when Truth_Xor_Expr =>
               D (Node.Op_1);
               Put (" xor ");
               D (Node.Op_2);
            when Lt_Expr =>
               D (Node.Op_1);
               Put (" < ");
               D (Node.Op_2);
            when Le_Expr =>
               D (Node.Op_1);
               Put (" <= ");
               D (Node.Op_2);
            when Gt_Expr =>
               D (Node.Op_1);
               Put (" > ");
               D (Node.Op_2);
            when Ge_Expr =>
               D (Node.Op_1);
               Put (" >= ");
               D (Node.Op_2);
            when Eq_Expr =>
               D (Node.Op_1);
               Put (" = ");
               D (Node.Op_2);
            when Ne_Expr =>
               D (Node.Op_1);
               Put (" /= ");
               D (Node.Op_2);
            when Bit_And_Expr =>
               D (Node.Op_1);
               Put (" and ");
               D (Node.Op_2);
            when Negate_Expr =>
               Put ("-");
               D (Node.Op_1);
            when Abs_Expr =>
               Put ("abs ");
               D (Node.Op_1);
            when Truth_Not_Expr =>
               Put ("not ");
               D (Node.Op_1);
            when Discrim_Val =>
               declare
                  Disc : constant String := Node.Discriminant_Number'Image;
               begin
                  Put ("#" & Disc (Disc'First + 1 .. Disc'Last));
               end;
            when Literal =>
               Put (Node.Value.Image);
         end case;
         if Parens_Needed then
            Put (")");
         end if;
      end Dump;

   begin
      Dump (Node, 0);
      New_Line;
   end Dump_Expr;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Self          : Numerical_Expression;
      Discriminants : Discriminant_Values) return GMP_Int.Big_Integer
   is
      use type GMP_Int.Big_Integer;

      function "+" (Self : GMP_Int.Big_Integer) return GMP_Int.Big_Integer;
      --  Create a copy of ``Self``

      function Eval (Node : Expr_Node_Access) return GMP_Int.Big_Integer;
      --  Evaluate the ``Node`` subexpression

      function Eval_As_Bool (Node : Expr_Node_Access) return Boolean;
      --  Likewise, but convert the result to a boolean (zero is False,
      --  everything else is True).

      ---------
      -- "+" --
      ---------

      function "+" (Self : GMP_Int.Big_Integer) return GMP_Int.Big_Integer is
      begin
         return Result : GMP_Int.Big_Integer do
            Result.Set (Self);
         end return;
      end "+";

      ----------
      -- Eval --
      ----------

      function Eval (Node : Expr_Node_Access) return GMP_Int.Big_Integer is
      begin
         case Node.Code is
            when Cond_Expr =>
               declare
                  Cond : constant GMP_Int.Big_Integer := Eval (Node.Op_1);
               begin
                  return (if Cond = Zero
                          then Eval (Node.Op_3)
                          else Eval (Node.Op_2));
               end;

            when Plus_Expr =>
               return Eval (Node.Op_1) + Eval (Node.Op_2);
            when Minus_Expr =>
               return Eval (Node.Op_1) - Eval (Node.Op_2);
            when Mult_Expr =>
               return Eval (Node.Op_1) * Eval (Node.Op_2);

            when Trunc_Div_Expr =>
               return GMP_Int.Truncate_Divide
                 (Eval (Node.Op_1), Eval (Node.Op_2));
            when Ceil_Div_Expr =>
               return GMP_Int.Ceil_Divide
                 (Eval (Node.Op_1), Eval (Node.Op_2));
            when Floor_Div_Expr =>
               return GMP_Int.Floor_Divide
                 (Eval (Node.Op_1), Eval (Node.Op_2));

            when Trunc_Mod_Expr =>
               return GMP_Int.Truncate_Remainder
                 (Eval (Node.Op_1), Eval (Node.Op_2));
            when Floor_Mod_Expr =>
               return GMP_Int.Floor_Remainder
                 (Eval (Node.Op_1), Eval (Node.Op_2));
            when Ceil_Mod_Expr =>
               return GMP_Int.Ceil_Remainder
                 (Eval (Node.Op_1), Eval (Node.Op_2));

            when Exact_Div_Expr =>
               return Eval (Node.Op_1) / Eval (Node.Op_2);

            when Min_Expr =>
               declare
                  L : constant GMP_Int.Big_Integer := Eval (Node.Op_1);
                  R : constant GMP_Int.Big_Integer := Eval (Node.Op_2);
               begin
                  return (if L < R then +L else +R);
               end;
            when Max_Expr =>
               declare
                  L : constant GMP_Int.Big_Integer := Eval (Node.Op_1);
                  R : constant GMP_Int.Big_Integer := Eval (Node.Op_2);
               begin
                  return (if L < R then +R else +L);
               end;

            when Truth_And_Expr =>
               return (if Eval_As_Bool (Node.Op_1)
                          and then Eval_As_Bool (Node.Op_2)
                       then +One
                       else +Zero);
            when Truth_Or_Expr =>
               return (if Eval_As_Bool (Node.Op_1)
                          or else Eval_As_Bool (Node.Op_2)
                       then +One
                       else +Zero);
            when Truth_Xor_Expr =>
               return (if Eval_As_Bool (Node.Op_1) /= Eval_As_Bool (Node.Op_2)
                       then +One
                       else +Zero);

            when Lt_Expr =>
               return (if Eval (Node.Op_1) < Eval (Node.Op_2)
                       then +One
                       else +Zero);
            when Le_Expr =>
               return (if Eval (Node.Op_1) <= Eval (Node.Op_2)
                       then +One
                       else +Zero);
            when Gt_Expr =>
               return (if Eval (Node.Op_1) > Eval (Node.Op_2)
                       then +One
                       else +Zero);
            when Ge_Expr =>
               return (if Eval (Node.Op_1) >= Eval (Node.Op_2)
                       then +One
                       else +Zero);
            when Eq_Expr =>
               return (if Eval (Node.Op_1) = Eval (Node.Op_2)
                       then +One
                       else +Zero);
            when Ne_Expr =>
               return (if Eval (Node.Op_1) /= Eval (Node.Op_2)
                       then +One
                       else +Zero);

            when Bit_And_Expr =>
               return Eval (Node.Op_1) and Eval (Node.Op_2);

            when Negate_Expr =>
               return -Eval (Node.Op_1);

            when Abs_Expr =>
               return abs Eval (Node.Op_1);
            when Truth_Not_Expr =>
               return (if Eval_As_Bool (Node.Op_1) then +Zero else +One);
            when Discrim_Val =>
               return Result : GMP_Int.Big_Integer do
                  Result.Set (Discriminants (Node.Discriminant_Number));
               end return;
            when Literal =>
               return +Node.Value.all;
         end case;
      end Eval;

      ------------------
      -- Eval_As_Bool --
      ------------------

      function Eval_As_Bool (Node : Expr_Node_Access) return Boolean is
      begin
         return Eval (Node) /= Zero;
      end Eval_As_Bool;

   begin
      return Eval (Self.Data.Root);
   end Evaluate;

   ---------------------
   -- Resolved_Record --
   ---------------------

   function Resolved_Record
     (Self          : Type_Representation;
      Discriminants : Discriminant_Values) return Resolved_Record_Type
   is
      package Resolved_Component_Vectors is new Ada.Containers.Vectors
        (Positive, Resolved_Component);
      use Resolved_Component_Vectors;

      Comps : Vector;

      procedure Append (C : Component_Representation);
      --  Resolve ``C`` and add the result to ``Comps``

      procedure Process_Variant_Part (Variants : Variant_Representation_Array);
      --  Call ``Append`` on all components of the variant in ``Variants`` that
      --  is active according to ``Discriminants``.

      function Evaluate (Expr : Numerical_Expression) return Size_Type;
      --  Shortcut to evaluate an expression using ``Discriminants`` and to
      --  return the result as a ``Size_Type``.

      ------------
      -- Append --
      ------------

      procedure Append (C : Component_Representation) is
         Decl            : constant Defining_Name := Declaration (C);
         Artificial_Name : constant Text_Type :=
           (if Decl.Is_Null
            then Component_Name (C)
            else "");
      begin
         Comps.Append
           (Resolved_Component'
              (Declaration     => Decl,
               Artificial_Name => To_Unbounded_Text (Artificial_Name),
               Position        => Evaluate (Position (C)),
               First_Bit       => First_Bit (C),
               Size            => Evaluate (Size (C))));
      end Append;

      --------------------------
      -- Process_Variant_Part --
      --------------------------

      procedure Process_Variant_Part (Variants : Variant_Representation_Array)
      is
         Is_Present : GMP_Int.Big_Integer;
      begin
         --  Select the first variant whose "Present" expression evaluates to
         --  non-zero.

         for V of Variants loop
            Is_Present.Set (Evaluate (Present (V), Discriminants));
            if not GMP_Int."=" (Is_Present, Zero) then

               --  Add resolved components from this variant, and from
               --  potential subvariants.

               for C of Components (V) loop
                  Append (C);
               end loop;
               if Has_Subvariant_Part (V) then
                  Process_Variant_Part (Subvariants (V));
               end if;

               return;
            end if;
         end loop;
      end Process_Variant_Part;

      --------------
      -- Evaluate --
      --------------

      function Evaluate (Expr : Numerical_Expression) return Size_Type is
         use type GMP_Int.Big_Integer;

         Result : constant GMP_Int.Big_Integer :=
           Evaluate (Expr, Discriminants);
      begin
         if Result < Zero or else Result > Size_Last then
            raise Resolution_Error;
         end if;
         return Size_Type'Value (Result.Image);
      end Evaluate;

   begin
      --  Add resolved components from the "root" record and from variant parts

      for C of Components (Self) loop
         Append (C);
      end loop;
      if Has_Variant_Part (Self) then
         Process_Variant_Part (Variants (Self));
      end if;

      --  Resolve other attributes

      return Result : Resolved_Record_Type (Natural (Comps.Length)) do
         Result.Alignment := Alignment (Self);
         Result.Object_Size := Evaluate (Object_Size (Self));
         Result.Value_Size := Evaluate (Value_Size (Self));
         Result.Bit_Order := Bit_Order (Self);
         Result.Scalar_Storage_Order := Scalar_Storage_Order (Self);
         for Cur in Comps.Iterate loop
            Result.Components (To_Index (Cur)) := Element (Cur);
         end loop;
      end return;
   end Resolved_Record;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Self : Repinfo_Collection;
      Decl : Base_Type_Decl'Class) return Type_Representation
   is
      use Type_Representation_Maps;
   begin
      if Self.Is_Null or else Decl.Is_Null then
         return No_Type_Representation;
      end if;

      declare
         Collection : Repinfo_Collection_Data renames Self.Unchecked_Get.all;

         --  Build a symbol for the fully qualified name of this type

         FQN : constant Text_Type := Decl.P_Fully_Qualified_Name;
         Key : constant Symbol_Type := Symbolize (Collection, FQN);

         --  Look for a corresponding type representation entry

         Cur : constant Cursor := Collection.Type_Representations.Find (Key);
      begin
         Trace.Trace ("Looking for representation of " & Image (FQN));
         return (if Has_Element (Cur)
                 then Wrap (Element (Cur), Decl, Self)
                 else No_Type_Representation);
      end;
   end Lookup;

   ----------
   -- Load --
   ----------

   function Load (Filenames : Filename_Array) return Repinfo_Collection is
      package Origin_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Symbol_Type,
         Element_Type    => Unbounded_String,
         Hash            => Hash,
         Equivalent_Keys => "=");
      Origins : Origin_Maps.Map;
      --  Mapping from the fully qualified name of imported entity to the
      --  JSON file name from which it was imported. Used for duplicates error
      --  reporting.

      --  Build the collection in place: create a refcounted pointer to an
      --  empty collection (``Result_Ref``), then get a raw access to the
      --  collection (``Result``), and finally fill the collection in.

      Result_Ref : Repinfo_Collection;
      Result     : Collection_Refs.Element_Access;
   begin
      Result_Ref.Set (Repinfo_Collection_Data'(others => <>));
      Result := Result_Ref.Unchecked_Get;

      --  Initialize internal tables

      Result.Pool := Create;
      Result.Symbols := Create_Symbol_Table;

      --  Import all entities from the parsing of all input files

      for XFilename of Filenames loop
         declare
            Filename : constant String := To_String (XFilename);
            Root     : JSON_Value;
            Entities : JSON_Array;
         begin
            if Trace.Is_Active then
               Trace.Trace ("Loading from " & Filename);
               Trace.Increase_Indent;
            end if;

            Root := Load_JSON (Filename);
            if Root.Kind /= JSON_Array_Type then
               raise Loading_Error with Filename & ": invalid root element";
            end if;
            Entities := Root.Get;

            for Entity of Entities loop
               if Entity.Kind /= JSON_Object_Type
                  or else not Entity.Has_Field ("name")
                  or else Entity.Get ("name").Kind /= JSON_String_Type
               then
                  raise Loading_Error with Filename & ": invalid entity found";
               end if;

               declare
                  Name      : constant String := Entity.Get ("name");
                  Key       : constant Symbol_Type :=
                    Symbolize (Result.all, Name);
                  Type_Repr : Type_Representation_Access;

                  --  Check that this is the first entity description for that
                  --  name.

                  Previous_From : Origin_Maps.Cursor;
                  Inserted      : Boolean;
               begin
                  Origins.Insert (Key, XFilename, Previous_From, Inserted);
                  if not Inserted then
                     raise Loading_Error with
                       Filename & ": duplicate entry for " & Name & " (from "
                       & To_String (Origins.Reference (Previous_From)) & ")";
                  end if;

                  --  Do the import iff we handle this kind of entity

                  if Trace.Is_Active then
                     Trace.Trace ("Loading entry: " & Name);
                     Trace.Increase_Indent;
                  end if;
                  begin
                     Type_Repr := Load_Type (Result.all, Entity, Name);
                  exception
                     when others =>
                        if Trace.Is_Active then
                           Trace.Decrease_Indent;
                        end if;
                        raise;
                  end;
                  if Trace.Is_Active then
                     Trace.Decrease_Indent;
                  end if;

                  if Type_Repr /= null then
                     Result.Type_Representations.Insert (Key, Type_Repr);
                  end if;
               end;
            end loop;

            --  Make sure the trace is decreased when terminating or aborting
            --  the iteration.

            if Trace.Is_Active then
               Trace.Decrease_Indent;
            end if;
         exception
            when others =>
               if Trace.Is_Active then
                  Trace.Decrease_Indent;
               end if;
               raise;
         end;
      end loop;

      return Result_Ref;
   end Load;

   ---------------------------
   -- Load_From_Directories --
   ---------------------------

   function Load_From_Directories
     (Name_Pattern : GNAT.Regexp.Regexp := Default_JSON_Filename_Regexp;
      Directories  : Filename_Array) return Repinfo_Collection
   is
      use GNATCOLL.VFS;
      Files : File_Array_Access;
   begin
      --  Convert Directories to File_Array and find all the JSON files to load

      declare
         Dirs : File_Array (Directories'Range);
      begin
         for I in Dirs'Range loop
            Dirs (I) := Create (+To_String (Directories (I)));
         end loop;
         Files := Find_Files_Regexp (Name_Pattern, Dirs);

         --  Sort files, for the determinism of the output

         Sort (Files.all);
      end;

      --  Now convert the list of files to the expected type

      declare
         Filenames : Filename_Array (Files.all'Range);
      begin
         for I in Filenames'Range loop
            Filenames (I) := To_Unbounded_String (+Files.all (I).Full_Name);
         end loop;
         Unchecked_Free (Files);

         --  Finally, load the JSON file

         return Load (Filenames);
      end;
   end Load_From_Directories;

   -------------------------
   -- Load_From_Directory --
   -------------------------

   function Load_From_Directory
     (Name_Pattern : GNAT.Regexp.Regexp := Default_JSON_Filename_Regexp;
      Directory    : String) return Repinfo_Collection is
   begin
      return Load_From_Directories
        (Name_Pattern, (1 => To_Unbounded_String (Directory)));
   end Load_From_Directory;

   -----------------------
   -- Load_From_Project --
   -----------------------

   function Load_From_Project
     (Tree    : GPR2.Project.Tree.Object;
      View    : GPR2.Project.View.Object := GPR2.Project.View.Undefined;
      Subdirs : String := "repinfo";
      Force   : Boolean := False) return Repinfo_Collection
   is
      use GNATCOLL.OS.FS;
      use GNATCOLL.OS.Process;
      use type GPR2.Filename_Optional;

      Filenames : GPR2.Path_Name.Set.Object;

      --  Build the command line for gprbuild

      Args : Argument_List;
   begin
      Args.Append ("gprbuild");
      Args.Append ("-c");
      Args.Append ("-P" & String (Tree.Root_Project.Path_Name.Value));

      --  Add "-X" arguments for external variables

      if Tree.Has_Context then
         for Cur in Tree.Context.Iterate loop
            declare
               use GPR2.Context.Key_Value;

               Name  : constant GPR2.Name_Type := Key (Cur);
               Value : constant GPR2.Value_Type := Element (Cur);
            begin
               Args.Append ("-X" & String (Name) & "=" & String (Value));
            end;
         end loop;
      end if;

      --  If there is a configuration project, pass "--config". Otherwise pass
      --  "--target" and "--RTS" arguments according to project settings.

      declare
         Config_Prj_File : constant GPR2.Path_Name.Object :=
           (if Tree.Has_Configuration
            then Tree.Configuration.Corresponding_View.Path_Name
            else GPR2.Path_Name.Undefined);
      begin
         if Config_Prj_File.Is_Defined and then Config_Prj_File.Exists then
            Args.Append ("--config=" & String (Config_Prj_File.Value));
         else
            Args.Append ("--target=" & String (Tree.Target));

            --  Determine the list of languages used in the project tree and
            --  emit one "--RTS:<lang>" argument for each that has a runtime.

            declare
               Langs : GPR2.Containers.Language_Set;
            begin
               for V of Tree.Root_Project.Closure loop
                  for L of V.Language_Ids loop
                     Langs.Include (L);
                  end loop;
               end loop;
               for L of Langs loop
                  declare
                     Runtime : constant String := String (Tree.Runtime (L));
                  begin
                     if Runtime'Length > 0 then
                        Args.Append
                          ("--RTS:" & String (GPR2.Name (L)) & "=" & Runtime);
                     end if;
                  end;
               end loop;
            end;
         end if;
      end;

      if Subdirs'Length > 0 then
         Args.Append ("--subdirs=" & Subdirs);
      end if;

      if Force then
         Args.Append ("-f");
      end if;

      --  Build all Ada sources with the -gnatR4js flag, to generate JSON
      --  repinfo in object directories.

      Args.Append ("-cargs:Ada");
      Args.Append ("-gnatR4js");

      --  If requested, print gprbuild arguments on the standard output

      if Trace.Is_Active then
         declare
            Msg : Unbounded_String :=
              To_Unbounded_String ("gprbuild arguments:");
         begin
            for A of Args loop
               Append (Msg, " " & A);
            end loop;
            Trace.Trace (To_String (Msg));
         end;
      end if;

      declare
         Exit_Code : constant Integer := Run
           (Args   => Args,
            Cwd    => "",
            Stdin  => Null_FD,
            Stdout => (if Trace.Is_Active then Standout else Null_FD),
            Stderr => (if Trace.Is_Active then Standerr else Null_FD));
      begin
         if Exit_Code /= 0 then
            raise Gprbuild_Error with
              "gprbuild exited with status code " & Exit_Code'Image;
         end if;
      end;

      --  Look for JSON files for all Ada sources

      declare
         Actual_View   : constant GPR2.Project.View.Object :=
           (if View.Is_Defined then View else Tree.Root_Project);
         Loaded_Subdir : constant GPR2.Filename_Optional := Tree.Subdirs;
         Obj_Dir       : GPR2.Path_Name.Object;
         Repinfo_File  : GPR2.Path_Name.Object;
      begin
         for V of Closure (Actual_View) loop
            Trace.Increase_Indent ("Processing subproject " & String (V.Name));

            --  If Tree was loaded with subdirs, remove it from Obj_Dir, then
            --  add our own, if requested. This should get us the object
            --  directory that gprbuild used.

            Obj_Dir := V.Object_Directory;
            if Loaded_Subdir'Length > 0 then
               pragma Assert (Obj_Dir.Simple_Name = Loaded_Subdir);
               Obj_Dir := Obj_Dir.Containing_Directory;
            end if;
            if Subdirs'Length > 0 then
               Obj_Dir := Obj_Dir.Compose
                 (GPR2.Filename_Type (Subdirs), Directory => True);
            end if;
            Trace.Trace ("Object directory: " & String (Obj_Dir.Value));

            for S of V.Sources loop
               if S.Is_Ada then
                  Trace.Trace
                    ("Processing Ada source: " & String (S.Path_Name.Value));
                  Repinfo_File :=
                    Obj_Dir.Compose (S.Path_Name.Simple_Name & ".json");
                  if Repinfo_File.Exists then
                     Trace.Trace ("Found: " & String (Repinfo_File.Value));
                     Filenames.Append (Repinfo_File);
                  else
                     Trace.Trace ("Not found: " & String (Repinfo_File.Value));
                  end if;
               end if;
            end loop;

            Trace.Decrease_Indent;
         end loop;
      end;

      --  Now that we have the list of JSON files to load, just build the
      --  result.

      declare
         F_List : Filename_Array (1 .. Natural (Filenames.Length));
         Next   : Positive := F_List'First;
      begin
         for F of Filenames loop
            F_List (Next) := To_Unbounded_String (String (F.Value));
            Next := Next + 1;
         end loop;
         return Load (F_List);
      end;
   end Load_From_Project;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Repinfo_Collection_Data) is
      Var_I   : Integer_Access;
      Var_R   : Rational_Access;
      Var_CRA : Component_Representation_Access_Array_Access;
      Var_VRA : Variant_Representation_Access_Array_Access;
   begin
      Free (Self.Pool);

      for I of Self.Integers loop
         Var_I := I;
         Free (Var_I);
      end loop;
      Self.Integers.Destroy;
      for R of Self.Rationals loop
         Var_R := R;
         Free (Var_R);
      end loop;
      Self.Rationals.Destroy;

      for CRA of Self.Component_Repinfo_Arrays loop
         Var_CRA := CRA;
         Free (Var_CRA);
      end loop;
      Self.Component_Repinfo_Arrays.Destroy;
      for VRA of Self.Variant_Repinfo_Arrays loop
         Var_VRA := VRA;
         Free (Var_VRA);
      end loop;
      Self.Variant_Repinfo_Arrays.Destroy;

      Destroy (Self.Symbols);
   end Release;

end Libadalang.Data_Decomposition;
