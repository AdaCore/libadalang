--
--  Copyright (C) 2022-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides routines to decompose composite program data into
--  elementary components, and more generally to query the representation of
--  program data.
--
--  .. ATTENTION::
--
--     This is an experimental feature, so even if it is exposed to allow
--     experiments, it is totally unsupported and the API is very likely to
--     change in the future.
--
--  Here is a small example of usage for this package:
--
--  .. code:: ada
--
--     --  Code that the program below analyzes, to be compiled with the
--     --  ``-gnatR4js`` option (``gcc -c pkg.ads -gnatR4js``) to generate the
--     --  ``pkg.ads.json`` file.
--
--     package Pkg is
--        type R (N : Natural) is record
--           case N is
--              when 0 .. 9 =>
--                 B : Boolean;
--              when 100 .. 199 | 900 .. 999 =>
--                 I : Integer;
--              when others =>
--                 null;
--           end case;
--        end record;
--     end Pkg;
--
--     --  Corresponding project file
--
--     project P is
--     end P;
--
--     --  Program using the ``Libadalang.Data_Decomposition`` API to analyze
--     --  representation information.
--
--     with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--     with Ada.Text_IO;           use Ada.Text_IO;
--
--     with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;
--     with GPR2.Context;
--     with GPR2.Path_Name;
--     with GPR2.Project.Tree;
--
--     with Libadalang.Analysis;           use Libadalang.Analysis;
--     with Libadalang.Common;             use Libadalang.Common;
--     with Libadalang.Data_Decomposition;
--     with Libadalang.Iterators;          use Libadalang.Iterators;
--     with Libadalang.Project_Provider;   use Libadalang.Project_Provider;
--
--     procedure Main is
--        package DDA renames Libadalang.Data_Decomposition;
--
--        --  Load the "p.gpr" project file, then create the unit provider (to
--        --  make Libadalang's name resolution work) and the representation
--        --  information collection for it.
--
--        Tree       : GPR2.Project.Tree.Object;
--        Provider   : Unit_Provider_Reference;
--        Collection : DDA.Repinfo_Collection;
--     begin
--        Tree.Load_Autoconf
--          (Filename => GPR2.Path_Name.Create_File
--                         (Name      => "p.gpr",
--                          Directory => GPR2.Path_Name.No_Resolution),
--           Context  => GPR2.Context.Empty);
--
--        Provider := Create_Project_Unit_Provider (Tree);
--
--        Collection := DDA.Load_From_Project (Tree);
--
--        declare
--           --  Analyze the "pkg.ads" source file
--
--           Ctx : constant Analysis_Context := Create_Context;
--           U   : constant Analysis_Unit := Ctx.Get_From_File ("pkg.ads");
--
--           --  Look for the first type declaration in "pkg.ads"
--
--           function Filter (Node : Ada_Node) return Boolean
--           is (Node.Kind in Ada_Base_Type_Decl);
--
--           Decl : constant Base_Type_Decl :=
--             Find_First (U.Root, Filter'Access).As_Base_Type_Decl;
--
--           --  Fetch type representation information
--
--           TR : constant DDA.Type_Representation :=
--             Collection.Lookup (Decl);
--
--           --  Resolve the layout for this record type for a specific
--           --  set of discriminants (N = 950).
--
--           Discs : constant DDA.Discriminant_Values :=
--             (1 => Make ("950"));
--           RR    : constant DDA.Resolved_Record_Type :=
--             DDA.Resolved_Record (TR, Discs);
--        begin
--           --  Display the size of this record (in bits) as well as the name
--           --  and position (in bytes) for each component.
--
--           Put_Line ("Record size:" & RR.Value_Size'Image);
--           for C of RR.Components loop
--              Put_Line ("Component " & C.Declaration.Image
--                        & " at position" & C.Position'Image);
--           end loop;
--        end;
--     end Main;
--
--  Expected output:
--
--  .. code:: text
--
--     Record size: 64
--     Component <DefiningName "N" pkg.ads:2:12-2:13> at position 0
--     Component <DefiningName "I" pkg.ads:7:13-7:14> at position 4

private with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with System;

with GNAT.Regexp;

with GNATCOLL.GMP.Integers;
with GNATCOLL.GMP.Rational_Numbers;
private with GNATCOLL.Refcount;
private with GNATCOLL.Traces;
with GPR2.Project.Tree;
with GPR2.Project.View;

private with Langkit_Support.Bump_Ptr;
private with Langkit_Support.Symbols;
with Langkit_Support.Text; use Langkit_Support.Text;
private with Langkit_Support.Vectors;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;

package Libadalang.Data_Decomposition is

   package GMP_Int renames GNATCOLL.GMP.Integers;
   package GMP_RN renames GNATCOLL.GMP.Rational_Numbers;

   type Numerical_Expression is private;
   --  Expression that, when evaluated, returns an integer (arbitrary
   --  precision).
   --
   --  This data type is used to represent dynamic attributes, such as the size
   --  of a discriminated record type, which depends on the specific values for
   --  its discriminants.
   --
   --  Each numerical expression requires a specific number of integer
   --  parameters to be evaluated, each integer corresponding to a record
   --  discriminant. See the ``Discriminant_Count`` and ``Evaluate`` primitives
   --  below.
   --
   --  Note that for record types, the ``Resolved_Record`` function below
   --  resolves all attributes/components at once for a given record type given
   --  values for its discriminants. This may be more convenient than going
   --  through the evaluation of numerical expressions manually.

   No_Numerical_Expression : constant Numerical_Expression;

   function Is_Null (Self : Numerical_Expression) return Boolean;

   Unsupported_Expression : exception;
   --  Exception raised in a function that returns ``Numerical_Expression``
   --  when the expression is unsupported. For now, this happens only when the
   --  expression depends on dynamic variables that are not discriminants.

   type Type_Kind is
     (Access_Type,
      Enumeration_Type,
      Signed_Type,
      Modular_Type,
      Floating_Type,
      Decimal_Type,
      Ordinary_Type,
      Array_Type,
      Interface_Type,
      Record_Type,
      Protected_Type,
      Task_Type);

   subtype Elementary_Type is Type_Kind range Access_Type .. Ordinary_Type;
   subtype Scalar_Type is
    Elementary_Type range Enumeration_Type .. Ordinary_Type;
   subtype Discrete_Type is Scalar_Type range Enumeration_Type .. Modular_Type;
   subtype Integer_Type is Discrete_Type range Signed_Type .. Modular_Type;
   subtype Real_Type is Scalar_Type range Floating_Type .. Ordinary_Type;
   subtype Fixed_Type is Real_Type range Decimal_Type .. Ordinary_Type;
   subtype Composite_Type is Type_Kind range Array_Type .. Task_Type;

   --  All the types below have shared pointer semantics

   type Type_Representation is private;
   --  Representation information for a given type

   type Component_Representation is private;
   --  Representation information for a given record component

   type Variant_Representation is private;
   --  Representation information for a given record variant (or "variant part
   --  alternative").

   No_Type_Representation      : constant Type_Representation;
   No_Component_Representation : constant Component_Representation;
   No_Variant_Representation   : constant Variant_Representation;

   function Is_Null (Self : Type_Representation) return Boolean;
   function Is_Null (Self : Component_Representation) return Boolean;
   function Is_Null (Self : Variant_Representation) return Boolean;

   type Component_Representation_Array is
     array (Positive range <>) of Component_Representation;

   type Variant_Representation_Array is
     array (Positive range <>) of Variant_Representation;

   Type_Mismatch_Error : exception;
   --  Exception raised when an inconsistency is detected between type
   --  representation information and type declarations as found in the
   --  sources.

   -----------------------
   -- Generic accessors --
   -----------------------

   function Kind (Self : Type_Representation) return Type_Kind
   with Pre => not Is_Null (Self);
   --  Return the kind of type ``Self`` is

   function Alignment (Self : Type_Representation) return Positive
   with Pre => not Is_Null (Self);
   --  Alignment for values of this type, in bytes

   function Object_Size
     (Self : Type_Representation) return Numerical_Expression
   with Pre => not Is_Null (Self);
   --  Number of bits used to hold objects whose type is ``Self`` (See GNAT RM
   --  9.6. Value_Size and Object_Size Clauses).

   function Value_Size
     (Self : Type_Representation) return Numerical_Expression
   with Pre => not Is_Null (Self);
   --  Number of bits required to represent a value whose type is ``Self``.
   --  This corresponds to the RM defined ``'Size`` attribute (See GNAT RM 9.6.
   --  Value_Size and Object_Size Clauses).

   function Scalar_Storage_Order
     (Self : Type_Representation) return System.Bit_Order
   with Pre => Kind (Self) in Composite_Type;
   --  Byte order for scalars stored in this compound type

   ----------------------------
   -- Record types accessors --
   ----------------------------

   function Components
     (Self : Type_Representation) return Component_Representation_Array
   with Pre => Kind (Self) = Record_Type;
   --  List of components for this record, excluding components from the
   --  variant part (if any).

   function Discriminants
     (Self : Type_Representation) return Component_Representation_Array
   with Pre => Kind (Self) = Record_Type;
   --  Subset of components for this record that are discriminants

   function Has_Variant_Part (Self : Type_Representation) return Boolean
   with Pre => Kind (Self) = Record_Type;
   --  Whether this record type has a variant part

   function Variants
     (Self : Type_Representation) return Variant_Representation_Array
   with Pre => Kind (Self) = Record_Type and then Has_Variant_Part (Self);
   --  Assuming this record type has a variant part, return all variants for it

   function Bit_Order (Self : Type_Representation) return System.Bit_Order
   with Pre => Kind (Self) = Record_Type;
   --  Order for bit numbering in this record type

   -----------------------
   -- Variant accessors --
   -----------------------

   function Present
     (Self : Variant_Representation) return Numerical_Expression
   with Pre => not Is_Null (Self);
   --  Expression that evaluates to non-zero when the compoments of the variant
   --  are contained in the record type, and to zero when they are not.

   function Components
     (Self : Variant_Representation) return Component_Representation_Array
   with Pre => not Is_Null (Self);
   --  Components for this variant, excluding components from the sub-variant
   --  (if any).

   function Has_Subvariant_Part (Self : Variant_Representation) return Boolean
   with Pre => not Is_Null (Self);
   --  Whether this variant has a sub-variant part

   function Subvariants
     (Self : Variant_Representation) return Variant_Representation_Array
   with Pre => Has_Subvariant_Part (Self);
   --  Assuming this variant has a sub-variant part, return all sub-variants
   --  for it.

   -------------------------
   -- Component accessors --
   -------------------------

   function Declaration
     (Self : Component_Representation) return Defining_Name
   with Pre => not Is_Null (Self);
   --  Declaration for this component. This returns ``No_Defining_Name`` for
   --  artificial components, i.e. components not defined in the source, but
   --  introduced by the compiler (for instance the tag for tagged types).

   function Component_Name (Self : Component_Representation) return Text_Type
   with Pre => not Is_Null (Self);
   --  Name for this component. The casing of the result is not specified. This
   --  is useful as a complement to the ``Declaration`` function above for
   --  artificial components, which have no declaration.

   function Discriminant_Number
     (Self : Component_Representation) return Natural
   with Pre => not Is_Null (Self);
   --  0 if this component is *not* a discriminant. Positive number that
   --  uniquely identifies this discriminant in the record type otherwise.

   function Position
     (Self : Component_Representation) return Numerical_Expression
   with Pre => not Is_Null (Self);
   --  Expression that evaluates to the index of first byte in the record that
   --  is used to represent this component.

   function First_Bit (Self : Component_Representation) return Natural
   with Pre => not Is_Null (Self);
   --  First bit in the byte at ``Position`` that is used to represent this
   --  component.

   function Size (Self : Component_Representation) return Numerical_Expression
   with Pre => not Is_Null (Self);
   --  Size (in bits) for this component

   ---------------------------
   -- Array types accessors --
   ---------------------------

   function Component_Size
     (Self : Type_Representation) return Numerical_Expression
   with Pre => Kind (Self) = Array_Type;
   --  Expression that evaluates to the size (in bits) for each array
   --  component.

   ---------------------------------
   -- Fixed point types accessors --
   ---------------------------------

   function Small (Self : Type_Representation) return GMP_RN.Rational
   with Pre => Kind (Self) in Fixed_Type;
   --  Return the "small" number for a fixed point type. This is a positive
   --  real number, and all values for this fixed point type are multiples of
   --  this "small" number.

   function Range_First (Self : Type_Representation) return GMP_RN.Rational
   with Pre => Kind (Self) in Fixed_Type;
   --  Lower bound for the mandatory range of a fixed point type

   function Range_Last (Self : Type_Representation) return GMP_RN.Rational
   with Pre => Kind (Self) in Fixed_Type;
   --  Upper bound for the mandatory range of a fixed point type

   -------------------------------------
   -- Numerical expression evaluation --
   -------------------------------------

   function Discriminant_Count (Self : Numerical_Expression) return Natural
   with Pre => not Is_Null (Self);
   --  Return the number of discriminants needed to evaluate this expression

   type Discriminant_Values is
     array (Positive range <>) of GMP_Int.Big_Integer;
   --  Actual values for all the discriminants in a record type.
   --
   --  For discriminants that are integers, the value must be the corresponding
   --  number. For discriminants that are enumerations, the value must be the
   --  code used to represent the enumeration literal (i.e. the equivalent of
   --  GNAT's ``Enum_Rep`` attribute). The ``Discriminant_Value`` function
   --  below can be used to turn a discriminant value into the expected integer
   --  value.

   No_Discriminant_Value : constant Discriminant_Values := (1 .. 0 => <>);

   Invalid_Discriminant : exception;
   --  See the ``Discriminant_Value`` function

   function Discriminant_Value
     (Result : Eval_Result) return GMP_Int.Big_Integer;
   --  Return the discriminant value corresponding to the given evaluated
   --  static expression. Raise an ``Invalid_Discriminant`` exception if
   --  ``Result`` is not an enum literal nor an integer.

   function Evaluate
     (Self          : Numerical_Expression;
      Discriminants : Discriminant_Values) return GMP_Int.Big_Integer
   with Pre => Discriminants'First = 1
               and then Discriminants'Last >= Discriminant_Count (Self);
   --  Evaluate a numerical expression given specific values for all required
   --  discriminants.
   --
   --  Note that more discriminants than required are accepted: the evaluation
   --  of the attribute of a component for a record with 2 discriminants may
   --  need only the first discriminant that record has

   ----------------------------------
   -- Record components resolution --
   ----------------------------------

   type Size_Type is mod 2 ** 64;
   --  Number of bits or bytes to denote the size of an object in memory.
   --
   --  Note that the range for this type may change in the future, for instance
   --  if Ada ever gets ported to a 128-bit architecture.

   type Resolved_Component is record
      Declaration : Defining_Name;
      --  Declaration for this component

      Artificial_Name : Unbounded_Text_Type;
      --  If ``Declaration`` is null (i.e. if this describes an artificial
      --  component), name for that component. The casing is not specified.

      Position : Size_Type;
      --  See the corresponding ``Component_Representation`` primitive

      First_Bit : Natural;
      --  See the corresponding ``Component_Representation`` primitive

      Size : Size_Type;
      --  See the corresponding ``Component_Representation`` primitive
   end record;
   --  Description of a record component once the record discriminants are
   --  resolved.

   type Resolved_Component_Array is
     array (Positive range <>) of Resolved_Component;

   type Resolved_Record_Type (Component_Count : Natural) is record
      Alignment : Positive;
      --  See the corresponding ``Type_Representation`` primitive

      Object_Size : Size_Type;
      --  See the corresponding ``Type_Representation`` primitive

      Value_Size : Size_Type;
      --  See the corresponding ``Type_Representation`` primitive

      Bit_Order : System.Bit_Order;
      --  See the corresponding ``Type_Representation`` primitive

      Scalar_Storage_Order : System.Bit_Order;
      --  See the corresponding ``Type_Representation`` primitive

      Components : Resolved_Component_Array (1 ..  Component_Count);
      --  See the corresponding ``Type_Representation`` primitive
   end record;

   Resolution_Error : exception;
   --  See the ``Resolved_Record`` function

   function Resolved_Record
     (Self          : Type_Representation;
      Discriminants : Discriminant_Values) return Resolved_Record_Type
   with Pre => Kind (Self) = Record_Type;
   --  Resolve all components in record type ``Self`` according to the values
   --  of its discriminants.
   --
   --  Raise a ``Resolution_Error`` when resolution for the given discriminants
   --  yields nonsensical sizes or positions (usually because the discriminants
   --  are invalid).

   -------------------------------------------
   -- Representation information collection --
   -------------------------------------------

   type Repinfo_Collection is tagged private;
   --  Collection of compiler-generated representation information

   function Lookup
     (Self : Repinfo_Collection;
      Decl : Base_Type_Decl'Class) return Type_Representation;
   --  Look for the type representation corresponding to the given type
   --  declaration. Return ``No_Type_Representation`` if nothing in ``Self``
   --  matches ``Type_Name``, and raise a ``Type_Mismatch_Error`` if an
   --  inconsistency is found between ``Decl`` and the type representation
   --  found for it.

   Loading_Error : exception;
   --  Exception raised when an error occurs while loading a collection of
   --  representation information.

   type Filename_Array is array (Positive range <>) of Unbounded_String;

   function Load (Filenames : Filename_Array) return Repinfo_Collection;
   --  Load type representation information from all the given ``Filenames``
   --  and return the corresponding collection.
   --
   --  All files are supposed to be generated running GNAT on compilation units
   --  with the ``-gnatR4js`` switch.
   --
   --  Raise a ``Loading_Error`` exception if unsuccessful.

   Default_JSON_Filename_Regexp : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile (".*\.(ad.|a|spc|bdy)\.json");
   --  Default matcher for JSON files created by the -gnatR4js compiler switch.
   --  It matches the JSON files created for Ada sources using the most usual
   --  file extensions: ``.ads``, ``.adb``, ``.ada``, ``.spc``, ``.bdy``, etc.

   function Load_From_Directories
     (Name_Pattern : GNAT.Regexp.Regexp := Default_JSON_Filename_Regexp;
      Directories  : Filename_Array) return Repinfo_Collection;
   --  Like ``Load``, but using automatically loading all files in any of the
   --  given ``Directories`` whose file name matches ``Name_Pattern``.

   function Load_From_Directory
     (Name_Pattern : GNAT.Regexp.Regexp := Default_JSON_Filename_Regexp;
      Directory    : String) return Repinfo_Collection;
   --  Like ``Load``, but using automatically loading all files whose name
   --  matches ``Name_Pattern`` in the given ``Directory`.

   Gprbuild_Error : exception;
   --  See ``Load_From_Project``

   function Load_From_Project
     (Tree    : GPR2.Project.Tree.Object;
      View    : GPR2.Project.View.Object := GPR2.Project.View.Undefined;
      Subdirs : String := "repinfo";
      Force   : Boolean := False) return Repinfo_Collection;
   --  Run GPRbuild on the given project ``Tree`` to compile all of its Ada
   --  units with the ``-gnatR4js`` compiler switch, then load all generated
   --  JSON files for units under the ``View`` sub-project.
   --
   --  Unless left to empty strings, the following formals are passed to
   --  ``gprbuild``'s command line:
   --
   --  * ``Subdirs`` is passed as ``--subdirs``.
   --
   --  If ``Force`` is ``True``, pass ``-f`` to gprbuild to force the build of
   --  compilation units.
   --
   --  Raise a ``Gprbuild_Error`` exception if ``gprbuild`` exits with a
   --  non-zero status code. Raise a ``Loading_Error`` exception if the loading
   --  of JSON files fails.

private

   use Langkit_Support.Symbols;
   use Langkit_Support.Bump_Ptr;

   Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("LIBADALANG.DDA", GNATCOLL.Traces.From_Config);

   --  The allocation model is centered around ``Repinfo_Collection``: that
   --  type is a shared pointer to the internal ``Repinfo_Collection_Data``
   --  record.
   --
   --  ``Repinfo_Collection_Data`` owns all related dynamically allocated
   --  records (all being internal): it is only when the last reference to a
   --  ``Repinfo_Collection_Data`` record disappears that all related dynamic
   --  allocations are free'd.
   --
   --  All these internal dynamically allocated records are presented to users
   --  as a kind of "fat pointer", containing both the pointer to these
   --  records, and a copy of the owning ``Repinfo_Collection``.
   --
   --  This design allows users not to worry about resources handling: as long
   --  as they get rid of their public values (``Repinfo_Collection``,
   --  ``Type_Representation``, ...), all the memory will be reclaimed, and it
   --  is not possible for them to get a dangling pointer. For the
   --  implementation, this means that all allocations are pooled in
   --  ``Repinfo_Collection_Data``, and no manual refcounting is necessary
   --  beyond what GNATCOLL.Refcount does for collections.

   type Integer_Access is access all GMP_Int.Big_Integer;
   type Rational_Access is access all GMP_RN.Rational;

   procedure Free is new Ada.Unchecked_Deallocation
     (GMP_Int.Big_Integer, Integer_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (GMP_RN.Rational, Rational_Access);

   package Integer_Vectors is new Langkit_Support.Vectors (Integer_Access);
   package Rational_Vectors is new Langkit_Support.Vectors (Rational_Access);

   type Numerical_Expression_Data;
   type Numerical_Expression_Access is access all Numerical_Expression_Data;

   type Type_Representation_Data;
   type Type_Representation_Access is access all Type_Representation_Data;

   type Component_Representation_Data;
   type Component_Representation_Access is
     access all Component_Representation_Data;

   type Variant_Representation_Data;
   type Variant_Representation_Access is
     access all Variant_Representation_Data;

   type Component_Representation_Access_Array is
     array (Positive range <>) of Component_Representation_Access;
   type Component_Representation_Access_Array_Access is
     access all Component_Representation_Access_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Component_Representation_Access_Array,
      Component_Representation_Access_Array_Access);
   package Component_Repinfo_Array_Vectors is new Langkit_Support.Vectors
     (Component_Representation_Access_Array_Access);

   type Variant_Representation_Access_Array is
     array (Positive range <>) of Variant_Representation_Access;
   type Variant_Representation_Access_Array_Access is
     access all Variant_Representation_Access_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Variant_Representation_Access_Array,
      Variant_Representation_Access_Array_Access);
   package Variant_Repinfo_Array_Vectors is new Langkit_Support.Vectors
     (Variant_Representation_Access_Array_Access);

   ------------------
   -- Data records --
   ------------------

   --  An expression is a tree of ``Expr_Node_Data``. Each node represents an
   --  operation, identified by its opcode, which determines the number of
   --  operands for that operation.

   type Opcode is (

      --  3 operands

      Cond_Expr,
      --  Condition (operand 1), then-expression (operand 2), else-expression
      --  (operand 3).

      --  2 operands

      Plus_Expr,
      Minus_Expr,
      Mult_Expr,
      Trunc_Div_Expr,
      Ceil_Div_Expr,
      Floor_Div_Expr,
      Trunc_Mod_Expr,
      Ceil_Mod_Expr,
      Floor_Mod_Expr,
      Exact_Div_Expr,
      Min_Expr,
      Max_Expr,
      Truth_And_Expr,
      Truth_Or_Expr,
      Truth_Xor_Expr,
      Lt_Expr,
      Le_Expr,
      Gt_Expr,
      Ge_Expr,
      Eq_Expr,
      Ne_Expr,
      Bit_And_Expr,

      --  1 operand

      Negate_Expr,
      Abs_Expr,
      Truth_Not_Expr,

      --  Misc

      Discrim_Val,
      Literal);
   --  Refer to "repinfo.ads" in GNAT for more information about opcodes

   subtype Opcode_3 is Opcode range Cond_Expr .. Cond_Expr;
   --  Opcodes which have 3 operands

   subtype Opcode_2 is Opcode range Opcode_3'First .. Bit_And_Expr;
   --  Opcodes which have at least 2 operands

   subtype Opcode_1 is Opcode range Opcode_3'First .. Truth_Not_Expr;
   --  Opcodes which have at least 1 operand

   type Expr_Node_Data;
   type Expr_Node_Access is access all Expr_Node_Data;
   type Expr_Node_Data (Code : Opcode := Opcode'First) is record
      case Code is
         when Opcode_1 =>
            Op_1 : Expr_Node_Access;

            case Code is
               when Opcode_2 =>
                  Op_2 : Expr_Node_Access;

                  case Code is
                     when Opcode_3 =>
                        Op_3 : Expr_Node_Access;
                     when others =>
                        null;
                  end case;

               when others =>
                  null;
            end case;

         when Discrim_Val =>
            Discriminant_Number : Positive;

         when Literal =>
            Value : Integer_Access;
      end case;
   end record;

   procedure Dump_Expr (Node : Expr_Node_Access);
   --  Debug helper: print a human-readable representation of the expression
   --  that ``Node`` represents.

   type Numerical_Expression_Data is record
      Discriminant_Count : Natural;
      --  Number of discriminant values required to evaluate this expression

      Root : Expr_Node_Access;
      --  Root expression node to evaluate
   end record;

   type Internal_Type_Kind is
     (Record_Type, Array_Type, Internal_Fixed_Type, Other_Type);
   subtype Internal_Composite_Type is
     Internal_Type_Kind range Record_Type ..  Array_Type;

   --  For all components in the record types below, please refer to the
   --  corresponding primitive functions for the documentation of their
   --  semantics.

   type Type_Representation_Data (Kind : Internal_Type_Kind) is record
      Alignment   : Positive;
      Object_Size : Numerical_Expression_Access;
      Value_Size  : Numerical_Expression_Access;

      case Kind is
         when Internal_Composite_Type =>
            Scalar_Storage_Order : System.Bit_Order;

            case Kind is
               when Record_Type =>
                  Discriminant_Count : Natural;
                  --  Number of components that are discriminants

                  Components : Component_Representation_Access_Array_Access;
                  Variants   : Variant_Representation_Access_Array_Access;
                  Bit_Order  : System.Bit_Order;

               when Array_Type =>
                  Component_Size : Numerical_Expression_Access;

               when others => null;
            end case;

         when Internal_Fixed_Type =>
            Small                   : Rational_Access;
            Range_First, Range_Last : Rational_Access;

         when others =>
            null;
      end case;
   end record;

   type Variant_Representation_Data is record
      Present     : Numerical_Expression_Access;
      Components  : Component_Representation_Access_Array_Access;
      Subvariants : Variant_Representation_Access_Array_Access;
   end record;

   type Component_Representation_Data is record
      Name : Symbol_Type;
      --  Name for this component

      Discriminant_Number : Natural;
      Position            : Numerical_Expression_Access;
      First_Bit           : Natural;
      Size                : Numerical_Expression_Access;
   end record;

   package Type_Representation_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Type_Representation_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Repinfo_Collection_Data is record
      Pool : Bump_Ptr_Pool;
      --  Pool in which all the plain records defined in this unit are
      --  allocated.

      Integers  : Integer_Vectors.Vector;
      Rationals : Rational_Vectors.Vector;
      --  List of all big integers/rationals allocated for this collection, to
      --  be free'd when the collection is finalized.

      Component_Repinfo_Arrays : Component_Repinfo_Array_Vectors.Vector;
      Variant_Repinfo_Arrays   : Variant_Repinfo_Array_Vectors.Vector;
      --  Likwise, for arrays of component/variant representation information

      Symbols : Symbol_Table;
      --  Symbol table, used to internalize all type names and component names

      Type_Representations : Type_Representation_Maps.Map;
      --  Mapping from lower case fully qualified type names to the
      --  corresponding type representations.
   end record;

   ---------------------
   -- Reference types --
   ---------------------

   type Numerical_Expression is record
      Collection : Repinfo_Collection;
      Data       : Numerical_Expression_Access;
   end record;

   type Type_Representation is record
      Collection : Repinfo_Collection;
      Data       : Type_Representation_Access;

      Declaration : Base_Type_Decl;
      --  Declaration for this type

      Kind : Type_Kind;
      --  Kind for this type, according to ``Declaration``

      --  Because of record extensions, the variant may belong to a base type.
      --  To avoid redundant computations in ``Has_Variant_Part`` and
      --  ``Variants``, pre-fetch variant part info from both the parse tree
      --  and its representation information when creating a
      --  ``Type_Representation`` object.

      Variant_Part_Node : Variant_Part;
      Variants          : Variant_Representation_Access_Array_Access;
   end record;

   type Variant_Representation is record
      Collection : Repinfo_Collection;
      Data       : Variant_Representation_Access;

      Declaration : Variant;
      --  Syntactic construct for this variant
   end record;

   type Component_Representation is record
      Collection : Repinfo_Collection;
      Data       : Component_Representation_Access;

      Declaration : Defining_Name;
      --  Declaration for this component
   end record;

   type Repinfo_Collection_Access is access all Repinfo_Collection_Data;

   procedure Release (Self : in out Repinfo_Collection_Data);

   package Collection_Refs is new GNATCOLL.Refcount.Shared_Pointers
     (Repinfo_Collection_Data, Release);
   type Repinfo_Collection is new Collection_Refs.Ref with null record;

   --------------------------
   -- Constant definitions --
   --------------------------

   No_Repinfo_Collection : constant Repinfo_Collection :=
     (Collection_Refs.Ref with null record);

   No_Numerical_Expression : constant Numerical_Expression :=
     (No_Repinfo_Collection, null);

   No_Type_Representation : constant Type_Representation :=
     (No_Repinfo_Collection,
      null,
      No_Base_Type_Decl,
      Type_Kind'First,
      No_Variant_Part,
      null);

   No_Variant_Representation : constant Variant_Representation :=
     (No_Repinfo_Collection, null, No_Variant);

   No_Component_Representation : constant Component_Representation :=
     (No_Repinfo_Collection, null, No_Defining_Name);

end Libadalang.Data_Decomposition;
