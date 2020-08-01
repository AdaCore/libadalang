private with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

private with String_Utils;

package Xrefs is

   ----------------
   -- File Table --
   ----------------

   type File_Index_Type is new Positive;
   --  Index that designates a source file in File_Table_Type

   type File_Table_Type is private;
   --  Sorted set of source files (base names only)

   function File_Index
     (Files    : in out File_Table_Type;
      Filename : String) return File_Index_Type;
   --  Look for the unique index for Filename in Files. Create one if it does
   --  not exist yet.

   function Filename
     (Files : File_Table_Type; File_Index : File_Index_Type) return String;
   --  Return the filename corresponding to File_Index

   function Comes_First
     (Files : in out File_Table_Type; L, R : File_Index_Type) return Boolean;
   --  Given the filename sorting order, return whether the file designated by
   --  L comes before the one designated by R.

   --------------------
   -- Xrefs database --
   --------------------

   type Xref_Type is record
      Ref_Sloc    : Source_Location;
      Ref_File    : File_Index_Type;
      --  Location for the "referencing" name

      Entity_Sloc : Source_Location;
      Entity_File : File_Index_Type;
      --  Location for the "referenced" name ("designated entity", "referenced
      --  declaration", ...). Valid iff Error is True.

      Error       : Boolean;
      --  For xrefs from LAL, specify if an error (Property_Error) occurred
      --  during name resolution.
   end record;

   No_Xref : constant Xref_Type :=
               (No_Source_Location, 1, No_Source_Location, 1, False);

   package Xref_Vectors is new Ada.Containers.Vectors (Positive, Xref_Type);

   type Unit_Xrefs_Type is record
      Unit  : File_Index_Type;
      --  File that contain all the references described in Xrefs

      Xrefs : Xref_Vectors.Vector;
   end record;

   type Unit_Xrefs_Access is access all Unit_Xrefs_Type;
   procedure Free is new Ada.Unchecked_Deallocation
     (Unit_Xrefs_Type, Unit_Xrefs_Access);

   package Unit_Xrefs_Vectors is new Ada.Containers.Vectors
     (Positive, Unit_Xrefs_Access);

   procedure Sort
     (Files : in out File_Table_Type; Xrefs : in out Xref_Vectors.Vector);

   procedure Remove_Duplicates (Xrefs : in out Xref_Vectors.Vector);

   procedure Sort
     (Files : in out File_Table_Type;
      Xrefs : in out Unit_Xrefs_Vectors.Vector);

   procedure Read_LI_Xrefs
     (LI_Filename : String;
      Files       : in out File_Table_Type;
      Xrefs       : out Unit_Xrefs_Vectors.Vector);

   procedure Put (Files : File_Table_Type; X : Xref_Type);
   procedure Put (Files : File_Table_Type; Xrefs : Unit_Xrefs_Vectors.Vector);

private

   use String_Utils;

   package Filename_Vectors is new Ada.Containers.Vectors
     (File_Index_Type, Unbounded_String);

   package Filename_Maps is new Ada.Containers.Hashed_Maps
     (Unbounded_String,
      File_Index_Type,
      Ada.Strings.Unbounded.Hash,
      "=");

   type Sort_Index_Type is new Positive;
   package Sort_Index_Vectors is new Ada.Containers.Vectors
     (File_Index_Type, Sort_Index_Type);

   type File_Table_Type is record
      Table        : Filename_Vectors.Vector;
      --  List of files, in order of registration (see File_Index)

      Map          : Filename_Maps.Map;
      --  Mapping: file name -> file index for all registered files

      Sort_Indexes : Sort_Index_Vectors.Vector;
      --  Mapping: file index -> index in what would be the sorted Table
      --  vector (not materialized).
   end record;

   function Filename
     (Files : File_Table_Type; File_Index : File_Index_Type) return String
   is (+Files.Table (File_Index));

end Xrefs;
