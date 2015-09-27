with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.String_Hash;

--  Provide a symbol table for text (Text_Type) identifiers

package Langkit_Support.Symbols is

   type Symbol_Type is new String_Access;

   type Symbol_Table is private;
   --  The actual symbol table type to use

   No_Symbol_Table : constant Symbol_Table;
   --  Value to use as a default for unallocated symbol tables

   function Create return Symbol_Table;
   --  Allocate a new symbol table and return it

   function Find (ST : Symbol_Table; S : String) return Symbol_Type
     with Inline_Always;
   --  Look for an entry for the T text in the ST symbol table. Return an
   --  access that is guaranteed to be the same for all equal Text_Type.

   procedure Destroy (ST : in out Symbol_Table);
   --  Deallocate a symbol table and all the text returned by the corresponding
   --  calls to Find.

private

   function Hash is new GNAT.String_Hash.Hash
     (Char_Type => Character,
      Key_Type  => String,
      Hash_Type => Ada.Containers.Hash_Type);

   function Hash (T : Symbol_Type) return Ada.Containers.Hash_Type is
     (Hash (T.all));

   function Key_Equal (L, R : Symbol_Type) return Boolean is (L.all = R.all);

   package Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Symbol_Type,
      Hash                => Hash,
      Equivalent_Elements => Key_Equal,
      "="                 => "=");

   type Symbol_Table is access Sets.Set;

   No_Symbol_Table : constant Symbol_Table := null;

end Langkit_Support.Symbols;
