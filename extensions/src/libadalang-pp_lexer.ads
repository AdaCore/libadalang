--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  Helper for ``Libadalang.Preprocessing``: this package implements a lexer
--  that allows to analyze both definition files and preprocessor data files
--  (see GNATprep's documentation).

private with Ada.Finalization;
with Ada.Strings.Unbounded;

with GNAT.Strings; use GNAT.Strings;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

private package Libadalang.PP_Lexer is

   package US renames Ada.Strings.Unbounded;

   function Read (Filename : String) return String_Access;
   --  Read the whole file at ``Filename`` and return its content as a
   --  dynamically allocated string.
   --
   --  If for some reason the file cannot be read, raise a
   --  ``Libadalang.Preprocessing.File_Read_Error`` exception.

   function Is_Valid_Identifier (Text : String) return Boolean;
   --  Return whether ``Text`` is a valid preprocessing symbol identifier

   type Lexer is limited private;
   --  Lexer for the content of a given file: lexing (i.e. token search) is
   --  done on demand, but reading the input file is done at lexer creation,
   --  and kept in a single buffer.

   function Create_Lexer (Diagnostic_Filename, Filename : String) return Lexer;
   --  Read the file at ``Filename`` and return a lexer to search for tokens in
   --  it.
   --
   --  If for some reason the file cannot be read, raise a
   --  ``Libadalang.Preprocessing.File_Read_Error`` exception.
   --
   --  Use ``Diagnostic_Filename`` as the filename to use in the error
   --  messages. This is useful to report about the exact filename passed from
   --  users instead of the possibly expanded and absolute ``Filename``,
   --  cluttering error messages.

   type Token_Kind is
     (Assign, String_Literal, Chars_Sequence, Star, Switch, EOL, EOF);
   --  Kind of token:
   --
   --  ``Assign`` is ``:=``.
   --
   --  ``String_Literal`` is a string literal.
   --
   --  ``Chars_Sequence`` is a sequence of characters, which can be either a
   --  preprocessing symbol, or any symbol value that is not a string literal.
   --
   --  ``Star`` is ``*``.
   --
   --  ``Switch`` is a GNATprep switch: ``-c``, ``-u``, ``-DFoo="bar"``, etc.
   --
   --  ``EOL`` is a line feed character.
   --
   --  ``EOF`` is a pseudo token (no associated text) for the end of the file.

   type Token_Type is record
      Kind : Token_Kind;
      --  Kind for this token

      First : Positive;
      Last  : Natural;
      --  Bounds for the slice in the source buffer for this token

      Sloc : Source_Location;
      --  Source location for the first character in this token. For ``EOF``,
      --  this is the location of the next character that would be appended at
      --  the end of the file.
   end record;

   procedure Next (Self : in out Lexer; Token : out Token_Type);
   --  Get the next token in the ``Self`` lexer. Yield ``EOF`` if there is no
   --  token anymore in the stream.

   function Is_Valid_Identifier
     (Self : Lexer; Token : Token_Type) return Boolean;
   --  Return whether ``Token`` designates a valid preprocessing symbol
   --  identifier.

   function Get_Text
     (Self : Lexer; Token : Token_Type) return US.Unbounded_String;
   --  Return the source slice that corresponds to this token

   function Get_Text_Lowercase
     (Self : Lexer; Token : Token_Type) return US.Unbounded_String;
   --  Like ``Get_Text``, but convert to lower case first

   function Denoted_String
     (Self : Lexer; Token : Token_Type) return US.Unbounded_String;
   --  Assuming that ``Token`` is a string literal, return the string value it
   --  denotes.

private

   type Lexer is new Ada.Finalization.Limited_Controlled with record
      Filename : US.Unbounded_String;
      --  Filename used to get the sources to scan. Kept here for error
      --  messages.

      Buffer : String_Access;
      --  Entire content of the source to scan

      Next_Char : Positive;
      --  Index in ``Buffer`` of the first character to consider for the next
      --  token.

      Next_Sloc : Source_Location;
      --  Sloc for the next character to consider for the next token
   end record;

   overriding procedure Finalize (Self : in out Lexer);

end Libadalang.PP_Lexer;
