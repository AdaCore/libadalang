--
--  Copyright (C) 2014-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.IO_Exceptions;

with GNATCOLL.Mmap;

with Langkit_Support.Errors; use Langkit_Support.Errors;

package body Libadalang.PP_Lexer is

   subtype Symbol_Set is Character
   with Static_Predicate =>
     Symbol_Set in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_';

   subtype Chars_Sequence_Set is Character
   with Static_Predicate => Chars_Sequence_Set in Symbol_Set | '.';

   ----------
   -- Read --
   ----------

   function Read (Filename : String) return String_Access is
      use GNATCOLL.Mmap;

      File   : Mapped_File;
      Region : Mapped_Region;
      Buffer : Str_Access;
   begin
      begin
         File := Open_Read (Filename);
      exception
         when Exc : Ada.IO_Exceptions.Name_Error =>
            raise File_Read_Error with Exception_Message (Exc);
      end;
      Region := Read (File);
      Buffer := Data (Region);

      return Result : constant String_Access := new String (1 .. Last (Region))
      do
         Result.all := Buffer.all (Result.all'Range);
         Free (Region);
         Close (File);
      end return;
   end Read;

   -------------------------
   -- Is_Valid_Identifier --
   -------------------------

   function Is_Valid_Identifier (Text : String) return Boolean is
      Last_Was_Underscore : Boolean := False;
   begin
      if Text = "" then
         return False;
      elsif Text (Text'First) = '_'
         or else Text (Text'Last) = '_'
         or else Text (Text'First) in '0' .. '9'
      then
         return False;
      end if;

      for I in Text'Range loop
         if Text (I) = '_' then
            if Last_Was_Underscore then
               return False;
            else
               Last_Was_Underscore := True;
            end if;

         elsif Text (I) not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' then
            return False;

         else
            Last_Was_Underscore := False;
         end if;
      end loop;

      return True;
   end Is_Valid_Identifier;

   ------------------
   -- Create_Lexer --
   ------------------

   function Create_Lexer (Diagnostic_Filename, Filename : String) return Lexer
   is
   begin
      return Result : Lexer do
         Result.Filename := US.To_Unbounded_String (Diagnostic_Filename);
         Result.Buffer := Read (Filename);
         Result.Next_Char := Result.Buffer.all'First;
         Result.Next_Sloc := (1, 1);
      end return;
   end Create_Lexer;

   ----------
   -- Next --
   ----------

   procedure Next (Self : in out Lexer; Token : out Token_Type) is

      type Peeked_Char is record
         EOF   : Boolean;
         Value : Character;
      end record;

      function Peek return Peeked_Char;

      procedure Next_Char;

      procedure Scan_String_Literal;

      procedure Error (Message : String) with No_Return;

      PC : Peeked_Char;

      ----------
      -- Peek --
      ----------

      function Peek return Peeked_Char is
      begin
         if Self.Next_Char > Self.Buffer.all'Last then
            return (EOF => True, Value => ASCII.NUL);
         else
            return (EOF => False, Value => Self.Buffer.all (Self.Next_Char));
         end if;
      end Peek;

      ---------------
      -- Next_Char --
      ---------------

      procedure Next_Char is
      begin
         pragma Assert (Self.Next_Char <= Self.Buffer.all'Last);
         Token.Last := Self.Next_Char;
         if Self.Buffer.all (Self.Next_Char) = ASCII.LF then
            Self.Next_Sloc.Line := Self.Next_Sloc.Line + 1;
            Self.Next_Sloc.Column := 1;
         else
            Self.Next_Sloc.Column := Self.Next_Sloc.Column + 1;
         end if;
         Self.Next_Char := Self.Next_Char + 1;
      end Next_Char;

      -------------------------
      -- Scan_String_Literal --
      -------------------------

      procedure Scan_String_Literal is
      begin
         loop
            PC := Peek;
            Next_Char;
            if PC.EOF or else PC.Value = ASCII.LF then
               Error ("unterminated string literal");
            elsif PC.Value = '"' then
               PC := Peek;
               if not PC.EOF and then PC.Value = '"' then
                  Next_Char;
                  PC := Peek;
               else
                  return;
               end if;
            end if;
         end loop;
      end Scan_String_Literal;

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
      begin
         raise Syntax_Error with
            US.To_String (Self.Filename)
            & ":" & Image (Self.Next_Sloc)
            & ": " & Message;
      end Error;
   begin
      loop
         Token.First := Self.Next_Char;
         Token.Sloc := Self.Next_Sloc;

         PC := Peek;
         if PC.EOF then
            Token.Kind := EOF;
            return;
         end if;
         Next_Char;

         case PC.Value is
            when ASCII.LF =>
               Token.Kind := EOL;
               return;

            when ' ' | ASCII.CR =>
               null;

            when '-' =>
               PC := Peek;
               if PC.EOF then
                  Error ("stray dash");

               elsif PC.Value = '-' then
                  while not PC.EOF and then PC.Value /= ASCII.LF loop
                     Next_Char;
                     PC := Peek;
                  end loop;

               --  Past this point, we can only be parsing a gnatprep switch

               elsif PC.Value not in 'a' .. 'z' | 'A' .. 'Z' then
                  Error ("stray dash");
               else
                  Token.Kind := Switch;

                  --  First take all alphanumeric characters possible (switch
                  --  name plus potentially a symbol name).

                  loop
                     PC := Peek;
                     if not PC.EOF and then PC.Value = '=' then
                        Next_Char;
                        exit;

                     elsif PC.EOF or else PC.Value not in Symbol_Set then
                        return;
                     end if;

                     Next_Char;
                  end loop;

                  --  We just got a "=" after the switch name: we expect either
                  --  nothing, the equivalent of a chars sequence, or a string
                  --  literal.

                  PC := Peek;
                  if PC.EOF or else PC.Value = ASCII.LF then
                     null;

                  elsif PC.Value in Chars_Sequence_Set then
                     while not PC.EOF and then PC.Value in Chars_Sequence_Set
                     loop
                        Next_Char;
                        PC := Peek;
                     end loop;

                  elsif PC.Value = '"' then
                     Next_Char;
                     Scan_String_Literal;
                  end if;

                  return;
               end if;

            when ':' =>
               PC := Peek;
               if PC.EOF then
                  Error ("stray colon");

               elsif PC.Value = '=' then
                  Token.Kind := Assign;
                  Next_Char;
                  return;

               else
                  Error ("invalid token");
               end if;

            when '*' =>
               Token.Kind := Star;
               return;

            when '"' =>
               Token.Kind := String_Literal;
               Scan_String_Literal;
               return;

            when Chars_Sequence_Set =>
               Token.Kind := Chars_Sequence;
               loop
                  PC := Peek;
                  if PC.EOF or else PC.Value not in Chars_Sequence_Set then
                     return;
                  end if;
                  Next_Char;
               end loop;

            when others =>
               Error ("invalid token");
         end case;
      end loop;
   end Next;

   -------------------------
   -- Is_Valid_Identifier --
   -------------------------

   function Is_Valid_Identifier
     (Self : Lexer; Token : Token_Type) return Boolean is
   begin
      return Is_Valid_Identifier (Self.Buffer.all (Token.First .. Token.Last));
   end Is_Valid_Identifier;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Self : Lexer; Token : Token_Type) return US.Unbounded_String is
   begin
      return US.To_Unbounded_String
        (Self.Buffer.all (Token.First .. Token.Last));
   end Get_Text;

   ------------------------
   -- Get_Text_Lowercase --
   ------------------------

   function Get_Text_Lowercase
     (Self : Lexer; Token : Token_Type) return US.Unbounded_String is
   begin
      return US.To_Unbounded_String
        (To_Lower (Self.Buffer.all (Token.First .. Token.Last)));
   end Get_Text_Lowercase;

   --------------------
   -- Denoted_String --
   --------------------

   function Denoted_String
     (Self : Lexer; Token : Token_Type) return US.Unbounded_String
   is
      Text   : String renames Self.Buffer.all (Token.First .. Token.Last);
      Result : US.Unbounded_String;
      I      : Positive := Text'First + 1;
   begin
      pragma Assert (Token.Kind = String_Literal);
      pragma Assert (Text'Length >= 2);
      pragma Assert (Text (Text'First) = '"' and then Text (Text'Last) = '"');
      while I < Text'Last loop
         US.Append (Result, Text (I));
         I := I + (if Text (I) = '"' then 2 else 1);
      end loop;
      return Result;
   end Denoted_String;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Lexer) is
   begin
      Free (Self.Buffer);
   end Finalize;

end Libadalang.PP_Lexer;
