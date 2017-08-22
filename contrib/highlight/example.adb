with Ada.Text_IO; use Ada.Text_IO;
use all type Ada.Text_IO.File_Type;

procedure Example is

   subtype Nat is Integer range 0 .. Integer'Last;

   type Rec (N : Natural) is tagged record
      S : String (1 .. N);
   end record;

   type Money_Type is delta 0.01 digits 14;

   generic
      with procedure Put_Line (S : String);
   package Things is
      procedure Process (S : access Wide_String)
        with Pre => S /= null and then S'Length > 0
                    and then (for all I in S.all'Range =>
                              Wide_Character'Pos (S.all (I)) = 0);
   end Things;

   package body Things is

      -------------
      -- Process --
      -------------

      procedure Process (S : access Wide_String) is
      begin
         Print_Loop : for C of S.all loop
            Print_Block : declare
               C_Pos : constant Integer := Wide_Character'Pos (C);
            begin
               if C_Pos < Character'Pos (Character'Last) then
                  declare
                     CS : constant String := (1 => Character'Val (C_Pos));
                  begin
                     Put_Line (CS);
                  end;
               end if;
            end Print_Block;
         end loop Print_Loop;
      end Process;

   end Things;

   package Text_IO_Things is new Things (Put_Line);

   A          : Integer := 0;
   ["03C0"]   : Standard.Integer := 0;
   S          : aliased Wide_String := "Hello, ["03C0"] world!";
   Hex_Digits : array (0 .. 15) of Character := "0123456789abcdef";
begin
   Text_IO_Things.Process (S'Access);
end;
