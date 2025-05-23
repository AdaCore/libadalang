with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Unit_Files; use Libadalang.Unit_Files;

procedure Main is
   procedure Check (Name : Text_Type);

   -----------
   -- Check --
   -----------

   procedure Check (Name : Text_Type) is
   begin
      Put ("#");
      for C of Name loop
         if C in ' ' .. '~' then
            Put (" '" & Character'Val (Character_Type'Pos (C)) & "'");
         else
            declare
               Img : constant String := Character_Type'Pos (C)'Image;
            begin
               Put (" [" & Img (Img'First + 1 .. Img'Last) & "]");
            end;
         end if;
      end loop;
      if Name = "" then
         Put (" <empty>");
      end if;
      New_Line;

      Put ("->");
      declare
         Str_Name : constant String := Unit_String_Name (Name);
      begin
         for C of Str_Name loop
            if C in ' ' .. '~' then
               Put (" '" & C & "'");
            else
               declare
                  Img : constant String := Character'Pos (C)'Image;
               begin
                  Put (" [" & Img (Img'First + 1 .. Img'Last) & "]");
               end;
            end if;
         end loop;
         if Str_Name = "" then
            Put (" <empty>");
         end if;
      end;
      New_Line;
      New_Line;
   end Check;
begin
   Check ("");
   Check ("foo");
   Check ("h" & (1 => Character_Type'Val (233)) & "llo");
   Put_Line ("Done.");
end Main;
