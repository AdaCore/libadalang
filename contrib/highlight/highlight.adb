with Ada.Command_Line;
with Ada.Text_IO;

with Langkit_Support.Diagnostics;
with Libadalang.Analysis;

with Colors;
with Highlighter;
with HTML;

--  Take an Ada source file as command-line argument and output on the standard
--  output an HTML document that syntax highlights the source code.

procedure Highlight is

   package LAL renames Libadalang.Analysis;

   function Get_Source_File return String;
   --  Get the source file to process from the command line. If the arguments
   --  are invalid, print an usage message and return an empty string.

   procedure Output_HTML (Unit : LAL.Analysis_Unit);
   --  Write to the standard output an HTML document that syntax highlights the
   --  source code for Unit.

   ---------------------
   -- Get_Source_File --
   ---------------------

   function Get_Source_File return String is
      use Ada.Command_Line, Ada.Text_IO;
   begin
      if Argument_Count /= 1 then
         Put_Line (Command_Name & ": invalid arguments");
         New_Line;
         Put_Line ("Usage: " & Command_Name & " [source-file]");
         Set_Exit_Status (Failure);
         return "";
      end if;
      return Argument (1);
   end Get_Source_File;

   -----------------
   -- Output_HTML --
   -----------------

   procedure Output_HTML (Unit : LAL.Analysis_Unit) is
      use Ada.Text_IO;

      procedure Put_CSS_Rules is new HTML.Put_CSS_Rules (Put);
      procedure Put_Tokens is new HTML.Put_Tokens (Put);

      Highlights : Highlighter.Highlights_Holder
        (Highlighter.Token_Index (LAL.Token_Count (Unit)),
         Highlighter.Token_Index (LAL.Trivia_Count (Unit)));
   begin
      Put_Line
        ("<html><head>");
      Put_Line
        ("<meta http-equiv=""Content-Type"""
         & " content=""charset=utf-8"" />");
      Put_Line
        ("<title>" & HTML.Escape (LAL.Get_Filename (Unit)) & "</title>");

      --  Write CSS rules for each highlighting style

      Put_Line ("<style type=""text/css"">");
      Put_Line ("body { background-color: #"
                & HTML.Color_To_HTML (Colors.Default_Style.Bg_Color)
                & "; }");
      Put_CSS_Rules (Colors.Default_Style.Style);
      Put_Line ("</style></head><body>");

      --  Then write the highlighted source code itself

      Highlighter.Highlight (Unit, Highlights);
      Put_Tokens (Unit, Highlights, "utf-8");

      Put_Line ("</body></html>");
   end Output_HTML;

   Ctx      : LAL.Analysis_Context := LAL.Create;
   Src_File : constant String := Get_Source_File;
begin
   if Src_File'Length > 0 then
      declare
         Unit : constant LAL.Analysis_Unit :=
           LAL.Get_From_File (Ctx, Src_File, With_Trivia => True);
      begin
         --  If there are any error, just print them on the standard error
         --  stream and abort. Otherwise, do our job.

         if LAL.Has_Diagnostics (Unit) then
            for D of LAL.Diagnostics (Unit) loop
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Langkit_Support.Diagnostics.To_Pretty_String (D));
            end loop;
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

         else
            Output_HTML (Unit);
         end if;
      end;

   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
   LAL.Destroy (Ctx);
end Highlight;
