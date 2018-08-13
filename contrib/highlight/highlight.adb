with Ada.Command_Line;
with Ada.Text_IO;

with GNATCOLL.VFS;
with Libadalang.Analysis;

with Colors;
with Highlighter;
with HTML;
with Term256;

--  Take an Ada source file as command-line argument and output a syntax
--  highlighted version of the source code on the standard output.
--
--  By default, or if one argument is --html, output it as an HTML document
--  (CSS included). If --term256 appears instead, output it as a sequence of
--  ANSI escape codes.

procedure Highlight is

   package LAL renames Libadalang.Analysis;

   type Output_Format_Type is (HTML_Output, Term256_Output);
   --  Format for the syntax highlighted output

   Output_Format : Output_Format_Type := HTML_Output;

   procedure Print_Usage (Error : String := "");
   --  Display command-line usage on the standard output. If Error is a
   --  non-empty string, also display it as an error message.

   function Get_Source_File return String;
   --  Get the source file to process and the requested output format from the
   --  command line. If the arguments are invalid, print an usage message and
   --  return an empty string.

   procedure Output_Highlighted (Unit : LAL.Analysis_Unit);
   --  Write the syntax highlighted source code for Unit according to
   --  Output_Format.

   function Basename (Filename : String) return String;
   --  Return the base name of the Filename path

   --------------
   -- Basename --
   --------------

   function Basename (Filename : String) return String is
      use GNATCOLL.VFS;
   begin
      return +Create (+Filename).Base_Name;
   end Basename;

   ---------------------
   -- Get_Source_File --
   ---------------------

   function Get_Source_File return String is
      Last_File : Natural := 0;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Arg : constant String := Ada.Command_Line.Argument (I);
         begin
            if Arg'Length > 2
              and then Arg (Arg'First .. Arg'First + 1) = "--"
            then
               declare
                  Opt : constant String := Arg (Arg'First + 2 .. Arg'Last);
               begin
                  if Opt = "help" then
                     Print_Usage;
                     return "";

                  elsif Opt = "html" then
                     Output_Format := HTML_Output;

                  elsif Opt = "term256" then
                     Output_Format := Term256_Output;

                  else
                     Print_Usage ("invalid output format: " & Opt);
                     return "";
                  end if;
               end;

            elsif Last_File /= 0 then
               Print_Usage ("too many output files");

            else
               Last_File := I;
            end if;
         end;
      end loop;

      if Last_File = 0 then
         Print_Usage ("input file missing");
         return "";
      end if;

      return Ada.Command_Line.Argument (Last_File);
   end Get_Source_File;

   -----------------
   -- Print_Usage --
   -----------------

   procedure Print_Usage (Error : String := "") is
      Command : constant String := Ada.Command_Line.Command_Name;
   begin
      if Error'Length > 0 then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         Ada.Text_IO.Put_Line (Command & ": " & Error);
         Ada.Text_IO.New_Line;
      end if;
      Ada.Text_IO.Put_Line
        ("Usage: " & Command & " [--html|--term256] [source-file]");
   end Print_Usage;

   ------------------------
   -- Output_Highlighted --
   ------------------------

   procedure Output_Highlighted (Unit : LAL.Analysis_Unit) is
      use Ada.Text_IO;

      function URL (Dummy_Unit : LAL.Analysis_Unit) return String is ("");

      procedure Put_CSS_Rules is new HTML.Put_CSS_Rules (Put);
      procedure Put_Tokens_HTML is new HTML.Put_Tokens (Put, URL);
      procedure Put_Tokens_Term256 is new Term256.Put_Tokens (Put);

      Highlights : Highlighter.Highlights_Holder
        (Highlighter.Token_Index (LAL.Token_Count (Unit)),
         Highlighter.Token_Index (LAL.Trivia_Count (Unit)));
   begin
      Highlighter.Highlight (Unit, Highlights);

      case Output_Format is
         when HTML_Output =>
            Put_Line
              ("<html><head>");
            Put_Line
              ("<meta http-equiv=""Content-Type"""
               & " content=""charset=utf-8"" />");
            Put_Line
              ("<title>" & HTML.Escape (Basename (LAL.Get_Filename (Unit)))
               & "</title>");

            --  Write CSS rules for each highlighting style

            Put_Line ("<style type=""text/css"">");
            Put_Line ("body { background-color: #"
                      & HTML.Color_To_HTML (Colors.Default_Style.Bg_Color)
                      & "; }");
            Put_Line ("span.line { display: block; }");
            Put_CSS_Rules (Colors.Default_Style.Style);
            Put_Line ("</style></head><body>");

            --  Then write the highlighted source code itself

            Put_Tokens_HTML (Unit, Highlights, "utf-8");

            Put_Line ("</body></html>");

         when Term256_Output =>
            Put_Tokens_Term256
              (Unit, Highlights, Colors.Default_Style.Style, "utf-8");
      end case;
   end Output_Highlighted;

   Ctx      : constant LAL.Analysis_Context := LAL.Create_Context;
   Src_File : constant String := Get_Source_File;
begin
   if Src_File'Length > 0 then
      declare
         Unit : constant LAL.Analysis_Unit :=
           LAL.Get_From_File (Ctx, Src_File);
      begin
         --  If there are any error, just print them on the standard error
         --  stream and abort. Otherwise, do our job.

         if LAL.Has_Diagnostics (Unit) then
            for D of LAL.Diagnostics (Unit) loop
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  LAL.Format_GNU_Diagnostic (Unit, D));
            end loop;
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

         else
            Output_Highlighted (Unit);
         end if;
      end;

   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Highlight;
