Preprocessing
#############

Libadalang's integrated preprocessor works thanks to the file reader mechanism:
you create a file reader that knows which files to preprocess and with which
parameters, and you pass that file reader when creating the analysis context.

Input source
============

``prep-data.txt``

   .. code-block:: text

      "os.ads" -Dos=windows

``os.ads``

   .. code-block:: ada
      :linenos:

      package OS is
         # if OS = "linux" then
            Name : constant String := "gnu-linux";
         # elsif OS = "windows" then
            Name : constant String := "microsoft-windows";
         # else
            Name : constant String := "unknown";
         # end if;
      end OS;

``print_os.adb``

   .. code-block:: ada

      with Ada.Text_IO; use Ada.Text_IO;

      with OS;

      procedure Print_OS is
      begin
         Put_Line (OS.Name);
      end Print_OS;


Sample code
===========

Python
------

.. code-block:: python

   import libadalang as lal


   # Let the file reader find preprocessor data files in the
   # current directory.
   path = ["."]

   # Create the preprocessor file reader from the
   # "prep-data.txt" preprocessor file. Force the "blank lines"
   # mode to preserve line numbers in the preprocessed code.
   fr = lal.FileReader.create_preprocessor_from_file(
       filename="prep-data.txt",
       path=path,
       line_mode=lal.FileReader.LineMode.blank_lines,
   )

   # Create a context using this preprocessor configuration and
   # start using Libadalang's analysis the usual way...
   ctx = lal.AnalysisContext(file_reader=fr)
   u = ctx.get_from_file("print_os.adb")

   for node in u.root.findall(lambda n: n.text == "Name"):
       # Fetch the declaration that this "Name" identifier
       # references.
       decl = node.p_referenced_decl()
       print(node)
       print(f"  references {decl}")
       print()

       # Show the text of the unit that contain that
       # declaration, with line numbers.
       print("Unit text:")
       for i, line in enumerate(
           decl.unit.text.split("\n"), 1
       ):
           print(f"{str(i).rjust(2)} | {line}")


Ada
---

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;
   with GNATCOLL.Strings; use GNATCOLL.Strings;

   with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
   with Langkit_Support.Text;         use Langkit_Support.Text;

   with Libadalang.Analysis;      use Libadalang.Analysis;
   with Libadalang.Common;        use Libadalang.Common;
   with Libadalang.Preprocessing; use Libadalang.Preprocessing;

   procedure Main is

      Path : constant Any_Path := Create_Path
        (Directories => (1 .. 0 => <>), CWD => If_Empty);
      --  Let the file reader find preprocessor data files in
      --  the current directory.

      FR  : constant File_Reader_Reference :=
        Create_Preprocessor_From_File
          ("prep-data.txt", Path, Blank_Lines);
      --  Create the preprocessor file reader from the
      --  "prep-data.txt" preprocessor file. Force the "blank
      --  lines" mode to preserve line numbers in the
      --  preprocessed code.

      Ctx : constant Analysis_Context :=
        Create_Context (File_Reader => FR);
      U   : constant Analysis_Unit :=
        Ctx.Get_From_File ("print_os.adb");
      --  Create a context using this preprocessor
      --  configuration and start using Libadalang's analysis
      --  the usual way...

      -----------
      -- Visit --
      -----------

      function Visit (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Text = "Name" then

            --  Fetch the declaration that this "Name"
            --  identifier references.

            declare
               Ref : constant Basic_Decl :=
                 Node.As_Identifier.P_Referenced_Decl;
            begin
               Put_Line (Node.Image);
               Put_Line ("  references " & Ref.Image);
               New_Line;

               --  Show the text of the unit that contain that
               --  declaration, with line numbers.

               Put_Line ("Unit text:");
               declare
                  Text   : constant XString :=
                    To_XString (To_UTF8 (Ref.Unit.Text));
                  Lineno : Positive := 1;
               begin
                  for Line of Text.Split (ASCII.LF) loop
                     declare
                        Lineno_Img : constant String :=
                          " " & Lineno'Image;
                        Prefix     : String renames Lineno_Img
                          (Lineno_Img'Last - 2
                           .. Lineno_Img'Last);
                     begin
                        Put (Prefix & " | ");
                        Put_Line (To_String (Line));
                        Lineno := Lineno + 1;
                     end;
                  end loop;
               end;
            end;
         end if;
         return Into;
      end Visit;

   begin
      U.Root.Traverse (Visit'Access);
   end Main;

Output
------

.. code-block:: text

   <Id "Name" print_os.adb:7:17-7:21>
     references <ObjectDecl ["Name"] os.ads:6:7-6:53>

   Unit text:
    1 | package OS is
    2 |    Bits : constant := 32;
    3 |
    4 |
    5 |
    6 |       Name : constant String := "microsoft-windows";
    7 |
    8 |
    9 |
   10 | end OS;
   11 |
