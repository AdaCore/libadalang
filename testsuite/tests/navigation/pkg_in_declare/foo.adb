package body Foo is
   procedure Bar is

      package Pkg_No_Body is
         I : Integer;
      end Pkg_No_Body;

      package Pkg_Body is
         procedure Process;
      end Pkg_Body;

      package body Pkg_Body is
         procedure Process is
         begin
            null;
         end Process;
      end Pkg_Body;

   begin

      declare
         package Nested_Pkg is
            procedure Process;
         end Nested_Pkg;

         package body Nested_Pkg is
            procedure Process is
            begin
               null;
            end Process;
         end Nested_Pkg;
      begin
         Nested_Pkg.Process;
      end;

      Pkg_Body.Process;
   end Bar;
end Foo;
