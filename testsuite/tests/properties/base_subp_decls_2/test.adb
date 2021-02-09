with Ada.Text_IO; use Ada.Text_IO;

package body  Test is
   overriding procedure Finalize (X : in out A) is
   begin
      Put_Line ("Finalize");
   end Finalize;
end Test;
