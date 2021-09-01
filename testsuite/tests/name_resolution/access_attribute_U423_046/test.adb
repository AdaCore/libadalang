with X; use X;

package body Test is

   procedure Read_Bytes_Proc(Bytes : String) is null;

   procedure Dummy is
   begin
      Load(Read_Bytes_Proc'Access);
      pragma Test_Statement;
   end Dummy;

end Test;
