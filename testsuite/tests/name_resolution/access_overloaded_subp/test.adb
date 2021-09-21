procedure Test is
   type T is access function (Bytes : String) return String;

   procedure Load (Read : T) is
   begin
      null;
   end Load;

   function Read_Bytes_Proc return String is ("1");

   function Read_Bytes_Proc (Bytes : String) return String is ("2");
begin
   Load (Read_Bytes_Proc'Access);
   pragma Test_Statement;
end Test;

