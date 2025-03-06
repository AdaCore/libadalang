procedure Test is
   S : constant String with External_Initialization => "data.bin";
   pragma Test_Block;
begin
   null;
end Test;
