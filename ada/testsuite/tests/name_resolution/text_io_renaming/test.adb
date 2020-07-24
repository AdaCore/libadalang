with Text_IO;

procedure Test is
   type U is mod 10;
   package IIO is new Text_IO.Integer_IO (U);
   pragma Test_Statement_UID;
begin
   null;
end Test;
