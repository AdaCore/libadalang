procedure Test_It is
   type T is range 1 .. 100;
   type NT is new Test_It.T'Base;
begin
   for J in NT loop
      null;
   end loop;
end Test_It;
