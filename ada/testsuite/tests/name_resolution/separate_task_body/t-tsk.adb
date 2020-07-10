separate (T)
task body Tsk is
begin
   accept B do
      Ignore_String (A'Image);
      pragma Test_Statement;
   end B;
end Tsk;
