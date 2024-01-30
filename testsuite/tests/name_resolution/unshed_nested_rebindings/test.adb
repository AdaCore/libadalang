procedure Test is
   generic
   package Data_G is
      generic
      package Buffer_G is
         procedure Test;
      end Buffer_G;
   end Data_G;

   package body Data_G is
      package body Buffer_G is
         X : Integer := 0;

         procedure Test is
         begin
            Buffer_G.X := Buffer_G.X + 1;
         end Test;
      end Buffer_G;
   end Data_G;

   generic
      with package Data is new Data_G (<>);
   package Table_G is
      procedure Test;
   end Table_G;

   package body Table_G is
      package Buffer is new Data.Buffer_G;

      procedure Test is
      begin
         Buffer.Test;
      end Test;
   end Table_G;

   generic
      with package Table is new Table_G (<>);
   package Action_G is
   end Action_G;

   package My_Data is new Data_G;
   package My_Table is new Table_G (My_Data);
   package My_Action is new Action_G (My_Table);
   pragma Test_Statement;
begin
   null;
end Test;
