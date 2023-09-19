procedure Test is

   type Enume is (S, P, A, R, K);

   task type TT is
      entry F (1 .. 3) (I : Integer; J : out Integer);
      entry G (Enume) (I : Integer; J : out Integer);
   end TT;

   type Parent is access TT;
   type T is new Parent;

   X : T;
   I : Integer := 0;
   J : Integer := 0;

   task body TT is
      N : Integer := 1;
   begin
      loop
         select
            accept F (2) (I : Integer; J : out Integer) do
               j := i + n;
            end F;
         or
            terminate;
         end select;
      end loop;
   end TT;
begin
     X := new TT;

     X.F (2) (I, J);
     pragma Test_Statement;

     X.G (R) (I, J);
     pragma Test_Statement;
end Test;
