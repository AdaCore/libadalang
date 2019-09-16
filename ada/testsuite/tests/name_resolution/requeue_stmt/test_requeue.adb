with Ada.Text_IO        ; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure ProducerConsumer_Test is

   Size : constant Positive := 5;

   protected type Producer_Consumer is

      entry     Get_Data (Value_Get : out Natural);
      procedure Put_Data (Value_Put : in  Natural);

   private
      entry Wait_For_Get (Value_Get : out Natural);

      Data_Available : Boolean := False;
      The_Data       : Natural;

   end Producer_Consumer;

   protected body Producer_Consumer is

      entry Get_Data (Value_Get : out Natural) when true is
      begin
         if Data_Available then
            Value_Get := The_Data;
            Data_Available := False;
         else
            requeue Wait_For_Get;
            pragma Test_Statement;
         end if;
      end Get_Data;

      entry Wait_For_Get(Value_Get : out Natural) when Data_Available is
      begin
         Value_Get := The_Data;
         Data_Available := False;
      end Wait_For_Get;

      procedure Put_Data(Value_Put : in Natural) is
      begin
         The_Data := Value_Put;
      end Put_Data;

   end Producer_Consumer;

   PassValue : ProducerConsumer;

   task type Consumer is
      entry Supply_Id (Id : in Positive);
   end Consumer;

   task body Consumer is
      Consumer_Id,
      My_Value, My_Total : Natural;
      Got_Value          : Boolean := True;
   begin
      accept Supply_Id (Id : in Positive) do
         Consumer_Id := Id;
      end Supply_Id;
      Put("Hello From Consumer "); Put(Consumer_Id); New_Line;
      My_Total := 0;
      while Got_Value loop
         select
            PassValue.Get_Data(My_Value);
            My_Total := My_Total + My_Value;
         or
            -- after 2 seconds and no data we assume it is time to end!
            delay 2.0;
            Got_Value := False;
         end select;
         -- add some delay to permit task swapping
         delay 0.1;
      end loop;
      Put("Consumer:Total ");Put(Consumer_Id);Put(My_Total);New_Line;
   end Consumer;

   Consumers : array (Positive range 1..Size) of Consumer;

   N : Integer := 200;
begin
   Put("Hello From the Producer");New_Line;
   -- start consumers
   for i in Consumers'Range loop
      Consumers (i).Supply_Id (i);
   end loop;

   Put("Grand Total should be: ");Put(N*(N+1)/2);New_Line;
   for I in 1..N loop
      PassValue.Put_Data(I);
   end loop;

end ProducerConsumer_Test;
