package Generic_Stack is
   procedure Pop;
private
   pragma Inline (Pop);  --  line 4
   pragma Test_Statement;
end Generic_Stack;
