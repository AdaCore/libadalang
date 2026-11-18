procedure Example is
   Message : constant String := "First part " & "second part " & "third part " & "fourth part " & "fifth part " & "last part.";
   Nested : constant String := "Prefix: " & ("First nested part " & "second nested part " & "third nested part " & "fourth nested part.") & " Suffix.";
   Long_Operand : constant String := "This individual string literal is deliberately longer than the configured maximum line width." & " short" & " suffix";
   Values : constant Vector := A + B & C - D & E + (F + G) & A + B & C - D & E + (F + G) & A + B & C - D;
   Sum : constant Integer := First_Value + Second_Value + Third_Value + Fourth_Value + Fifth_Value;
begin
   Put_Line ("Short " & "message.");
   Put_Line ("First part " & "second part " & "third part " & "fourth part " & "fifth part " & "last part.");
   Put_Line ("First part " & "second part " -- Keep this trailing comment.
             & "third part " & "fourth part " & "fifth part " & "last part.");
   Put_Line ("First part " & "second part "
             -- Keep this comment before an operator.
             & "third part " & "fourth part.");
end Example;
