protected type Buffer (Nb_Elements_In_Buffer : Integer := 100; Need_To_Activate_All : Boolean := False) is
   procedure Activate;
   procedure Release;
private
   Busy : Boolean := False;
end Buffer;
