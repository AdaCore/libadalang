procedure Test is
   subtype Node_Range is Natural range 0 .. 63;
   type Queue_Type is (Blocked, Ready, Waiting);

   type Task_State is record
      Queue     : Queue_Type;
      Suspended : Boolean;
   end record;

   type State_Table is array (Node_Range) of Task_State;

   type CPU_Context is record
      State : State_Table := [others => (Queue => Blocked,
                              Suspended => False)];
   end record;

   type Handle is record
      ID : Node_Range;
   end record;

   procedure Notify (This : in out CPU_Context; Target : Handle) is
      IDD    renames Target.ID;
      Statee renames This.State (IDD);
      pragma Test_Statement;
   begin
      if Statee.Queue /= Ready then
         null;
      end if;
      pragma Test_Statement;
   end Notify;
begin
   null;
end Test;
