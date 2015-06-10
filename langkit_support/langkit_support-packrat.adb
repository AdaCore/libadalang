package body Langkit_Support.Packrat is

   -----------
   -- Clear --
   -----------

   procedure Clear (Memo : in out Memo_Type) is
   begin
      for E of Memo loop
         if E.State = Success then
            Dec_Ref (E.Instance);
         end if;
         E.State := No_Result;
      end loop;
   end Clear;

   ---------
   -- Get --
   ---------

   function Get (Memo : Memo_Type; Offset : Integer) return Memo_Entry is
      E : Memo_Entry renames Memo (Offset mod Memo_Size);
   begin
      if E.Offset = Offset then
         return E;
      else
         return (State => No_Result, others => <>);
      end if;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (Memo              : in out Memo_Type;
                  Is_Success        : Boolean;
                  Instance          : T;
                  Offset, Final_Pos : Integer)
   is
      E : Memo_Entry renames Memo (Offset mod Memo_Size);
   begin
      if E.State = Success then
         Dec_Ref (E.Instance);
      end if;

      E := (State     => (if Is_Success then Success else Failure),
            Instance  => Instance,
            Offset    => Offset,
            Final_Pos => Final_Pos);
   end Set;

end Langkit_Support.Packrat;
