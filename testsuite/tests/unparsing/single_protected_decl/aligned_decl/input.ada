   protected X is
      entry Start (Count : Natural);
      entry Start_Selector (Service_Available : out Boolean);
   private
      Started_Transaction : Boolean := False;
      In_Service    : Boolean := False;
      Waiting   : Boolean := False;
  end X;
  
