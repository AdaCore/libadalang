procedure Test is
   package P is
      type Ctrl is record
         Id : Natural := 0;
      end record
         with Finalizable => (Initialize           => Initialize,
                              Adjust               => Adjust,
                              Finalize             => Finalize,
                              Relaxed_Finalization => True);
      pragma Test_Block;

      procedure Initialize (Obj : in out Ctrl);
      procedure Adjust     (Obj : in out Ctrl);
      procedure Finalize   (Obj : in out Ctrl);
   end P;
begin
   null;
end Test;
