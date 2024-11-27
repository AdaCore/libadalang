procedure Test is

   package P is
      type Ctrl_1 is record
         Id : Natural := 0;
      end record
         with Finalizable => (Initialize => Initialize,
                              Adjust     => Adjust,
                              Finalize   => Finalize);

      type Ctrl_2 is record
         Id : Natural := 0;
      end record
         with Finalizable => (Initialize           => Initialize,
                              Adjust               => Adjust,
                              Finalize             => Finalize,
                              Relaxed_Finalization => True);

      type Ctrl_3 is record
         Id : Natural := 0;
      end record
         with Finalizable => (Initialize           => Initialize,
                              Adjust               => Adjust,
                              Finalize             => Finalize,
                              Relaxed_Finalization => True);

      procedure Adjust     (Obj : in out Ctrl_1);
      procedure Finalize   (Obj : in out Ctrl_1);
      procedure Initialize (Obj : in out Ctrl_1);

      procedure Adjust     (Obj : in out Ctrl_2);
      procedure Finalize   (Obj : in out Ctrl_2);
      procedure Initialize (Obj : in out Ctrl_2);

      procedure Adjust     (Obj : in out Ctrl_3);
      procedure Finalize   (Obj : in out Ctrl_3);
      procedure Initialize (Obj : in out Ctrl_3);

      procedure R1 with No_Raise;
      procedure R2;
      procedure R3;
      procedure R4;
      procedure R5 with No_Raise => False;
      pragma No_Raise (R2, R3);
   end P;
   --% subps = node.findall(lal.SubpDecl)
   --% [sp.p_get_aspect("No_Raise") for sp in subps]

   --% [sp.p_defining_name.p_find_all_references([sp.unit]) for sp in subps]

   package body P is
      -- Pragma No_Raise applies to R4 and all its prior overloadings

      -- TODO: not working yet, see #1495
      pragma No_Raise (R4);

      procedure Adjust     (Obj : in out Ctrl_1) is null;
      procedure Finalize   (Obj : in out Ctrl_1) is null;
      procedure Initialize (Obj : in out Ctrl_1) is null;

      procedure Adjust     (Obj : in out Ctrl_2) is null;
      procedure Finalize   (Obj : in out Ctrl_2) is null;
      procedure Initialize (Obj : in out Ctrl_2) is null;

      procedure Adjust     (Obj : in out Ctrl_3) is null;
      procedure Finalize   (Obj : in out Ctrl_3) is null;
      procedure Initialize (Obj : in out Ctrl_3) is null;
      -- Pragma No_Raise applies to Finalize and all its prior overloadings
      pragma No_Raise (Finalize);

      procedure R1 is null;
      procedure R2 is null;
      procedure R3 is null;
      procedure R4 is null;
      procedure R5 is null;
   end P;

begin
   null;
end;
