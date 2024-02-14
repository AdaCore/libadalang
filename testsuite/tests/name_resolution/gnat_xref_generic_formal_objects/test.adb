procedure Test is
   generic
      type T is (<>);
      A, B, C : Integer;
      with function F return Integer;
      with procedure G;
   package P is end P;

   generic
      type T is (<>);
      A, B, C : Integer;
      with function F return Integer;
      with procedure G;
   procedure Proc;
   --  GNAT doesn't add references to generic subprogram formal objects as it
   --  does for generic packages. If GNAT supports them one day, gnat_compare
   --  already supports it.

   procedure Proc is
   begin
      null;
   end Proc;
begin
   null;
end;
