pragma SPARK_Mode (On);

package body A is
   procedure P is
   begin
      null;
   end P;
   -- Off because of the annotation on the spec

   procedure Q is
      procedure QQ is
         procedure QQQ is null;  -- Off because QQ is Off
      begin
         null;
      end QQ;
      -- Off because Q is Off
   begin
      null;
   end Q;
   -- Off because of the annotation on the spec

   procedure R is
      procedure RR is
         procedure RRR is null;  -- Off because RR is Off
      begin
         null;
      end RR;
      -- Off because R is Off
   begin
      null;
   end R;
   -- Off because of the annotation on the spec

   procedure S is
      procedure SS with Annotate => (GNATprove, Skip_Flow_And_Proof) is
         procedure SSS is null;  -- Off because SS is Off
      begin
         null;
      end SS;
      -- Off because of the Annotate
   begin
      null;
   end S;
   -- On because of the configuration pragma

   procedure T is
      procedure TT is
         procedure TTT with Annotate => (GNATprove, Skip_Flow_And_Proof) is
            procedure TTTT is null;  -- Off because TTT is Off
         begin
            null;
         end TTT;
         -- Off because of the Annotate
      begin
         null;
      end TT;
      -- On because T is On
   begin
      null;
   end T;
   -- On because of the configuration pragma

   procedure U is
      procedure UU is
         procedure UUU;
         pragma Annotate (GNATprove, Skip_Flow_And_Proof, UUU);
         procedure UUU is
            procedure UUUU is null;  -- Off because UUU is Off
         begin
            null;
         end UUU;
         -- Off because of the Annotate
      begin
         null;
      end UU;
      -- On because U is On
   begin
      null;
   end U;
   -- On because of the configuration pragma
end A;
