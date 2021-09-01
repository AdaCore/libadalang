package X is

   type Read_Bytes_Proc is access procedure (Bytes : String);

   procedure Load(Read : Read_Bytes_Proc);
end X;
