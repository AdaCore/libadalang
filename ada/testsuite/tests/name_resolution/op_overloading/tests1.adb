procedure Tests1 is
   type File_Type is null record;
   type File_Kind is (In_File, Out_File, In_Out_File);

   procedure Create
     (Self : out File_Type; Kind : File_Kind; Name : String) is null;

   function Is_Open (Self : File_Type) return Boolean is (False);

   procedure Put_Line (Self : File_Type; Line : String) is null;

   procedure Close (Self : File_Type);

   type File_Type_Access is access all File_Type;

   f: aliased File_Type;
   g, h: File_Type_Access;

   procedure Lala(x: File_Type_Access) is
   begin
      Put_Line(x.all, "lalala");
   end Lala;

   function "="(lhs, rhs: in File_Type_Access) return Boolean is
   begin
      return True;
   end "=";
begin

   Create(f, Out_File, "test.txt");

   g := f'Access;
   Lala(g);
   if Is_Open(f) then
      Put_Line(f, "hello world");
      Close(f);
   end if;

   if h = g then
      null;
   end if;

   if "=" (h, g) then
      null;
   end if;
end Tests1;
pragma Test_Block;
