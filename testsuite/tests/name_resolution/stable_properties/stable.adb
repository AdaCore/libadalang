package body Stable is

   function Is_Open (File : in File_Type) return Boolean is
   begin
      if File >= 10 then
         return True;
      else
         return False;
      end if;
   end Is_Open;

   function Mode (File : in File_Type) return Mode_Type is
   begin
   if File >= 10 then
         return In_File;
      else
         return Out_File;
      end if;
   end Mode;

   function Is_Open (File : in File_Type1) return Boolean is
   begin
      if File >= 10.0 then
         return True;
      else
         return False;
      end if;
   end Is_Open;

   function Mode (File : in File_Type1) return Mode_Type is
   begin
   if File >= 10.0 then
         return In_File;
      else
         return Out_File;
      end if;
   end Mode;

   function Mode (File : Integer) return Mode_Type is
   begin
   if File >= 10 then
         return In_File;
      else
         return Out_File;
      end if;
   end Mode;

   function Is_Close (File : in File_Type1) return Boolean is
   begin
      if File >= 10.0 then
         return True;
      else
         return False;
      end if;
   end Is_Close;

   function Is_Open (File : in File_Type2) return Boolean is
   begin
      if File >= 10 then
         return True;
      else
         return False;
      end if;
   end Is_Open;

   function Mode (File : in File_Type2) return Mode_Type is
   begin
   if File >= 10 then
         return In_File;
      else
         return Out_File;
      end if;
   end Mode;

end Stable;
