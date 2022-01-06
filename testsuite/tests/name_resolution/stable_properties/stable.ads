package Stable is
   type File_Type is new Integer
      with Stable_Properties => (Is_Open, Mode, ExprF);
   pragma Test_Block;

   type Mode_Type is (In_File, Out_File, Append_File);

   function Is_Open (File : in File_Type) return Boolean;
   function Mode (File : in File_Type) return Mode_Type
      with Pre => Is_Open(File) or else raise Constraint_Error;
   function ExprF (File : in File_Type) return Boolean is (True);

   type File_Type1 is new Float
      with Stable_Properties => (Is_Open, Mode);
   pragma Test_Block;

   function Is_Open (File : in File_Type1) return Boolean;
   function Mode (File : in File_Type1) return Mode_Type
      with Pre => Is_Open(File) or else raise Constraint_Error;
   function Mode (File : in Integer) return Mode_Type;

   function Is_Close (File : in File_Type1) return Boolean
      with Stable_Properties => (Mode);
   pragma Test_Block;

   type File_Type2 is new Integer
      with Stable_Properties => (Is_Open, Mode);
   function Is_Open (File : in File_Type2) return Boolean;
   function Mode (File : in File_Type2) return Mode_Type
      with Stable_Properties => (not Is_Open);
   pragma Test_Block;

end Stable;
