package Tag is
   type File_Type is tagged null record;

   type Derived is new File_Type with record
      A : Integer;
   end record with Stable_Properties'Class => (Is_Open, Mode);
   pragma Test_Block;

   type Mode_Type is (In_File, Out_File, Append_File);

   function Is_Open (File : in Derived) return Boolean;
   function Mode (File : in Derived) return Mode_Type
      with Stable_Properties'Class => (not Is_Open);
   pragma Test_Block;
end Tag;
