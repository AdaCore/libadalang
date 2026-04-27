return
  (Ada.Finalization.Controlled
   with
     Dispatching_Create
       (Ref       => Dir.Value,
        Full_Path =>
          FS_String
            (Dir.Full_Name (Normalize).all)
          & From_Unix
              (Dir.Value.Get_FS,
               +Base_Name)));
