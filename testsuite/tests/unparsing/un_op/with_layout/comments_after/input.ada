if not Sync_Folders
         (From            => Sync_Entry.Primary_Queue,
          To              => Sync_Entry.Ending_Corpus,
          Filter          => Filter_Folder_Type'(Has_Filter => False),
          Conversion      => Sync_Entry.From_Primary_Queue_Conversion,
          To_Final_Folder => True,
          Final_Index     => Singleton.Ending_Corpus)

  --  Now sync from the crashing test cases folder to the final
  --  crashing test cases folder corpus

  or else not Sync_Folders
                (From            => Sync_Entry.Crashing_Test_Cases,
                 To              => Sync_Entry.Final_Crashing_Test_Cases,
                 Filter          =>
                   Filter_Folder_Type'(Has_Filter => False),
                 Conversion      =>
                   Sync_Entry.From_Primary_Queue_Conversion,
                 To_Final_Folder => True,
                 Final_Index     => Singleton.Final_Crashing_Test_Cases)

  --  Finally sync from the hanging test cases folder to the final
  --  hanging test cases folder corpus

  or else not Sync_Folders
                (From            => Sync_Entry.Hanging_Test_Cases,
                 To              => Sync_Entry.Final_Hanging_Test_Cases,
                 Filter          =>
                   Filter_Folder_Type'(Has_Filter => False),
                 Conversion      =>
                   Sync_Entry.From_Primary_Queue_Conversion,
                 To_Final_Folder => True,
                 Final_Index     => Singleton.Final_Hanging_Test_Cases)
then
   return Error_During_Fuzzing_Campaigns;
end if;
