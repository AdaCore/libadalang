procedure Foo is
begin
   begin
      begin
         begin
            begin
               begin
                  begin
                     begin
                        declare
                           Pos : Node_Maps.Cursor;
                           Ins : Boolean;
                        begin
                           Access_To_Incomplete_Views.Insert
                             (Retysp (Des_Ty), E, Pos, Ins);

                           pragma
                             Assert
                               (Is_Access_Type (Node_Maps.Element (Pos))
                                and then
                                  Ekind
                                    (Directly_Designated_Type
                                       (Node_Maps.Element (Pos)))
                                  /= E_Subprogram_Type
                                and then
                                  (Acts_As_Incomplete_Type
                                     (Directly_Designated_Type
                                        (Node_Maps.Element (Pos)))
                                   or else
                                     (Ekind (Node_Maps.Element (Pos))
                                      = E_Access_Subtype
                                      and then
                                        Acts_As_Incomplete_Type
                                          (Directly_Designated_Type
                                             (Base_Retysp
                                                (Node_Maps.Element (Pos)))))));
                        end;
                     end;
                  end;
               end;
            end;
         end;
      end;
   end;
end Foo;
