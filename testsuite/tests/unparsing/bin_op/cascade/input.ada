function "<" (Left, Right : Completion_Id) return Boolean is
begin
   return Left.Id_Length < Right.Id_Length
     or else
       (Left.Id_Length = Right.Id_Length
        and then
          (Left.Line < Right.Line
           or else
            (Left.Line = Right.Line
             and then
               (Left.Column < Right.Column
                or else
                  (Left.Column = Right.Column
                   and then
                   (Left.Id < Right.Id
                    or else
                      (Left.Id = Right.Id
                       and then
                         (Left.File < Right.File
                          or else
                          (Left.File = Right.File
                           and then
                           Left.Resolver_Id < Right.Resolver_Id)))))))));
end "<";
