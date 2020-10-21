procedure A is
    type Color is (Red, Green, Blue, Yellow, Black, White);
    subtype Primary is Color range Red .. Blue;

    type Arr is array (Color range <>) of Integer;

    procedure Print (TheArr : Arr) is
    begin
       null;
    end Print;

    TheArr : Arr (Color);
begin
    Print (TheArr (Color range Red .. Blue));
    pragma Test_Statement;
    Print (TheArr (Primary));
    pragma Test_Statement;
end A;
