package Index is

    type T is limited private;
    type Entry_Id_Mod is mod 2;
    subtype Entry_Id_Sub is Integer range 1 .. 4;

private

    protected type T is
        entry E (Entry_Id_Mod) with
          Pre => E'Index < 2;
        pragma Postcondition (E'Index >= 0);
        entry F (Entry_Id_Sub) with
          Post => F'Index >= 0;
        pragma Precondition (F'Index < 2);
    end T;
    pragma Test_Block;

end Index;
