(First in From .. Length (Source)
 and then Last in First .. Length (Source)

 --  No character between From and First satisfies the property
 --  Test on Set.

 and then Last in First .. Length (Source)

 and then (for all J in From .. First - 1
           => (Test = Inside) /= Maps.Is_In (Element (Source, J), Set))

           --  All characters between First and Last satisfy the property
           --  Test on Set.

 and then (for all J in First .. Last
           => (Test = Inside) = Maps.Is_In (Element (Source, J), Set))

           --  If Last is not Source'Last, then the character at position
           --  Last + 1 does not satify the property Test on Set.

 and then (if Last < Length (Source)
           then
             (Test = Inside)
             /= Maps.Is_In (Element (Source, Last + 1), Set)))
