Implicit dereference aspect/expected subtype
============================================

Problem
-------

For the moment, the implicit dereference aspect is handled wrong in LAL: We
handle it as a special case directly in the equality property.

In parallel, there is an unsoundness in how we resolve the expected static
subtypes/real static subtypes for actuals in calls in LAL: We use the same
logic variable for both, and use the equality property of the logic bind to
make the two compatible.

The two are wrong, and for the same reasons, which is basically that it is
wrong to use the equality property to handle type/subtype coercion:

1. In the old (unsound) solver depending on the order of the logic resolution,
   a given variable might have either of the expected or real (sub)type.

2. In the new solver, those equations will just be impossible to use, so we
   must get rid of those unsoundesses before we make the switch.

High level solution for expected subtype (TODO fill out)
--------------------------------------------------------

The high level solution for the expected subtype problem is to have two
variable for call actuals, one for the real subtype, and one for the expected
subtype, and use a predicate between the two variables for the subtype
correspondence. TODO fill out when we start implementing.

High level solution for implicit dereference aspect (TODO fill out)
-------------------------------------------------------------------

In the ARM, the static semantics/name resolution rules for this aspect state
that basically as soon as a name refers to an object with this aspect, then it
can either be an implicit dereference or not. If it is, the type of this name
is the type specified by the implicit dereference aspect, else, it is the
original type.

The high level solution for the implicit dereference aspect would be to encode
that disjunction in the name resolution of the `Name` node. This does not
appear to be very difficult, appart from the necessary plumbing. TODO fill out
when we start implementing.
