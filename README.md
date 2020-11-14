# Mars Rover Kata

## Instructions

2- 3 hours
kiss
use the sample data
do the hard things first
commit frequently

## Taking it further

It might be better to have separate types for the inputs and for the domain. This would allow Location to have a Mars property, which makes logical sense, and would probably mean less parameters to pass around. It would also stop an invalid combination of Mars and Location being used together.

The Scent concept is only weakly represented in the code, and could be strengthened, probably with a Scent type.

`mapAccuml` is a relatively complicated thing, and relies on a Tuple, as opposed to a domain type. It might be better to pattern match on the list instead and create a domain type.

The names of the things in code should be the same as the domain (the instructions). I have tried to do this, but a check would probably reveal some things.

There is only one implementation module and one test module, but ideally these would be split up in to smaller, more cohesive modules.

A bit of a tidy up, especially in the tests, would probably be good. Some of the tests are quite long and have a lot of confusing brackets.

There is a minor primitive obsession on Int, in the Location and Mars types. This is hard to remove, but if these were in their own modules, opaque types could be used to make sure the obession didn't leak out of the module. It would also allow easy addition of other world types (such as non rectangular worlds). A world with a different number of dimensions would be difficult to encapsulate though.