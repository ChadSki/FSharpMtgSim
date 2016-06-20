module Mana

open Cards

// Represents a quantity of mana
type ManaAmount =
    { red:int; green:int; colorless:int }

    static member (+) (left:ManaAmount, right:ManaAmount) =
        { red       = left.red       + right.red
          green     = left.green     + right.green
          colorless = left.colorless + right.colorless }

    static member (-) (left:ManaAmount, right:ManaAmount) =
        { red       = left.red       - right.red
          green     = left.green     - right.green
          colorless = left.colorless - right.colorless }

let noMana = { red=0; green=0; colorless=0 }

let Cost = function
| BurningWish ->           { red=1; green=0; colorless=1 }
| ChancellorOfTheTangle -> { red=0; green=3; colorless=4 }
| ChromeMox ->             noMana
| DesperateRitual ->       { red=1; green=0; colorless=1 }
| ElvishSpiritGuide ->     { red=0; green=1; colorless=2 }
| EmptyTheWarrens ->       { red=1; green=0; colorless=3 }
| GitaxianProbe ->         { red=0; green=0; colorless=99 } // Not to be played normally
| GoblinCharbelcher ->     { red=0; green=0; colorless=4 }
| LandGrant ->             { red=0; green=1; colorless=1 }
| LionsEyeDiamond ->       noMana
| LotusPetal ->            noMana
| Manamorphose ->          { red=0; green=0; colorless=2 }  // R/G mana makes this effectively colorless
| PyreticRitual ->         { red=1; green=0; colorless=1 }
| RiteOfFlame ->           { red=1; green=0; colorless=0 }
| SeethingSong ->          { red=1; green=0; colorless=2 }
| SerumPowder ->           { red=0; green=0; colorless=3 }
| SimianSpiritGuide ->     { red=1; green=0; colorless=2 }
| StreetWraith ->          { red=0; green=0; colorless=99 } // Not to be played normally
| Taiga ->                 noMana
| TinderWall ->            { red=0; green=1; colorless=0 }

type ManaColor =
     | Red
     | Green
     | Colorless
     | Multicolor

let Color = function
| Manamorphose -> Multicolor  // Special-cased because we counted the R/G mana as effectively colorless
| card -> let manaCost = Cost card
          match manaCost.red > 0 with
          | true -> match manaCost.green > 0 with
                    | true -> Multicolor
                    | false -> Red

          | false -> match manaCost.green > 0 with
                     | true -> Green
                     | false -> Colorless
