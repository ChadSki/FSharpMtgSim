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

// For when a card isn't meant to be cast normally
let prohibitiveCost = { red=99; green=99; colorless=99; }

let Cost = function
| BurningWish ->           { red=1; green=0; colorless=1 }
| ChancellorOfTheTangle -> prohibitiveCost
| ChromeMox ->             noMana
| DesperateRitual ->       { red=1; green=0; colorless=1 }
| ElvishSpiritGuide ->     prohibitiveCost
| EmptyTheWarrens ->       { red=1; green=0; colorless=3 }
| GitaxianProbe ->         prohibitiveCost
| GoblinCharbelcher ->     { red=0; green=0; colorless=4 }
| LandGrant ->             { red=0; green=1; colorless=1 }
| LionsEyeDiamond ->       noMana
| LotusPetal ->            noMana
| Manamorphose ->          { red=0; green=0; colorless=2 } // R/G mana makes this effectively colorless
| PyreticRitual ->         { red=1; green=0; colorless=1 }
| RiteOfFlame ->           { red=1; green=0; colorless=0 }
| SeethingSong ->          { red=1; green=0; colorless=2 }
| SerumPowder ->           prohibitiveCost
| SimianSpiritGuide ->     prohibitiveCost
| StreetWraith ->          prohibitiveCost
| Taiga ->                 noMana
| TinderWall ->            { red=0; green=1; colorless=0 }

type ManaColor =
     | Red
     | Green
     | Colorless
     | Multicolor

let Color card =
    let manaCost = Cost card
    match manaCost.red > 0 with
    | true -> match manaCost.green > 0 with
              | true -> Multicolor
              | false -> Red

    | false -> match manaCost.green > 0 with
                | true -> Green
                | false -> Colorless
