module Cards

open System
open Microsoft.FSharp.Reflection
open Mancala

// The only cards we can encounter in this sim
type Card = | BurningWish       | ChancellorOfTheTangle | ChromeMox     | DesperateRitual
            | ElvishSpiritGuide | EmptyTheWarrens       | GitaxianProbe | GoblinCharbelcher
            | LandGrant         | LionsEyeDiamond       | LotusPetal    | Manamorphose
            | PyreticRitual     | RiteOfFlame           | SeethingSong  | SerumPowder
            | SimianSpiritGuide | StreetWraith          | Taiga         | TinderWall

// A list of all the above cases
let allCards = FSharpType.GetUnionCases typeof<Card>
               |> Array.map(fun caseInfo -> FSharpValue.MakeUnion(caseInfo, [||]) :?> Card)
               |> Array.toList


// Represents a quantity of mana
type ManaAmount =
    { red:int; white:int; green:int; blue:int; black:int; colorless:int }

    static member (+) (left:ManaAmount, right:ManaAmount) =
        { red       = left.red       + right.red
          white     = left.white     + right.white
          green     = left.green     + right.green
          blue      = left.blue      + right.blue
          black     = left.black     + right.black
          colorless = left.colorless + right.colorless }

    static member (-) (left:ManaAmount, right:ManaAmount) =
        { red       = left.red       - right.red
          white     = left.white     - right.white
          green     = left.green     - right.green
          blue      = left.blue      - right.blue
          black     = left.black     - right.black
          colorless = left.colorless - right.colorless }

let cost = function
| BurningWish ->           { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| ChancellorOfTheTangle -> { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| ChromeMox ->             { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| DesperateRitual ->       { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| ElvishSpiritGuide ->     { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| EmptyTheWarrens ->       { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| GitaxianProbe ->         { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| GoblinCharbelcher ->     { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| LandGrant ->             { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| LionsEyeDiamond ->       { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| LotusPetal ->            { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| Manamorphose ->          { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| PyreticRitual ->         { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| RiteOfFlame ->           { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| SeethingSong ->          { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| SerumPowder ->           { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| SimianSpiritGuide ->     { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| StreetWraith ->          { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| Taiga ->                 { red=0; white=0; green=0; blue=0; black=0; colorless=0 }
| TinderWall ->            { red=0; white=0; green=0; blue=0; black=0; colorless=0 }

let x = cost BurningWish
let y = cost ChromeMox
let z = x + y
let w = x - y
