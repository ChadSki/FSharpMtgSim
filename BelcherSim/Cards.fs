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

let CardLabel = function
| BurningWish ->           "Bw"
| ChancellorOfTheTangle -> "Ct"
| ChromeMox ->             "Cm"
| DesperateRitual ->       "Dr"
| ElvishSpiritGuide ->     "Eg"
| EmptyTheWarrens ->       "Ew"
| GitaxianProbe ->         "Gp"
| GoblinCharbelcher ->     "Gc"
| LandGrant ->             "Lg"
| LionsEyeDiamond ->       "Le"
| LotusPetal ->            "Lp"
| Manamorphose ->          "Mm"
| PyreticRitual ->         "Pr"
| RiteOfFlame ->           "Rf"
| SeethingSong ->          "Ss"
| SerumPowder ->           "Sp"
| SimianSpiritGuide ->     "Sg"
| StreetWraith ->          "Sw"
| Taiga ->                 "Ta"
| TinderWall ->            "Tw"
