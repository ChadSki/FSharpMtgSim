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
    | BurningWish ->           "BWi"
    | ChancellorOfTheTangle -> "CoT"
    | ChromeMox ->             "ChM"
    | DesperateRitual ->       "DRt"
    | ElvishSpiritGuide ->     "ESG"
    | EmptyTheWarrens ->       "EtW"
    | GitaxianProbe ->         "GPr"
    | GoblinCharbelcher ->     "GCb"
    | LandGrant ->             "LGr"
    | LionsEyeDiamond ->       "LED"
    | LotusPetal ->            "LoP"
    | Manamorphose ->          "MMp"
    | PyreticRitual ->         "PyR"
    | RiteOfFlame ->           "RoF"
    | SeethingSong ->          "SSo"
    | SerumPowder ->           "SrP"
    | SimianSpiritGuide ->     "SSG"
    | StreetWraith ->          "SWr"
    | Taiga ->                 "Tai"
    | TinderWall ->            "TiW"

let IsWinCondition = function
    | BurningWish | EmptyTheWarrens | GoblinCharbelcher
      -> true
    | ChancellorOfTheTangle | ChromeMox         | DesperateRitual | ElvishSpiritGuide
    | GitaxianProbe         | LandGrant         | LionsEyeDiamond | LotusPetal
    | Manamorphose          | PyreticRitual     | RiteOfFlame     | SeethingSong
    | SerumPowder           | SimianSpiritGuide | StreetWraith    | Taiga
    | TinderWall
      -> false
