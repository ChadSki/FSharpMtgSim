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
