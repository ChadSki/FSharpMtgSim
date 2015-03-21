module Simulation

open System
open Cards
open Mancala

do
    let myMetaDeck = function
        | BurningWish | EmptyTheWarrens | GoblinCharbelcher | LionsEyeDiamond | LotusPetal
          -> { min=4; max=4 }  // Core cards are always maxed out

        | ChancellorOfTheTangle | ChromeMox    | DesperateRitual | ElvishSpiritGuide
        | GitaxianProbe         | LandGrant    | Manamorphose    | PyreticRitual
        | RiteOfFlame           | SeethingSong | SerumPowder     | SimianSpiritGuide
        | StreetWraith          | Taiga        | TinderWall
          -> { min=0; max=3 }  // Non-core

    let combos = DeckCombinations myMetaDeck 60
    let total = combos |> Seq.map (fun combo -> printfn "%s" (PrettyPrintDeck combo))
                       |> Seq.length

    printfn "Total = %d" total
    printfn "Press any key to exit."
    System.Console.ReadKey() |> ignore
