module Simulation

open System
open Microsoft.FSharp.Collections
open Cards
open Mancala

// individual deck score
let simScore deck =
    printfn "%s" (PrettyPrintDeck deck)
    0.0

// keeps track of best-performing deck
let exploreDecks metaDeck =
    let combos = DeckCombinations metaDeck 60
    let best = ref 0.0
    for deck in combos |> Seq.take(120) do
        let score = simScore deck
        if score > !best then best := score

do
    let myMetaDeck = function
        | BurningWish | EmptyTheWarrens | GoblinCharbelcher | LionsEyeDiamond | LotusPetal
          -> { min=4; max=4 }  // Core cards are always maxed out

        | ChancellorOfTheTangle | ChromeMox    | DesperateRitual | ElvishSpiritGuide
        | GitaxianProbe         | LandGrant    | Manamorphose    | PyreticRitual
        | RiteOfFlame           | SeethingSong | SerumPowder     | SimianSpiritGuide
        | StreetWraith          | Taiga        | TinderWall
          -> { min=0; max=4 }  // Non-core

    exploreDecks myMetaDeck
    ()
