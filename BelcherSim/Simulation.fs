module Simulation

open System
open Microsoft.FSharp.Collections
open Mancala
open Cards
open Deck
open Library

// individual deck score
let SimScore deck =
    printfn "%s" (PrettyPrintDeck deck)
    0.0

// keeps track of best-performing deck
let ExploreDecks metaDeck =
    let combos = DeckCombinations metaDeck 60
    let best = ref 0.0
    for deck in combos |> Seq.take(120) do
        let score = SimScore deck
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

    //ExploreDecks myMetaDeck

    let combos = DeckCombinations myMetaDeck 60
    let firstCombo = combos |> Seq.take 1
                            |> Seq.exactlyOne
                            |> AsLibrary
    printfn "%s" (PrettyPrintLibrary firstCombo)
    let foo = Shuffle firstCombo
    printfn "%s" (PrettyPrintLibrary foo)
    printfn "%s" (PrettyPrintLibrary ((Shuffle foo) |> Seq.take 15
                                                    |> Seq.toList))
    ()
