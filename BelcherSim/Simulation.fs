﻿module Simulation

open System
open Microsoft.FSharp.Collections
open Logging
open Mancala
open Cards
open Deck
open Game

// Individual deck score.
let SimScore deck numRuns =
    let trials = Array.Parallel.init numRuns (fun _ -> AttemptWin deck)
    let wins = trials |> Seq.filter (fun x -> x)
                      |> Seq.length

    (float wins) / (float numRuns)

// Keeps track of best-performing decks.
let ExploreDecks metaDeck =
    // TODO for range in metadeck, create mapping of
    // Card -> numCards:int ->(numWins:int, numGames:int)
    // Even if we can't save the results of each individual deck, we can
    // get an idea of how good having N of a certain card is.

    let combos = DeckCombinations metaDeck 60
    let mutable bestDeck = []
    let mutable bestScore = 0.0

    for deck in combos do
        let score = SimScore deck 100
        if score > bestScore then
            bestScore <- score
            bestDeck <- deck
            printfn "New best score %f" bestScore

    if bestDeck = []
    then log "All decks sucked."
    else log (sprintf "The best deck is %swith a score of %f" (PrettyPrintDeck bestDeck) bestScore)

do
    let metaDeck = function
        | BurningWish | GoblinCharbelcher | LionsEyeDiamond | LotusPetal
          -> { min=4; max=4 }  // Core cards are always maxed out

        | EmptyTheWarrens
          -> { min=3; max=3 }  // One of these has to be in the sideboard.

        | ChancellorOfTheTangle | ChromeMox    | DesperateRitual | ElvishSpiritGuide
        | GitaxianProbe         | LandGrant    | Manamorphose    | PyreticRitual
        | RiteOfFlame           | SeethingSong | SerumPowder     | SimianSpiritGuide
        | StreetWraith          | Taiga        | TinderWall
          -> { min=2; max=4 }  // Non-core

    // TODO: 2nd metadeck with 0 BurningWish and 4 EmptyTheWarrens?
    //       ExploreDecks instead takes merged sequences.

    ExploreDecks metaDeck
    closeLog ()
    Console.ReadKey() |> ignore
