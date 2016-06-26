module Simulation

open System
open Microsoft.FSharp.Collections
open Mancala
open Cards
open Deck
open Game

// individual deck score
let SimScore deck =
    printfn "%s" (PrettyPrintDeck deck)
    if AttemptWin deck
    then 1.0
    else 0.0

// keeps track of best-performing decks
let ExploreDecks metaDeck =
    // TODO for range in metadeck, create mapping of
    // Card -> numCards:int ->(numWins:int, numGames:int)
    // Even if we can't save the results of each individual deck, we can
    // get an idea of how good having N of a certain card is.

    let combos = DeckCombinations metaDeck 60

    // The first is best by default
    let bestDeck = ref (combos |> Seq.take(1)
                               |> Seq.exactlyOne)
    let bestScore = ref 0.0

    for deck in combos |> Seq.take(120) do
        let score = SimScore deck
        if score > !bestScore then
            bestScore := score
            bestDeck := deck

    printfn "The best deck is %s" (PrettyPrintDeck !bestDeck)

do
    let myMetaDeck = function
        | BurningWish | EmptyTheWarrens | GoblinCharbelcher | LionsEyeDiamond | LotusPetal
          -> { min=4; max=4 }  // Core cards are always maxed out

        | ChancellorOfTheTangle | ChromeMox    | DesperateRitual | ElvishSpiritGuide
        | GitaxianProbe         | LandGrant    | Manamorphose    | PyreticRitual
        | RiteOfFlame           | SeethingSong | SerumPowder     | SimianSpiritGuide
        | StreetWraith          | Taiga        | TinderWall
          -> { min=0; max=4 }  // Non-core

    ExploreDecks myMetaDeck
    ()
