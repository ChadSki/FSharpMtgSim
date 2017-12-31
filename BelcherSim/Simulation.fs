module Simulation

open System
open System.Collections.Generic
open Microsoft.FSharp.Collections
open Logging
open Mancala
open Cards
open Deck
open Game

// Number of wins
let SimScore deck numRuns =
    Array.Parallel.init numRuns (fun _ -> AttemptWin deck)
    |> Seq.filter (fun x -> x)
    |> Seq.length

type Results() =
    member val TotalGames = 0 with get, set
    member val Wins = 0 with get, set

// Keeps track of best-performing decks.
let ExploreDecks (metaDecks : MetaDeck list) =

    // Even though we don't have room to save the results of each
    // individual deck, we can get an idea of how good having N
    // of a certain card is.
    let cardStats = new Dictionary<CardCount, Results>()
    for metaDeck in metaDecks do
        for card in allCards do
            let range = metaDeck card
            for count in [range.min .. range.max] do
                cardStats.[(card, count)] <- Results()

    let tallyScores deck trials wins =
        for card in allCards do
            let count = deck |> List.find (fun (x, y) -> x = card)
                             |> fun (x, y) -> y
            let cardCount = card, count
            cardStats.[cardCount].TotalGames <-
                cardStats.[cardCount].TotalGames + trials
            cardStats.[cardCount].Wins <-
                cardStats.[cardCount].Wins + wins

    let combos = metaDecks
                 |> Seq.map(fun metaDeck -> DeckCombinations metaDeck 60)
                 |> Seq.concat

    let mutable bestDeck = []
    let mutable bestRatio = 0.0
    let mutable totalDecksTested = 0
    let mutable tier2Decks = 0
    let mutable tier3Decks = 0

    let shallowTrials = 100
    let shallowThreshold = 6
    let mediumTrials = 500
    let mediumThreshold = 100
    let deepTrials = 2000

    for deck in combos do
        totalDecksTested <- totalDecksTested + 1

        let shallowScore1 = SimScore deck shallowTrials
        tallyScores deck shallowTrials shallowScore1

        let shallowScore2 = SimScore deck shallowTrials
        tallyScores deck shallowTrials shallowScore2

        // Two checks, just to be quite sure
        if shallowScore1 > shallowThreshold &&
           shallowScore2 > shallowThreshold then
            tier2Decks <- tier2Decks + 1

            let mediumScore1 = SimScore deck mediumTrials
            tallyScores deck mediumTrials mediumScore1

            let mediumScore2 = SimScore deck mediumTrials
            tallyScores deck mediumTrials mediumScore2

            // Secondary threshold
            if mediumScore1 > mediumThreshold &&
               mediumScore2 > mediumThreshold then
                tier3Decks <- tier3Decks + 1

                let deepScore1 = SimScore deck deepTrials
                tallyScores deck deepTrials deepScore1

                let deepScore2 = SimScore deck deepTrials
                tallyScores deck deepTrials deepScore2

                let shallowRatio1 = (float shallowScore1) / (float shallowTrials)
                let shallowRatio2 = (float shallowScore2) / (float shallowTrials)
                let mediumRatio1 = (float mediumScore1) / (float mediumTrials)
                let mediumRatio2 = (float mediumScore2) / (float mediumTrials)
                let deepRatio1 = (float deepScore1) / (float deepTrials)
                let deepRatio2 = (float deepScore2) / (float deepTrials)

                log (sprintf "Passable deck %s" (PrettyPrintDeck deck))
                log (sprintf "    Tier 1 score %.4f, %.4f" shallowRatio1 shallowRatio2)
                log (sprintf "    Tier 2 score %.4f, %.4f" mediumRatio1 mediumRatio2)
                log (sprintf "    Tier 3 score %.4f, %.4f" deepRatio1 deepRatio2)

                let deepRatio = (deepRatio1 + deepRatio2) / 2.0
                if deepRatio > bestRatio then
                    bestRatio <- deepRatio
                    bestDeck <- deck
                    log (sprintf "\r\nNew best win ratio %f\r\n" bestRatio)

        // Print status info every now and again.
        else if totalDecksTested % 1000 = 0 then
            let bestSoFar = if bestDeck = [] then "" else PrettyPrintDeck bestDeck
            log (sprintf "\r\nThe best deck so far is %s" bestSoFar)
            log (sprintf "with a win ratio of %f" bestRatio)
            log (sprintf "Total decks %d. Tier 2 %d. Tier 3 %d.\r\n" totalDecksTested tier2Decks tier3Decks)

            log (sprintf "Stats by card:")
            for stat in cardStats do
                let (card, count) = stat.Key
                let result = stat.Value
                let ratio = float(result.Wins) / float(result.TotalGames)
                log (sprintf "  %s * %d = %f" (CardLabel card) count ratio)
            log (sprintf "\r\nout of %d games so far" totalDecksTested)

    if bestDeck = []
    then log "All decks sucked."
    else log (sprintf "The best deck is %swith a score of %f" (PrettyPrintDeck bestDeck) bestRatio)
    log (sprintf "Total number of decks run: %d" totalDecksTested)

do
    let metaDeckBW = function
        // Core cards are always maxed out
        | BurningWish | GoblinCharbelcher | LionsEyeDiamond | LotusPetal
          -> { min=4; max=4 }

        // One of these has to be in the sideboard for BurningWish
        | EmptyTheWarrens
          -> { min=3; max=3 }

        // Non-core
        | ChancellorOfTheTangle | ChromeMox    | DesperateRitual | ElvishSpiritGuide
        | GitaxianProbe         | LandGrant    | Manamorphose    | PyreticRitual
        | RiteOfFlame           | SeethingSong | SerumPowder     | SimianSpiritGuide
        | StreetWraith          | Taiga        | TinderWall
          -> { min=0; max=4 }

    let metaDeckNoBW = function
        // Core cards are always maxed out
        | EmptyTheWarrens | GoblinCharbelcher | LionsEyeDiamond | LotusPetal
          -> { min=4; max=4 }

        // No BurningWish because we're running four EmptyTheWarrens
        | BurningWish
          -> { min=0; max=0 }

        // Non-core
        | ChancellorOfTheTangle | ChromeMox    | DesperateRitual | ElvishSpiritGuide
        | GitaxianProbe         | LandGrant    | Manamorphose    | PyreticRitual
        | RiteOfFlame           | SeethingSong | SerumPowder     | SimianSpiritGuide
        | StreetWraith          | Taiga        | TinderWall
          -> { min=0; max=4 }

    
    let mutable totalDecks = 0
//        for deck in combos do
//            totalDecks <- totalDecks + 1
//            if totalDecks % 1000000 = 0 then
//                log (sprintf "%d" totalDecks)
//        log (sprintf "Final total %d" totalDecks)
    ExploreDecks [metaDeckBW; metaDeckNoBW]
    closeLog ()
    Console.ReadKey() |> ignore
