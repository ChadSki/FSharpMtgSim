module Simulation

open System
open FSharp.Collections.ParallelSeq
open System.Collections.Generic
open Microsoft.FSharp.Collections
open Logging
open Mancala
open Cards
open Deck
open Game


let consumeAllCombos metaDecks =
    let total = metaDecks
                |> Seq.map(fun metaDeck -> DeckCombinations metaDeck 60)
                |> Seq.concat
                |> Seq.length
    log (sprintf "Final total %d" total)

let SimScore deck numRuns =
    let mutable wins = 0
    for trial in [0..numRuns] do
        if AttemptWin deck
        then wins <- wins + 1
    wins

type Results() =
    member val TotalGames = 0 with get, set
    member val Wins = 0 with get, set

// Keeps track of best-performing decks.
let ExploreDecks metaDecks =

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

    let printStats () =
        log (sprintf "Stats by card:")
        for stat in cardStats do
            let (card, count) = stat.Key
            let result = stat.Value
            let ratio = float(result.Wins) / float(result.TotalGames)
            log (sprintf "  %s * %d = %.4f" (CardLabel card) count ratio)

    let tallyAgent = MailboxProcessor.Start(fun inbox ->
        let mutable msgNum = 0
        let rec messageLoop() = async {
            let! deck, trials, wins = inbox.Receive()
            tallyScores deck trials wins
            msgNum <- msgNum + 1
            if msgNum % 700000 = 0 then
                printStats()
            return! messageLoop()
            }
        messageLoop())

    let numTrials = 3

    metaDecks
    |> Seq.map(fun metaDeck -> DeckCombinations metaDeck 60)
    |> Seq.concat
    |> PSeq.map (fun deck ->
        let numWins = SimScore deck numTrials
        tallyAgent.Post(deck, numTrials, numWins))
    |> PSeq.reduce (fun x y -> ())

    printStats()

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

    
    ExploreDecks [metaDeckBW; metaDeckNoBW]
    //consumeAllCombos [metaDeckBW; metaDeckNoBW]
    closeLog ()
    log "Exit..."
    Console.ReadKey() |> ignore
