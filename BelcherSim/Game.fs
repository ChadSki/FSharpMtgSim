﻿module Game

open System
open Logging
open Cards
open Deck
open CardGroup
open Mana

// Big 'ol mutable wad of state.
type GameState(library: Card list,
               hand: Card list,
               battlefield: Card list,
               graveyard: Card list,
               mana: ManaAmount,
               stormCount: int,
               playedLand: bool) =

    member val Library = library with get, set
    member val Hand = hand with get, set
    member val Battlefield = battlefield with get, set
    member val Graveyard = graveyard with get, set
    member val Mana = mana with get, set
    member val StormCount = stormCount with get, set
    member val PlayedLand = playedLand with get, set
    member this.Clone() = this.MemberwiseClone() :?> GameState

// General "what should I do next" function.
// Returns true if we have won, false otherwise.
let rec TakeAction (gs:GameState) : bool =

    // Play ChromeMox first, since it's free and has the most possibilities.

    // ChromeMox imprints a colored card, then can tap to generate
    // mana of those colors. Even if nothing is imprinted, it helps
    // boost the storm count.
    if gs.Hand |> HasCard ChromeMox then
        log "P ChM"
        gs.Hand <- RemoveOneCard gs.Hand ChromeMox
        gs.Battlefield <- ChromeMox :: gs.Battlefield
        gs.StormCount <- gs.StormCount + 1

        // Not sure the best way to choose a card to imprint, so let's try them all!
        // `None` represents not imprinting a card, otherwise `Some card`.
        let possibleImprints =
            None :: (gs.Hand |> Seq.distinct
                             |> Seq.filter (fun card -> not (Colorless = Color (Cost card)))
                             |> Seq.map (fun card -> Some card)
                             |> Seq.toList)

        // Try each possibility in turn. Stop if a possibility wins the game.
        // `List.exists` will short-circuit if a win is discovered.
        possibleImprints
        |> List.exists (fun maybeImprint ->
            let gs2 = gs.Clone()
            match maybeImprint with
            | None -> log "Hypothetical: no imprint"
            | Some imprint ->
                let imprintColor = Color (Cost imprint)
                log (sprintf "Hypothetical: imprint %s, +1 %s" (CardLabel imprint) (ColorLabel imprintColor))
                gs2.Hand <- RemoveOneCard gs.Hand imprint
                gs2.Mana <- gs2.Mana + OneMana imprintColor

            // This if-statement breaks tail-recursion, but probably helps with debugging.
            if TakeAction gs2 then
                log "Hyp.succeeded!"
                true
            else
                log "Hyp.failed"
                false)

    // Next play all the free cards that allow us to draw.

    // Paying the life cost, GitaxianProbe is effectively free.
    // Draw a card and put GitaxianProbe in the graveyard.
    else if gs.Hand |> HasCard GitaxianProbe then
        log "P GPr"
        let newHand = RemoveOneCard gs.Hand GitaxianProbe
        let newLibrary, drawn = Draw 1 gs.Library
        gs.Hand <- List.append newHand drawn
        gs.Library <- newLibrary
        gs.Graveyard <- GitaxianProbe :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Paying the life cost, StreetWraith is effectively free.
    // Draw a card and put StreetWraith in the graveyard.
    else if gs.Hand |> HasCard StreetWraith then
        log "P SWr"
        let newHand = RemoveOneCard gs.Hand StreetWraith
        let newLibrary, drawn = Draw 1 gs.Library
        gs.Hand <-  List.append newHand drawn
        gs.Library <- newLibrary
        gs.Graveyard <- StreetWraith :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Next play all other free cards.

    // Cast LED now and increase the storm count. We can use it for mana later.
    else if gs.Hand |> HasCard LionsEyeDiamond then
        log "P LED"
        gs.Hand <- RemoveOneCard gs.Hand LionsEyeDiamond
        gs.Battlefield <- LionsEyeDiamond :: gs.Battlefield
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Cast LotusPetal now and immediately pop for redgreen mana.
    else if gs.Hand |> HasCard LotusPetal then
        log "P LoP, +1R/G"
        gs.Hand <- RemoveOneCard gs.Hand LotusPetal
        gs.Mana <- gs.Mana + OneMana RedGreen
        gs.Graveyard <- LotusPetal :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Exile ElvishSpiritGuide to provide green mana.
    else if gs.Hand |> HasCard ElvishSpiritGuide then
        log "Exile ESG, +1G"
        gs.Hand <- RemoveOneCard gs.Hand ElvishSpiritGuide
        gs.Mana <- gs.Mana + OneMana Green
        TakeAction gs

    // Exile SimianSpiritGuide to provide green mana.
    else if gs.Hand |> HasCard SimianSpiritGuide then
        log "Exile SSG, +1R"
        gs.Hand <- RemoveOneCard gs.Hand SimianSpiritGuide
        gs.Mana <- gs.Mana + OneMana Red
        TakeAction gs

    // Play our land for the turn, immediately tapping for redgreen mana.
    else if not gs.PlayedLand && gs.Hand |> HasCard Taiga then
        log "P Taiga, +1R/G"
        gs.Hand <- RemoveOneCard gs.Hand Taiga
        gs.Battlefield <- Taiga :: gs.Battlefield
        gs.Mana <- gs.Mana + OneMana RedGreen
        gs.PlayedLand <- true
        TakeAction gs

    // Now play our mana-gain cards.

    // Manamorphose first since it allows us to draw and makes redgreen mana.
    else if gs.Hand |> HasCard Manamorphose && CanPay (Cost Manamorphose) gs.Mana then
        log "P MMp, ~2x(R/G)"
        let newHand = RemoveOneCard gs.Hand Manamorphose
        let newLibrary, drawn = Draw 1 gs.Library
        gs.Hand <-  List.append newHand drawn
        gs.Library <- newLibrary

        // Pay first, then reap the bounty.
        match gs.Mana - Cost Manamorphose with
        | None -> raise (new InvalidOperationException "We already asserted that we can pay for Manamorphose.")
        | Some result ->
            gs.Mana <- result + twoRedGreenMana

        gs.Graveyard <- Manamorphose :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // TinderWall next because it costs scarcer green mana.
    else if gs.Hand |> HasCard TinderWall && CanPay (Cost TinderWall) gs.Mana then
        log "P TiW, +2R"
        gs.Hand <- RemoveOneCard gs.Hand TinderWall

        // Pay first, then reap the bounty.
        match gs.Mana - Cost TinderWall with
        | None -> raise (new InvalidOperationException "We already asserted that we can pay for TinderWall.")
        | Some result ->
            gs.Mana <- result + twoRedMana

        gs.Graveyard <- TinderWall :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // RiteOfFlame is the cheapest red mana source.
    else if gs.Hand |> HasCard RiteOfFlame && CanPay (Cost RiteOfFlame) gs.Mana then

        // Bonus red mana for each copy of this card in the graveyard.
        let bonusMana =
            noMana :: (gs.Graveyard
                       |> List.map (function
                           | RiteOfFlame -> OneMana Red
                           | _ -> noMana))
            |> List.reduce (+)

        log (sprintf "P RoF, +%dR" (bonusMana.red + 1))
        gs.Hand <- RemoveOneCard gs.Hand RiteOfFlame

        // Pay first, then reap the bounty.
        match gs.Mana - Cost RiteOfFlame with
        | None -> raise (new InvalidOperationException "We already asserted that we can pay for RiteOfFlame.")
        | Some result ->
            gs.Mana <- result + OneMana Red + bonusMana

        gs.Graveyard <- RiteOfFlame :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Relatively cheap mana source, dead simple.
    else if gs.Hand |> HasCard PyreticRitual && CanPay (Cost PyreticRitual) gs.Mana then
        log "P PyR, +1R"
        gs.Hand <- RemoveOneCard gs.Hand PyreticRitual

        // Pay first, then reap the bounty.
        match gs.Mana - Cost PyreticRitual with
        | None -> raise (new InvalidOperationException "We already asserted that we can pay for PyreticRitual.")
        | Some result ->
            gs.Mana <- result + threeRedMana

        gs.Graveyard <- PyreticRitual :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Large and simple mana source.
    else if gs.Hand |> HasCard SeethingSong && CanPay (Cost SeethingSong) gs.Mana then
        log "P SSo, +2R"
        gs.Hand <- RemoveOneCard gs.Hand SeethingSong

        // Pay first, then reap the bounty.
        match gs.Mana - Cost SeethingSong with
        | None -> raise (new InvalidOperationException "We already asserted that we can pay for SeethingSong.")
        | Some result ->
            gs.Mana <- result + fiveRedMana

        gs.Graveyard <- SeethingSong :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // This works best if we have as much mana as possible,
    // so it's the last used mana source.
    else if gs.Hand |> HasCard DesperateRitual && CanPay (Cost DesperateRitual) gs.Mana then
        log "P Drt, +1R"
        gs.Hand <- RemoveOneCard gs.Hand DesperateRitual

        let mutable pendingCosts = Cost DesperateRitual

        // If there are other DesperateRituals in our hand, they can be
        // grafted onto this one.
        let mutable successfulGrafts = 0
        for desperateRitual in gs.Hand |> List.filter ((=) DesperateRitual) do
            if CanPay (pendingCosts + Cost DesperateRitual) gs.Mana then
                pendingCosts <- pendingCosts + Cost DesperateRitual
                successfulGrafts <- successfulGrafts + 1

        log (sprintf "Graft %d, +%dR" successfulGrafts (successfulGrafts * 3))

        // Pay first, then reap the bounty.
        match gs.Mana - pendingCosts with
        | None -> raise (new InvalidOperationException "We should be able to pay for all these DesperateRituals.")
        | Some result ->
            gs.Mana <- result + threeRedMana  // Mana for the host card itself.
            while successfulGrafts > 0 do
                gs.Mana <- gs.Mana + threeRedMana  // Bonus mana for each graft.
                successfulGrafts <- successfulGrafts - 1

        gs.Graveyard <- DesperateRitual :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Here come the win conditions, baby!

    // TODOs:
    // BurningWish ->           # Sorcery from sideboard (EtW)
    // ChancellorOfTheTangle -> # Really? Casting this?
    // EmptyTheWarrens ->       # Win condition
    // GoblinCharbelcher ->     # Win condition
    // LandGrant ->             # Free if no lands in hand

    else if gs.Hand |> HasCard GoblinCharbelcher && CanPay (Cost GoblinCharbelcher) gs.Mana then
        log "P GCb"
        gs.Hand <- RemoveOneCard gs.Hand GoblinCharbelcher

        // Pay the casting cost.
        match gs.Mana - Cost GoblinCharbelcher with
        | None -> raise (new InvalidOperationException "We already asserted that we can pay for GoblinCharbelcher.")
        | Some result ->
            gs.Mana <- result

        // Crack open a diamond! Requires discarding your hand.
        if gs.Battlefield |> HasCard LionsEyeDiamond then
            gs.Battlefield <- RemoveOneCard gs.Battlefield LionsEyeDiamond
            gs.Graveyard <- LionsEyeDiamond :: List.append gs.Graveyard gs.Hand
            gs.Hand <- []
            gs.Mana <- gs.Mana + threeRedGreenMana

        // Pay the cost to activate Charbelcher's ability.
        match gs.Mana - threeColorlessMana with
        | None ->
            // Failed to play Charbelcher's ability. You lose.
            false

        | Some result ->
            // Ability activated! Reveal cards until a land is encountered.
            let mutable continueRevealing = true
            let revealed, remaining =
                gs.Library
                |> List.partition (fun card ->
                    if continueRevealing then
                        if card = Taiga then
                            continueRevealing <- false
                        true
                    else false)

            // Double damage for encountering a mountain (Taiga).
            let multiplier =
                if Taiga = Seq.last revealed then 2 else 1

            // We win if we deal more than 20 damage
            let damage = (List.length revealed) * multiplier
            if damage >= 20 then
                log (sprintf "We win, dealing %d damage!" damage)
                true
            else
                log (sprintf "We lose, dealing only %d damage." damage)
                false

    else
        false


// Mulligan until we find an adequate hand.
let rec MulliganOrPlay (deck:Deck) (handSize:int) : bool =

    // Failure if we have mulliganed to death
    if handSize = 0 then
        log "Mulliganed to death."
        false
    else
        // Shuffle the deck and draw our opening hand
        let library, hand = Draw handSize (AsCardGroup deck)

        // Mulligan right away unless we hold key cards
        if hand |> Seq.exists IsWinCondition then
            log (sprintf "Choosing a hand with %d win condition(s)."
                         (hand |> List.filter IsWinCondition |> List.length))

            // Collect our free mana, if we get any.
            let numCott =
                hand |> List.map (fun card -> if card = ChancellorOfTheTangle
                                              then 1 else 0)
                     |> List.reduce (+)

            if numCott > 0 then
                log (sprintf "Collecting %d green mana from ChancellorOfTheTangle." numCott)
            let startingMana = { red=0; green=numCott; redgreen=0; colorless=0; other=0 }

            // Start playing with this hand
            if TakeAction (new GameState (library, hand, [], [], startingMana, 0, false)) then
                log "win"
                true
            else
                log "lose"
                false

        else
            // Can we do the special mulligan?
            if hand |> Seq.exists ((=) SerumPowder) then
                log "Mulliganing with SerumPowder."

                // Exile Serum Powder and mulligan with the same hand size
                let newDeck = deck |> List.map (fun (card, num) ->
                    (card, if card = SerumPowder then num - 1 else num))
                MulliganOrPlay newDeck handSize

            else
                // Normal mulligan
                log "Mulliganing."
                MulliganOrPlay deck (handSize - 1)

// Can we win the game? If so, return true. Otherwise, false.
let AttemptWin deck = MulliganOrPlay deck 7
