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
               battlefield: Card list,  // Cards in play
               graveyard: Card list,
               mana: ManaAmount,
               pendingCosts: ManaAmount,
               stormCount: int,
               playedLand: bool) =

    member val Library = library with get, set
    member val Hand = hand with get, set
    member val Battlefield = battlefield with get, set
    member val Graveyard = graveyard with get, set
    member val Mana = mana with get, set
    member val PendingCosts = pendingCosts with get, set
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
        log "Playing ChromeMox."
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
        |> List.exists (function
            | None ->
                log "Entering hypothetical: nothing was imprinted."
                if TakeAction (gs.Clone()) then
                    log "Hypothetical succeeded!"
                    true
                else
                    log "Returning from failed hypothetical."
                    false

            | Some imprint ->
                let imprintColor = Color (Cost imprint)
                log (sprintf "Entering hypothetical: imprinting %s." (CardLabel imprint))
                log (sprintf "Tapping ChromeMox for %s mana." (ColorLabel imprintColor))
                let gs2 = gs.Clone()
                gs2.Hand <- RemoveOneCard gs.Hand imprint
                gs2.Mana <- gs2.Mana + OneMana imprintColor
                gs2.Battlefield <- imprint :: gs2.Battlefield
                if TakeAction gs2 then
                    log "Hypothetical succeeded!"
                    true
                else
                    log "Returning from failed hypothetical."
                    false)

    // Next play all the free cards that allow us to draw.

    // Paying the life cost, GitaxianProbe is effectively free.
    // Draw a card and put GitaxianProbe in the graveyard.
    else if gs.Hand |> HasCard GitaxianProbe then
        log "Playing GitaxianProbe"
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
        log "Playing StreetWraith."
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
        log "Playing LionsEyeDiamond."
        gs.Hand <- RemoveOneCard gs.Hand LionsEyeDiamond
        gs.Battlefield <- LionsEyeDiamond :: gs.Battlefield
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Cast LotusPetal now and increase the storm count. We can use it for mana later.
    else if gs.Hand |> HasCard LotusPetal then
        log "Playing LotusPetal."
        gs.Hand <- RemoveOneCard gs.Hand LotusPetal
        gs.Battlefield <- LotusPetal :: gs.Battlefield
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Exile ElvishSpiritGuide to provide green mana.
    else if gs.Hand |> HasCard ElvishSpiritGuide then
        log "Playing ElvishSpiritGuide."
        gs.Hand <- RemoveOneCard gs.Hand ElvishSpiritGuide
        gs.Mana <- gs.Mana + OneMana Green
        TakeAction gs

    // Exile SimianSpiritGuide to provide green mana.
    else if gs.Hand |> HasCard SimianSpiritGuide then
        log "Playing SimianSpiritGuide."
        gs.Hand <- RemoveOneCard gs.Hand SimianSpiritGuide
        gs.Mana <- gs.Mana + OneMana Red
        TakeAction gs

    // Play our land for the turn, immediately tapping for redgreen mana.
    else if not gs.PlayedLand && gs.Hand |> HasCard Taiga then
        log "Playing Taiga."
        gs.Hand <- RemoveOneCard gs.Hand Taiga
        gs.Battlefield <- Taiga :: gs.Battlefield
        gs.Mana <- gs.Mana + OneMana RedGreen
        gs.PlayedLand <- true
        TakeAction gs

    // Now play our mana-gain cards.

    // Manamorphose first since it allows us to draw and makes redgreen mana.
    else if (gs.Hand |> HasCard Manamorphose &&
             CanPay (gs.PendingCosts + Cost Manamorphose) gs.Mana) then
        log "Playing Manamorphose."
        let newHand = RemoveOneCard gs.Hand Manamorphose
        let newLibrary, drawn = Draw 1 gs.Library
        gs.Hand <-  List.append newHand drawn
        gs.Library <- newLibrary
        gs.PendingCosts <- gs.PendingCosts + Cost Manamorphose  // TODO: Does this PendingCosts business check out,
        gs.Mana <- gs.Mana + (OneMana RedGreen) + (OneMana RedGreen)  // or does it allow fishy business?
        gs.Graveyard <- Manamorphose :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // TinderWall next because it costs scarcer green mana.
    else if (gs.Hand |> HasCard TinderWall &&
             CanPay (gs.PendingCosts + Cost TinderWall) gs.Mana) then
        log "Playing TinderWall."
        gs.Hand <- RemoveOneCard gs.Hand TinderWall
        gs.PendingCosts <- gs.PendingCosts + Cost TinderWall
        gs.Battlefield <- TinderWall :: gs.Battlefield
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // RiteOfFlame is the cheapest red mana source.
    else if (gs.Hand |> HasCard RiteOfFlame &&
             CanPay (gs.PendingCosts + Cost RiteOfFlame) gs.Mana) then
        log "Playing RiteOfFlame."
        gs.Hand <- RemoveOneCard gs.Hand RiteOfFlame
        gs.PendingCosts <- gs.PendingCosts + Cost RiteOfFlame

        // Bonus red mana for each copy of this card in the graveyard.
        let bonusMana = gs.Graveyard
                        |> List.map (function
                            | RiteOfFlame -> OneMana Red
                            | _ -> noMana)
                        |> List.reduce (+)

        gs.Graveyard <- RiteOfFlame :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Relatively cheap mana source, dead simple.
    else if (gs.Hand |> HasCard PyreticRitual &&
             CanPay (gs.PendingCosts + Cost PyreticRitual) gs.Mana) then
        log "Playing PyreticRitual."
        gs.Hand <- RemoveOneCard gs.Hand PyreticRitual
        gs.PendingCosts <- gs.PendingCosts + Cost PyreticRitual
        gs.Mana <- gs.Mana + threeRedMana
        gs.Graveyard <- PyreticRitual :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Large and simple mana source.
    else if (gs.Hand |> HasCard SeethingSong &&
             CanPay (gs.PendingCosts + Cost SeethingSong) gs.Mana) then
        log "Playing SeethingSong."
        gs.Hand <- RemoveOneCard gs.Hand SeethingSong
        gs.PendingCosts <- gs.PendingCosts + Cost SeethingSong
        gs.Mana <- gs.Mana + fiveRedMana
        gs.Graveyard <- SeethingSong :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1
        TakeAction gs

    // Pop some mana!

    // Has card doesn't work... but I don't really need to keep track of tappedness do I?
    else if gs.Battlefield |> HasCard TinderWall then
        log "Popping TinderWall for 2 red mana."
        gs.Battlefield <- RemoveOneCard gs.Battlefield TinderWall
        gs.Mana <- gs.Mana + twoRedMana
        TakeAction gs

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
            TakeAction (new GameState (library, hand, [], [],
                                       startingMana, noMana, 0, false))

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
