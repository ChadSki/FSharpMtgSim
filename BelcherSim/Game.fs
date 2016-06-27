module Game

open System
open Logging
open Cards
open Deck
open CardGroup
open Mana

// Big 'ol mutable wad of state.
type GameState(library: Card list,
               hand: Card list,
               battlefield: (Card * bool) list,  // Card, and whether it's tapped
               moxen: (ManaColor * bool) list,  // Imprinted Chrome Mox colors, and whether they've been spent
               graveyard: Card list,
               mana: ManaAmount,
               pendingCosts: ManaAmount,
               stormCount: int) =

    member val Library = library with get, set
    member val Hand = hand with get, set
    member val Battlefield = battlefield with get, set
    member val Moxen = moxen with get, set
    member val Graveyard = graveyard with get, set
    member val Mana = mana with get, set
    member val PendingCosts = pendingCosts with get, set
    member val StormCount = stormCount with get, set
    member this.Clone() = this.MemberwiseClone() :?> GameState

// General "what should I do next" function.
// Returns true if we have won, false otherwise.
let rec TakeAction (gs:GameState) : bool =

    // ChromeMox imprints a colored card, then can tap to generate
    // mana of those colors. Even if nothing is imprinted, it helps
    // boost the storm count.
    if gs.Hand |> HasCard ChromeMox then
        log "Playing ChromeMox."
        gs.Hand <- RemoveOneCard gs.Hand ChromeMox
        gs.Battlefield <- (ChromeMox, false) :: gs.Battlefield
        gs.StormCount <- gs.StormCount + 1

        // Not sure which card to choose, so let's try them all!
        // `None` represents not imprinting a card, otherwise `Some card`.
        // Stop if a possibility wins the game (`List.exists` will short-circuit).
        //
        // TODO: For performance, sort so that we're more likely to play the best option first?
        None :: (gs.Hand |> Seq.distinct
                         |> Seq.filter (fun card -> not (Colorless = Color (Cost card)))
                         |> Seq.map (fun card -> Some card)
                         |> Seq.toList)
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
                log (sprintf "Entering hypothetical: imprinting %s for %s." (CardLabel imprint) (ColorLabel imprintColor))
                let hypotheticalGameState = gs.Clone()
                hypotheticalGameState.Hand <- RemoveOneCard gs.Hand imprint
                hypotheticalGameState.Moxen <- (imprintColor, false) :: gs.Moxen
                if TakeAction hypotheticalGameState then
                    log "Hypothetical succeeded!"
                    true
                else
                    log "Returning from failed hypothetical."
                    false)

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

    // Cast Manamorphose if we can.
    else if (gs.Hand |> HasCard Manamorphose &&
             CanPay (gs.PendingCosts + Cost Manamorphose) gs.Mana) then
        log "Playing Manamorphose."
        let newHand = RemoveOneCard gs.Hand Manamorphose
        let newLibrary, drawn = Draw 1 gs.Library
        gs.Hand <-  List.append newHand drawn
        gs.Library <- newLibrary
        gs.Graveyard <- Manamorphose :: gs.Graveyard
        gs.StormCount <- gs.StormCount + 1

        // TODO: Does this PendingCosts business check out,
        // or does it allow fishy business?
        gs.PendingCosts <- gs.PendingCosts + Cost Manamorphose
        gs.Mana <- gs.Mana + oneRedGreen + oneRedGreen
        TakeAction gs

    else
        // Cast LED now and increase the storm count. We can use it for mana later.
        while gs.Hand |> HasCard LionsEyeDiamond do
            log "Playing LionsEyeDiamond."
            gs.Hand <- RemoveOneCard gs.Hand LionsEyeDiamond
            gs.Battlefield <- (LionsEyeDiamond, false) :: gs.Battlefield
            gs.StormCount <- gs.StormCount + 1

        // Exile ElvishSpiritGuide to provide green mana.
        while gs.Hand |> HasCard ElvishSpiritGuide do
            log "Playing ElvishSpiritGuide."
            gs.Hand <- RemoveOneCard gs.Hand ElvishSpiritGuide
            gs.Mana <- gs.Mana + oneGreen

        // Exile SimianSpiritGuide to provide green mana.
        while gs.Hand |> HasCard SimianSpiritGuide do
            log "Playing SimianSpiritGuide."
            gs.Hand <- RemoveOneCard gs.Hand SimianSpiritGuide
            gs.Mana <- gs.Mana + oneRed

        // Cast all TinderWalls we can.
        while (gs.Hand |> HasCard TinderWall &&
               CanPay (gs.PendingCosts + Cost TinderWall) gs.Mana) do
            log "Playing TinderWall."
            gs.PendingCosts <- gs.PendingCosts + Cost TinderWall
            gs.Hand <- RemoveOneCard gs.Hand TinderWall
            gs.Battlefield <- (TinderWall, false) :: gs.Battlefield
            gs.StormCount <- gs.StormCount + 1

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

            // Collect our free mana, if we get any.
            let numCott =
                hand |> List.map (fun card -> if card = ChancellorOfTheTangle
                                              then 1 else 0)
                     |> List.reduce (+)

            if numCott > 0 then
                log (sprintf "Collecting %d green mana from ChancellorOfTheTangle." numCott)
            let startingMana = { red=0; green=numCott; redgreen=0; colorless=0; other=0 }

            // Start playing with this hand
            TakeAction (new GameState (library, hand, [], [], [], startingMana, noMana, 0))

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
