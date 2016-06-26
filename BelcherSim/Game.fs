module Game

open System
open Logging
open Cards
open Deck
open CardGroup
open Mana
open ChromeMox

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
let rec TakeAction (gs:GameState) : bool =

    // Paying the life cost, GitaxianProbe is effectively free.
    // Draw a card and put GitaxianProbe in the graveyard.
    while gs.Hand |> HasCard GitaxianProbe do
        log "Playing GitaxianProbe"
        let newHand = RemoveOneCard gs.Hand GitaxianProbe
        let newLibrary, drawn = Draw 1 gs.Library
        gs.Hand <- List.append newHand drawn
        gs.Library <- newLibrary
        gs.Graveyard <- GitaxianProbe :: gs.Graveyard

    // Paying the life cost, StreetWraith is effectively free.
    // Draw a card and put StreetWraith in the graveyard.
    while gs.Hand |> HasCard StreetWraith do
        log "Playing StreetWrait.h"
        let newHand = RemoveOneCard gs.Hand StreetWraith
        let newLibrary, drawn = Draw 1 gs.Library
        gs.Hand <-  List.append newHand drawn
        gs.Library <- newLibrary
        gs.Graveyard <- StreetWraith :: gs.Graveyard

    // Cast LED now and increase the storm count. We can use it for mana later.
    while gs.Hand |> HasCard LionsEyeDiamond do
        log "Playing LionsEyeDiamond."
        gs.Hand <- RemoveOneCard gs.Hand LionsEyeDiamond
        gs.Battlefield <- (LionsEyeDiamond, false) :: gs.Battlefield
        gs.StormCount <- gs.StormCount + 1

    // ChromeMox imprints a colored card, then can tap to generate
    // mana of those colors. Even if nothing is imprinted, it helps
    // boost the storm count.
    while gs.Hand |> HasCard ChromeMox do
        log "Playing ChromeMox."
        gs.Hand <- RemoveOneCard gs.Hand ChromeMox
        gs.Battlefield <- (ChromeMox, false) :: gs.Battlefield
        gs.StormCount <- gs.StormCount + 1

        // If we imprinted a card, add its color to the list of moxen.
        match ChooseImprint gs.Hand with
        | None -> log "Nothing was imprinted."
        | Some imprint ->
            let imprintColor = Color (Cost imprint)
            log (sprintf "Imprinted %s for %s." (CardLabel imprint) (ColorLabel imprintColor))
            gs.Hand <- RemoveOneCard gs.Hand imprint
            gs.Moxen <- (imprintColor, false) :: gs.Moxen

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
    while gs.Hand |> HasCard TinderWall &&
          CanPlay TinderWall gs.PendingCosts gs.Mana do
        log "Playing TinderWall."
        gs.PendingCosts <- gs.PendingCosts + Cost TinderWall
        gs.Hand <- RemoveOneCard gs.Hand TinderWall
        gs.Battlefield <- (TinderWall, false) :: gs.Battlefield

    false


// Mulligan until we find an adequate hand.
let rec MulliganOrPlay (deck:Deck) (handSize:int) : bool =

    // Failure if we have mulliganed to death
    if handSize = 0 then
        false
    else
        // Shuffle the deck and draw our opening hand
        let library, hand = Draw handSize (AsCardGroup deck)

        // Mulligan right away unless we hold key cards
        if hand |> Seq.exists IsWinCondition then

            // This hand is worth keeping.
            // Collect our free mana, if we get any.
            let numCott =
                hand |> List.map (fun card -> if card = ChancellorOfTheTangle
                                              then 1 else 0)
                     |> List.reduce (+)

            let startingMana = { red=0; green=numCott; redgreen=0; colorless=0; other=0 }

            // Start playing with this hand
            TakeAction (new GameState (library, hand, [], [], [], startingMana, noMana, 0))

        else
            // Can we do the special mulligan?
            if hand |> Seq.exists ((=) SerumPowder) then

                // Exile Serum Powder and mulligan with the same hand size
                let newDeck = deck |> List.map (fun (card, num) ->
                    (card, if card = SerumPowder then num - 1 else num))
                MulliganOrPlay newDeck handSize

            else
                // Normal mulligan
                MulliganOrPlay deck (handSize - 1)

// Can we win the game? If so, return true. Otherwise, false.
let AttemptWin deck = MulliganOrPlay deck 7
