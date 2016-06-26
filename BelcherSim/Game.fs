module Game

open System
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
               stormCount: int) =

    member val Library = library with get, set
    member val Hand = hand with get, set
    member val Battlefield = battlefield with get, set
    member val Moxen = moxen with get, set
    member val Graveyard = graveyard with get, set
    member val Mana = mana with get, set
    member val StormCount = stormCount with get, set
    member this.Clone() = this.MemberwiseClone() :?> GameState

// General "what should I do next" function.
let rec TakeAction (gs:GameState) : bool =

    // Paying the life cost, GitaxianProbe is effectively free.
    // Draw a card and put GitaxianProbe in the graveyard.
    while gs.Hand |> List.exists ((=) GitaxianProbe) do
        let newHand = RemoveFromCardGroup gs.Hand GitaxianProbe
        let newLibrary, drawn = Draw 1 gs.Library
        gs.Hand <- List.append newHand drawn
        gs.Library <- newLibrary
        gs.Graveyard <- GitaxianProbe :: gs.Graveyard

    // Paying the life cost, StreetWraith is effectively free.
    // Draw a card and put StreetWraith in the graveyard.
    while gs.Hand |> List.exists ((=) StreetWraith) do
        let newHand = RemoveFromCardGroup gs.Hand StreetWraith
        let newLibrary, drawn = Draw 1 gs.Library
        gs.Hand <-  List.append newHand drawn
        gs.Library <- newLibrary
        gs.Graveyard <- StreetWraith :: gs.Graveyard

    // Cast LED now and increase the storm count. We can use it for mana later.
    while gs.Hand |> List.exists ((=) LionsEyeDiamond) do
        gs.Hand <- RemoveFromCardGroup gs.Hand LionsEyeDiamond
        gs.Battlefield <- (LionsEyeDiamond, false) :: gs.Battlefield
        gs.StormCount <- gs.StormCount + 1

    // ChromeMox imprints a colored card, then can tap to generate
    // mana of those colors. Even if nothing is imprinted, it helps
    // boost the storm count.
    while gs.Hand |> List.exists ((=) ChromeMox) do
        gs.Hand <- RemoveFromCardGroup gs.Hand ChromeMox
        gs.Battlefield <- (ChromeMox, false) :: gs.Battlefield
        gs.StormCount <- gs.StormCount + 1

        // If we imprinted a card, add its color to the list of moxen.
        match ChooseImprint gs.Hand with
        | None -> ()
        | Some imprint ->
            gs.Hand <- RemoveFromCardGroup gs.Hand imprint
            gs.Moxen <- (Color (Cost imprint), false) :: gs.Moxen

    // Exile ElvishSpiritGuide to provide green mana.
    while gs.Hand |> List.exists ((=) ElvishSpiritGuide) do
        gs.Hand <- RemoveFromCardGroup gs.Hand ElvishSpiritGuide
        gs.Mana <- gs.Mana + oneGreen

    // Exile SimianSpiritGuide to provide green mana.
    while gs.Hand |> List.exists ((=) SimianSpiritGuide) do
        gs.Hand <- RemoveFromCardGroup gs.Hand SimianSpiritGuide
        gs.Mana <- gs.Mana + oneRed

    // Cast all TinderWalls we can
    while gs.Hand |> List.exists ((=) TinderWall) &&
          gs.Mana.green + gs.Mana.redgreen > 0 do
        ()

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
            TakeAction (new GameState (library, hand, [], [], [], startingMana, 0))

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
