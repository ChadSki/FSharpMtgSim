module Game

open System
open Cards
open Deck
open CardGroup
open Mana
open ChromeMox

type GameState(_library: Card list,
               _hand: Card list,
               _battlefield: (Card * bool) list,  // Card, and whether it's tapped
               _moxen: (ManaColor * bool) list,  // Color, and whether it's been used
               _graveyard: Card list,
               _mana: ManaAmount) =

    member x.Library = ref _library
    member x.Hand = ref _hand
    member x.Battlefield = ref _battlefield
    member x.Moxen = ref _moxen
    member x.Graveyard = ref _graveyard
    member x.Mana = ref _mana

// General "what should I do next" function, can be called recursively.
let TakeAction (gs:GameState) =

    // Play Chrome Mox if possible
    if !(gs.Hand) |> List.exists ((=) ChromeMox) then

        // Remove from hand and put in play
        gs.Hand := RemoveFromCardGroup !(gs.Hand) ChromeMox
        gs.Battlefield := (ChromeMox, false) :: !(gs.Battlefield)

        // If we imprinted a card, add its color to the list of moxen
        match ChooseImprint !(gs.Hand) with
        | None -> ()
        | Some imprint ->
            gs.Hand := RemoveFromCardGroup !(gs.Hand) imprint
            gs.Moxen := (Color (Cost imprint), false) :: !(gs.Moxen)

        true
    else
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
            TakeAction (new GameState (library, hand, [], [], [], startingMana))

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
