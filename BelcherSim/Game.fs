module Game

open System
open Cards
open Deck
open Library
open Mana

let IsWinCondition = function
| BurningWish | EmptyTheWarrens | GoblinCharbelcher
  -> true
| ChancellorOfTheTangle | ChromeMox         | DesperateRitual | ElvishSpiritGuide
| GitaxianProbe         | LandGrant         | LionsEyeDiamond | LotusPetal
| Manamorphose          | PyreticRitual     | RiteOfFlame     | SeethingSong
| SerumPowder           | SimianSpiritGuide | StreetWraith    | Taiga
| TinderWall
  -> false


// Draw n cards. Return a new (smaller) library and the drawn cards.
let Draw n library =
    let drawnCards = library |> Seq.take n
    let newLibrary = library |> Seq.skip n
    newLibrary, drawnCards

// Can we trigger the combo? If so, return true. Otherwise, false.
// Allows recursion for mulliganing.
let rec AttemptWin (deck:Deck) (handSize:int) : bool =

    // Failure if we have mulliganed to death
    match handSize with
    | 0 -> false
    | _ ->
        // Shuffle the deck and draw our opening hand
        let library, hand = Draw handSize (AsLibrary deck)

        // Do we have any win conditions in our hand?
        match hand |> Seq.filter IsWinCondition |> Seq.isEmpty with
        | true ->
            // No win conditions, so mulligan
            AttemptWin deck (handSize - 1)

        | false ->
            // We have a win condition, so we win ;) (jk actually TODO)
            true
