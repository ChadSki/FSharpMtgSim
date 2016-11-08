module Deck

open System
open Mancala
open Cards

// For each card, how many of that card may appear in a deck
type MetaDeck = Card -> Range
and Range = { min:int; max:int }

// For each card, how many are present in this deck combination
type Deck = (Card * int) list

// Generate a string that represents the given deck state
let PrettyPrintDeck (deck:Deck) =
    deck |> List.map (fun (card, count) -> sprintf "%d %s, " count (CardLabel card))
         |> List.reduce (+)


// Given a metadeck and number of cards, creates the inital MancalaSequence board
let MetaDeckToEmptyBoard (metaDeck:MetaDeck) (totalCards:int) : Board * int =
    let mutable numCards = totalCards
    let emptyBoard =
        allCards |> List.map (fun card -> let range = metaDeck card
                                          numCards <- numCards - range.min
                                          { count = 0
                                            max = (range.max - range.min) })

    if numCards < 0 then raise (new ArgumentException "Too few cards to distribute minimums required by the deck definition.")
    else emptyBoard, numCards

// Given a metadeck and MancalaSequence board state, returns the corresponding deck
let BoardToDeck (metaDeck:MetaDeck) (combo:Board) : Deck =
    List.zip allCards combo
    |> List.map (fun (card, slot) -> card, slot.count + (metaDeck card).min)

// Iterate through all possible deck combinations for a given metadeck and deck size
let DeckCombinations (metaDeck:MetaDeck) (totalCards:int) : seq<Deck> =
    let emptyBoard, leftoverBeads = MetaDeckToEmptyBoard metaDeck totalCards
    seq {
        for board in MancalaSequence emptyBoard leftoverBeads do
            yield BoardToDeck metaDeck board }
