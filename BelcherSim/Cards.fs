module Cards

open System
open Microsoft.FSharp.Reflection
open Mancala

// The only cards we can encounter in this sim
type Card = | BurningWish       | ChancellorOfTheTangle | ChromeMox     | DesperateRitual
            | ElvishSpiritGuide | EmptyTheWarrens       | GitaxianProbe | GoblinCharbelcher
            | LandGrant         | LionsEyeDiamond       | LotusPetal    | Manamorphose
            | PyreticRitual     | RiteOfFlame           | SeethingSong  | SerumPowder
            | SimianSpiritGuide | StreetWraith          | Taiga         | TinderWall

// A list of all the above cases
let allCards = FSharpType.GetUnionCases typeof<Card>
               |> Array.map(fun caseInfo -> FSharpValue.MakeUnion(caseInfo, [||]) :?> Card)
               |> Array.toList

// For each card, how many of that card may appear in a deck
type MetaDeck = Card -> Range
and Range = { min:int; max:int }

// For each card, how many are present in this deck combination
type Deck = (Card * int) list

// Generate a string that represents the given deck state
let PrettyPrintDeck (deck:Deck) = deck
                                  |> List.map (fun (_, count) -> sprintf "%d " count)
                                  |> List.reduce (+)


// Given a metadeck and number of cards, creates the inital MancalaSequence board
let MetaDeckToEmptyBoard (metaDeck:MetaDeck) (totalCards:int) : Board * int =
    let numCards = ref totalCards
    let emptyBoard =
        allCards |> List.map (fun card -> let range = metaDeck card
                                          numCards := !numCards - range.min
                                          { count = 0
                                            max = (range.max - range.min) })

    // In F#, `!` is used to access a ref cell. It's not logical negation.
    if !numCards < 0 then raise (new ArgumentException "Too few cards to distribute minimums")
    else emptyBoard, !numCards

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
