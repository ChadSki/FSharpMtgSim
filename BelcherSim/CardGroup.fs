module CardGroup

open System
open CryptoRandom
open Logging
open Cards
open Deck

// Draw n cards. Return a new (smaller) library and the drawn cards.
let Draw n cardGroup =
    log (sprintf "Drawing %d cards. %d left in library." n ((Seq.length cardGroup) - n))
    cardGroup |> Seq.skip n |> Seq.toList,
    cardGroup |> Seq.take n |> Seq.toList

let HasCard card cardGroup =
    cardGroup |> List.exists ((=) card)

let RemoveOneCard cardgroup card : Card list =
    let wanted, others = cardgroup |> List.partition ((=) card)
    List.append others (wanted |> Seq.skip 1 |> Seq.toList)

// Generate a string that represents the given deck state
let PrettyPrintCardGroup (lib:Card list) =
    let numToShow = 15
    let ellipse = if lib.Length > numToShow then "..." else ""
    let libraryTop =
        lib |> Seq.truncate numToShow
            |> Seq.map (fun card -> sprintf "%s " (CardLabel card))
            |> Seq.reduce (+)

    sprintf "Library %d cards: %s%s" lib.Length libraryTop ellipse

let rng = new CryptoRandom()

// Return a new, shuffled list. Fisher-Yates algorithm.
let Shuffle original =
    let deck = List.toArray original
    let n = ref deck.Length

    while !n > 1 do
        // Choose random index
        let k = rng.Next !n
        n := !n - 1

        // Swap
        let temp = Array.get deck k
        Array.set deck k (Array.get deck !n)
        Array.set deck !n temp

    Array.toList deck

// Take a deck definition and actually instatiate that many cards
let AsCardGroup deck =
    deck |> List.map (fun (card, num) -> [for i in 1..num -> card])
         |> List.concat
         |> Shuffle
