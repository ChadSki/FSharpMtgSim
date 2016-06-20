module Library

open System
open CryptoRandom
open Cards
open Deck

type Library = Card list

// Generate a string that represents the given deck state
let PrettyPrintLibrary (lib:Library) =
    let numToShow = 15
    let ellipse = if lib.Length > numToShow then "..." else ""
    let libraryTop =
        lib |> Seq.truncate numToShow
            |> Seq.map (fun card -> sprintf "%s " (CardLabel card))
            |> Seq.reduce (+)

    sprintf "Library %d cards: %s%s" lib.Length libraryTop ellipse

let rng = new CryptoRandom()

// Return a new, shuffled list. Fisher-Yates algorithm.
let Shuffle (original:Library) : Library =
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
let AsLibrary deck =
    deck |> List.map (fun (card, num) -> [for i in 1..num -> card])
         |> List.concat
         |> Shuffle
