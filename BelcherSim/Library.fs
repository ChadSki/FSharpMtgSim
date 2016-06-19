module Library

open System.Security.Cryptography
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

// This crypto provider is thread-safe FYI
let cryptoProvider = new RNGCryptoServiceProvider()

// Return a new, shuffled list
let Shuffle (original:Library) : Library =
    let deck = List.toArray original
    let n = ref deck.Length
    let rnd : byte [] = [|0uy|]

    while !n > 1 do
        // Get random index
        cryptoProvider.GetBytes(rnd)
        let k = int32 (Array.get rnd 0) % !n
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
