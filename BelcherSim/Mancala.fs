module Mancala

// This module defines what I call the 'mancala sequence'. It was developed to iterate through
// Magic the Gathering deck possibilities. Conceptually, it is a lot like the board game Mancala.
//
// MtG allows up to four copies of any one card in print. All of these copies are functionally
// identical, and a deck listing doesn't involve the order of cards -- just their counts.
//
// Mancala is a literal 'board' game. Slots are carved into a board in a row. Tokens (usually
// beads or stones) fill these slots. Players pick up and move tokens from slot to slot. Flow
// across the board is unidirectional.
//
// The mancala sequence imagines a board where each slot represents one MtG card. The slot may be
// empty (no copies of that card) or filled with up to four tokens (maximum copies of that card).
// Tokens are moved across the board in such a way that each successive state represents a unique
// deck combination.

open System
open System.Collections.Generic
open System.IO

type Board = Slot list
and Slot = { count:int; max:int }

let PrettyPrintBoard (board:Board) =
    board |> List.map (fun slot -> if slot.max = 0 then "" // ignore non-participating slots
                                   else (sprintf "%d " slot.count))
          |> List.reduce (+)

let InitialCombination (emptyBoard:Board) totalTokens : Board =
    // mutable reference represents the global token pool
    let numTokens = ref totalTokens

    // Distribute the tokens starting with the leftmost slots.
    let filledBoard =
        emptyBoard |> List.map (fun slot ->
            match !numTokens - slot.max with

            // We want more than is available. Take what is left.
            | leftover when leftover < 0 ->
                let allThatsLeft = !numTokens
                numTokens := 0
                { count = allThatsLeft; max = slot.max }

            | _ ->  // There's enough to fill the slot to the brim
                numTokens := !numTokens - slot.max
                { count = slot.max; max = slot.max })

    if !numTokens > 0 then raise (new ArgumentException "Too many tokens to fit in these slots")
    else filledBoard

type OverflowResult = SpilledOffEnd | ValidState

// My definition of the mancala sequence is iterative, so I need a datatype with mutable state
// to keep track of tokens as I slide them around.
type MutBoard = MutSlot list
and MutSlot =
    { mutable counter:int; max:int }
    // Convenience operators for manipulating the counter of a MutSlot
    static member (+=) (ms : MutSlot, a) = ms.counter <- ms.counter + a
    static member (-=) (ms : MutSlot, a) = ms.counter <- ms.counter - a
    static member (!=) (ms : MutSlot, a) = ms.counter <- a

let rec NextCombination (oldBoard:Board) : Board option =
    // Mutable copy of the deck
    let mutDeck = oldBoard |> List.map (fun slot -> { counter = slot.count; max = slot.max })
    let lastPos = oldBoard.Length - 1

    // The algorithm involves 'lifting' tokens up to move them around. This var represents the
    // number of tokens currently lifted. By the time we return (or recurse), lifted should be
    // empty so that we aren't losing any tokens.
    let lifted = ref 0

    // First lift all the tokens from the last slot
    lifted := mutDeck.[lastPos].counter
    mutDeck.[lastPos] != 0

    // Find the right-most token that isn't lifted
    match mutDeck |> Seq.toList
                  |> List.rev
                  |> List.tryFindIndex (fun slot -> slot.counter > 0) with

    // Can't find a token; no more legal deck positions; sequence complete.
    | None -> None

    // Slide tokens into next combination
    | Some indexFromBack ->
        // Position of the right-most token
        let pos = ref ((oldBoard |> Seq.length) - 1 - indexFromBack)  // invert, since we searched
                                                                      // while reversed

        mutDeck.[!pos] -= 1              // pick up a single token
        pos := !pos + 1                  // move one slot to the right
        mutDeck.[!pos] += (1 + !lifted)  // drop all tokens
        lifted := 0

        // Recursively "overflow" tokens to the right until each slots is within its maximum.
        let rec overflow () =
            if (mutDeck.[!pos].counter <= mutDeck.[!pos].max)
            then ValidState
            else  // We're on the board, but the number of tokens is over the maximum and
                  // we need to spill into the next slot.

                // Lift extra tokens
                lifted := mutDeck.[!pos].counter - mutDeck.[!pos].max
                mutDeck.[!pos] != mutDeck.[!pos].max

                // Try to deposit them one slot to the right
                pos := !pos + 1
                if !pos <= lastPos
                then
                    // Okay! Deposit tokens.
                    mutDeck.[!pos] != !lifted
                    lifted := 0

                    // Recurse, in case this slot also needs to overflow.
                    overflow ()

                else
                    // No more slots! Position is off the board. Deposit unaccounted-for
                    // tokens in the last slot.
                    mutDeck.[lastPos] += !lifted
                    lifted := 0
                    SpilledOffEnd

        match overflow () with
        // We have a legal deck position! Convert back to immutable form.
        | ValidState -> Some (mutDeck |> List.map (fun mutSlot -> { count = mutSlot.counter
                                                                    max = mutSlot.max }))
        // Overflowed! Speed ahead to the next valid combination
        | SpilledOffEnd ->
            // Pick up all tokens on the cusp of overflow, starting from the far right and
            // iterating to the left.
            pos := lastPos
            while mutDeck.[!pos].counter >= mutDeck.[!pos].max do
                // Pick up tokens
                lifted := !lifted + mutDeck.[!pos].counter
                mutDeck.[!pos] != 0

                // Move one slot to the left
                pos := !pos - 1

            // Deposit all overflowed tokens at the end
            mutDeck.[lastPos] += !lifted
            lifted := 0

            // This combination isn't legal, but we can find the next legal combo
            // by recursing on it.
            NextCombination (mutDeck |> List.map (fun mutSlot -> { count = mutSlot.counter
                                                                   max = mutSlot.max }))


// Given a board, iterate through all possible combinations
let MancalaSequence emptyBoard numTokens =
    // #region Debug
    let prevElapsedSeconds = ref 0.
    let chunkSize = 1000000
    let total = ref 0
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let outFile = new StreamWriter("output-mtg.txt")
    let myprint (str:string) =
        outFile.WriteLine(str)
        printfn "%s" str

    myprint (sprintf "Started at %s" (DateTime.Now.ToString()))
    // #endregion

    seq {
        let firstBoard = InitialCombination emptyBoard numTokens
        yield firstBoard
        let currBoard = ref firstBoard
        total := !total + 1

        let doneIterating = ref false
        while not !doneIterating do
            match (NextCombination !currBoard) with
            | Some newBoard ->

                // #region Debug
                total := !total + 1
                if (!total % chunkSize) = 0 then
                    let elapsedSeconds = stopWatch.Elapsed.TotalSeconds
                    let averageSecondsPerChunk = elapsedSeconds / (float (!total / chunkSize))
                    myprint (sprintf "%s#%9d, %5.2f sec/chunk, %8.3f total sec, %5.3f since last"
                                     (PrettyPrintBoard newBoard) !total averageSecondsPerChunk
                                     elapsedSeconds (elapsedSeconds - !prevElapsedSeconds))
                    prevElapsedSeconds := elapsedSeconds
                ()  // useful breakpoint location
                // #endregion

                yield newBoard
                currBoard := newBoard

            | None ->
                // #region Debug
                stopWatch.Stop()
                myprint (sprintf "Finished at %s" (DateTime.Now.ToString()))
                myprint (sprintf "Final total = %d" !total)
                myprint (sprintf "Elapsed time: %f total minutes" stopWatch.Elapsed.TotalMinutes)
                myprint (sprintf "              %f total seconds" stopWatch.Elapsed.TotalSeconds)
                outFile.Close()
                // #endregion
                doneIterating := true
    }
