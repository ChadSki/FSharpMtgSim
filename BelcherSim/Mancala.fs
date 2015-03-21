module Mancala

open System
open System.Collections.Generic
open System.IO

type Slot = { count:int; max:int }
type Board = Slot list

let PrettyPrintBoard (board:Board) =
    board |> List.map (fun slot -> if slot.max = 0 then "" // ignore non-participating slots
                                   else (sprintf "%d " slot.count))
          |> List.reduce (+)

let InitialCombination (emptyBoard:Board) totalBeads : Board =
    let numBeads = ref totalBeads

    // Fill the slots in sequence order (left to right)
    let filledBoard =
        emptyBoard |> List.map (fun slot ->
            let remainderIfGreedy = !numBeads - slot.max
            match remainderIfGreedy with

            // We want more than is available. Take what is left.
            | remainder when remainder < 0 -> let allThatsLeft = !numBeads
                                              numBeads := 0
                                              { count = allThatsLeft
                                                max = slot.max }

            // There's enough to take our fill!
            | _ -> numBeads := remainderIfGreedy
                   { count = slot.max
                     max = slot.max })

    if !numBeads > 0 then raise (new ArgumentException "Too many beads to fit in these slots")
    else filledBoard

type OverflowResult = SpilledOffEnd | ValidState

let rec NextCombination (oldBoard:Board) : Board option =
    // Mutable copy of the deck
    let mutDeck = new List<Slot>(oldBoard)
    let lastPos = oldBoard.Length - 1

    // The algorithm often involves 'lifting' beads up to move them around. This var represents the
    // number of beads currently lifted. By the time we return (or recurse), lifted should be empty
    // so that we aren't losing any beads.
    let lifted = ref 0

    // First lift all the beads from the last slot
    lifted := mutDeck.[lastPos].count
    mutDeck.[lastPos] <- { count = 0
                           max = mutDeck.[lastPos].max }

    // Find the right-most bead that isn't lifted
    match mutDeck |> Seq.toList
                  |> List.rev
                  |> List.tryFindIndex (fun slot -> slot.count > 0) with

    | None -> None  // No more legal deck positions; sequence complete
    | Some indexFromBack ->
        // Position of the right-most bead
        let pos = ref ((oldBoard |> Seq.length) - 1 - indexFromBack)  // invert, since we searched while reversed

        // Pick up a single bead, move to the right, and drop all beads
        mutDeck.[!pos] <- { count = mutDeck.[!pos].count - 1
                            max = mutDeck.[!pos].max }
        pos := !pos + 1
        mutDeck.[!pos] <- { count = mutDeck.[!pos].count + 1 + !lifted
                            max = mutDeck.[!pos].max }
        lifted := 0

        // Recursively "overflow" beads to the right until no slots hold more than their maximum.
        let rec overflow () =
            if !pos > lastPos then SpilledOffEnd
            else
                let curr = mutDeck.[!pos].count
                let ceiling = mutDeck.[!pos].max
                if (curr > ceiling || curr = ceiling && !lifted > 0) then
                    // Position is on the board, but the number of beads will be over the maximum
                    // unless we spill into the next slot.
                    lifted := !lifted + mutDeck.[!pos].count - ceiling
                    mutDeck.[!pos] <- { count = ceiling
                                        max = mutDeck.[!pos].max }

                    pos := !pos + 1
                    if !pos > lastPos then SpilledOffEnd
                    else
                        mutDeck.[!pos] <- { count = mutDeck.[!pos].count + !lifted
                                            max = mutDeck.[!pos].max }
                        lifted := 0
                        overflow ()
                else ValidState

        match overflow () with

        // We have a legal deck position
        | ValidState -> Some (mutDeck |> Seq.toList)

        // Overflowed! Speed ahead to the next valid combination
        | SpilledOffEnd ->

            // First, deposit beads at the end
            mutDeck.[lastPos] <- { count = mutDeck.[lastPos].count + !lifted
                                   max = mutDeck.[lastPos].max }
            lifted := 0

            // Pick up all beads on the cusp of overflow
            pos := lastPos
            while mutDeck.[!pos].count >= mutDeck.[!pos].max do
                lifted := !lifted + mutDeck.[!pos].count
                mutDeck.[!pos] <- { count = 0
                                    max = mutDeck.[!pos].max }
                pos := !pos - 1

            // Deposit them at the end, too
            mutDeck.[lastPos] <- { count = mutDeck.[lastPos].count + !lifted
                                   max = mutDeck.[lastPos].max }
            lifted := 0

            // This combination isn't legal, but we can find the next legal combo from it.
            NextCombination (mutDeck |> Seq.toList)


// Given a board, iterate through all possible combinations
let MancalaSequence emptyBoard numBeads =
    // #region Debug
    let prevElapsedSeconds = ref 0.
    let chunkSize = 100000
    let total = ref 0
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let outFile = new StreamWriter("output-mtg.txt")
    let myprint (str:string) =
        printfn "%s" str
        outFile.WriteLine(str)
        outFile.Flush()

    myprint (sprintf "Started at %s" (DateTime.Now.ToString()))
    // #endregion

    seq {
        let firstBoard = InitialCombination emptyBoard numBeads
        yield firstBoard
        let currBoard = ref firstBoard
        total := !total + 1

        // TODO Turn this tail-recursive
        let doneIterating = ref false
        while not !doneIterating do
            match (NextCombination !currBoard) with
            | Some board ->
                yield board
                currBoard := board

                // #region Debug
                total := !total + 1
                if (!total % chunkSize) = 0 then
                    let elapsedSeconds = stopWatch.Elapsed.TotalSeconds
                    let averageSecondsPerChunk = elapsedSeconds / (float (!total / chunkSize))
                    myprint (sprintf "%s#%9d, %5.2f sec/chunk, %8.3f total sec, %5.3f since last"
                                     (PrettyPrintBoard !currBoard) !total averageSecondsPerChunk
                                     elapsedSeconds (elapsedSeconds - !prevElapsedSeconds))
                    prevElapsedSeconds := elapsedSeconds
                ()
                // #endregion

            | None ->
                doneIterating := true

                // #region Debug
                stopWatch.Stop()
                myprint (sprintf "Finished at %s" (DateTime.Now.ToString()))
                myprint (sprintf "Final total = %d" !total)
                myprint (sprintf "Elapsed time: %f total minutes" stopWatch.Elapsed.TotalMinutes)
                myprint (sprintf "              %f total seconds" stopWatch.Elapsed.TotalSeconds)
                outFile.Close()
                // #endregion
    }
