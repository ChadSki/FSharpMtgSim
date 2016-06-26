module Logging

open System
open System.IO

let mutable logNumber = 0
while File.Exists(sprintf "belcher-log-%d.txt" logNumber) do
    logNumber <- logNumber + 1

// Never explicitly closed, trust that the OS closes it for us.
let logFile = new StreamWriter(sprintf "belcher-log-%d.txt" logNumber)

let log (str:string) =
    logFile.WriteLine(str)
    printfn "%s" str
