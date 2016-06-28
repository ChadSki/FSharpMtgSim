﻿module Logging

open System
open System.IO

let LogName n = sprintf "belcher-log-%d.txt" n
let mutable logNumber = 0
while File.Exists(LogName logNumber) do
    logNumber <- logNumber + 1

let logFile = new StreamWriter(LogName logNumber)

let log (str:string) =
    logFile.WriteLine(str)
    printfn "%s" str

log (sprintf "Opened logfile '%s'" (LogName logNumber))

let closeLog () =
    logFile.Flush()
    logFile.Close()
