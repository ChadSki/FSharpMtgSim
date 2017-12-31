module Logging

open System
open System.IO

let LogName n = sprintf "belcher-log-%d.txt" n
let mutable logNumber = 0
while File.Exists(LogName logNumber) do
    logNumber <- logNumber + 1

let logFile = new StreamWriter(LogName logNumber)

let logAgent = MailboxProcessor.Start(fun inbox -> 
        let rec messageLoop() = async {
            let! (msg:string) = inbox.Receive()
            logFile.WriteLine(msg)
            printfn "%s" msg
            return! messageLoop()
            }
        messageLoop())

let log (str:string) =
    logAgent.Post str

log (sprintf "Opened logfile '%s'" (LogName logNumber))

let closeLog () =
    logFile.Flush()
    logFile.Close()
