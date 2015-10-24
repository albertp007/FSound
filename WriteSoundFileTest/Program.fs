// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

// Old way using sound file

open FSound.Signal
open FSound.Utilities
open FSound.IO

[<EntryPoint>]
let main argv = 
    let duration = 10.0
    let path = @"C:\Users\panga\project\FSound\profile1.wav"
    // [triangle 10000.0 440.0 |> generate 44100.0 duration] |> play2 44100 2
    // System.Threading.Thread.Sleep(int duration*1000)
    triangle 10000.0 440.0 |> wavCd1 duration path
    0