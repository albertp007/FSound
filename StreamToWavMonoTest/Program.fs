// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FSound.Play
open FSound.Signal
open FSound.IO

[<EntryPoint>]
let main argv = 
    let duration = 10.0
    let sf = 44100.0
    let path = @"C:\Users\panga\project\FSound\profileMono.wav"
    triangle 10000.0 440.0 
    |> generate sf duration
    |> streamToWavMono (int sf) 2 path
    0 // return an integer exit code
