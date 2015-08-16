// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I @"packages\FSharp.Charting.0.90.12\lib\net40"
#r @"FSharp.Charting.dll"
#load "Signal.fs"

namespace FSound

module SignalTest =

  open FSound.Signal
  open FSharp.Charting

  // Define your library scripting code here
  let testSinewave =
    let start = 0.0
    let endTime = 0.01
    let sf = 44100.0
    let s = sinewave 0.8 440.0 0.0 44100.0 0.0 2.0
    let i = [start..(1.0/sf)..endTime]
    Seq.zip i s |> Chart.Line

