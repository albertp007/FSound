// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I @"packages\FSharp.Charting.0.90.12\lib\net40"
#I @"packages\MathNet.Numerics.3.7.0\lib\net40"
#r @"FSharp.Charting.dll"
#r @"MathNet.Numerics.dll"
#load "Signal.fs"
#load "IO.fs"

open FSound.Signal
open FSound.IO
open FSharp.Charting

let seqToPoint s =
  s |> Seq.mapi (fun i x -> (i,x))

// Define your library scripting code here
let testSinewave =
  let start = 0.0
  let endTime = 0.01
  let sf = 44100.0
  let s = sinewave 0.8 440.0 0.0 44100.0 0.0 2.0
  let i = [start..(1.0/sf)..endTime]
  Seq.zip i s |> Chart.Line

let testReadWavFile =
  let wav = readWavFile @"chimes.wav"
  fst wav.Data 
  |> fft 
  |> Array.mapi (fun i x -> (i, x)) 
  |> Chart.Point