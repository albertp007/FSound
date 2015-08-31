// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I @"bin/Debug"
#r @"FSound.dll"

open FSound.Signal
open FSound.IO

// Define your library scripting code here
let testWaveform waveformGen path=
  waveformGen
  |> floatTo16
  |> Seq.toArray
  |> makeSoundFile 44100.0 1 16 true
  |> toWav path

let testSinusoid() =
  testWaveform (sinusoidGenerator 20000.0 440.0 0.0 44100.0 2.0) 
    @"sinusoid-440.wav"

let testSquare() =
  testWaveform (squareGenerator 20000.0 440.0 44100.0 2.0) @"square-440.wav"

let testSaw() = 
  testWaveform (sawGenerator 20000.0 440.0 44100.0 2.0) @"saw-440.wav"

let testTriangle() =
  testWaveform (triangleGenerator 20000.0 440.0 44100.0 2.0) @"triangle-440.wav"

let testNoise() =
  testWaveform (whiteNoiseGenerator 20000.0 44100.0 2.0) @"whitenoise.wav"

let testWave() =
  testWaveform (waveGenerator 20000.0 0.05 44100.0 10.0) @"wave.wav"

let test() =
  testSinusoid()
  testSquare()
  testSaw()
  testTriangle()
  testNoise()
  testWave()

