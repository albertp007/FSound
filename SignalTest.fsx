//
// FSound - F# Sound Processing Library
// Copyright (c) 2015 by Albert Pang <albert.pang@me.com> 
// All rights reserved.
//
// This file is a part of FSound
//
// FSound is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// FSound is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
#I @"bin/Debug"
#r @"FSound.dll"

open FSound.Signal
open FSound.IO

// Define your library scripting code here
let testWaveform waveformGen path=
  waveformGen
  |> floatTo16
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

let testRead() =
  let w1 = squareGenerator 20000.0 440.0 44100.0 2.0
           |> floatTo16
           |> makeSoundFile 44100.0 1 16 true
  let fileName = @"temp_square.wav"
  w1.WriteWav fileName
  let w2 = SoundFile.ReadWav fileName
  ( w1.Channels.[0] |> Seq.toArray ) = (w1.Channels.[0] |> Seq.toArray )

let test() =
  testSinusoid()
  testSquare()
  testSaw()
  testTriangle()
  testNoise()
  testWave()

