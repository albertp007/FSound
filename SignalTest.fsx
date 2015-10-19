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
open FSound.Filter
open FSound.Utilities

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
  testWaveform (waveGenerator 44100.0 40.0) @"wave.wav"

let testAdsr() =
  let signal = triangle 20000.0 2000.0
  let adsr = (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  testWaveform (modulate signal adsr |> generate 44100.0 2.0) 
    @"triangle-adsr.wav"

let testFilter() =
  let r1 = List.map (filter [1.0; 0.0; 0.0; 0.5**3.0] 
                           [0.0; 0.0; 0.0; 0.0; 0.9**5.0]) 
                   (1.0::(List.init 29 (fun _ -> 0.0)))
  let r2 = [1.0; 0.0; 0.0; 0.125; 0.0; -0.59049; 0.0; 0.0; -0.07381125; 0.0;
    0.3486784401; 0.0; 0.0; 0.04358480501; 0.0; -0.2058911321; 0.0; 0.0;
    -0.02573639151; 0.0; 0.1215766546; 0.0; 0.0; 0.01519708182; 0.0;
    -0.07178979877; 0.0; 0.0; -0.008973724846; 0.0]
  List.map2 (fun x y -> abs (x - y)) r1 r2
  |> List.forall (fun x -> x < 0.000000001)
  |> ( fun x -> if x then x else failwith "testFilter failed")

let testSum() =
  let wf = sum [sinusoid 8000.0 10.0 0.0; 
                sinusoid 7200.0 25.0 0.0; 
                sinusoid 6400.0 50.0 0.0] 
           |> generate 44100.0 10.0
  testWaveform wf @"sum.wav"

let testRead() =
  let w1 = squareGenerator 20000.0 440.0 44100.0 2.0
           |> floatTo16
           |> makeSoundFile 44100.0 1 16 true
  let fileName = @"temp_square.wav"
  w1.WriteWav fileName
  let w2 = SoundFile.ReadWav fileName
  ( w1.Channels.[0] |> Seq.toArray ) = (w1.Channels.[0] |> Seq.toArray )

let testClip() =
  testWaveform (sinusoid 20000.0 256.0 0.0 >> clip 16000.0 
                |> generate 44100.0 5.0) @"clip.wav"

let funny() =
  let adsr1 = adsr 0.05 1.0 0.05 0.3 0.1 0.05
  let sleep (tau:int) = System.Threading.Thread.Sleep tau
  let playDuration tau (waveFunc:float->float) = playWave 44100.0 tau waveFunc
                                                 sleep (((int tau) + 1) * 1000)
  let play = playDuration 2.0
  
  // a triangle wave with an adsr
  modulate (triangle 20000.0 2000.0) adsr1 |> play

  // saw wave with smith angell resonator at 1024
  saw 20000.0 440.0
  >> smithAngell 44100.0 1024.0 10.0
  |> play

  // noise with resonator
  whiteNoise 50000.0
  >> smithAngell 44100.0 880.0 10.0
  |> play

  // noise with resonator + adsr - the sound of, umm, hitting air?
  modulate (whiteNoise 50000.0) adsr1
  >> smithAngell 44100.0 880.0 10.0
  |> play

  // sound of waves using a low pass filter at 200Hz
  modulate (whiteNoise 50000.0) (lfo 0.05 0.0 0.8) 
  >> lp 44100.0 220.0
  |> playDuration 50.0

  // Vanilla delay effect
  modulate (triangle 20000.0 2000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05) 
  >> delay 44100.0 2.0 200.0 0.15 0.4
  |> playDuration 2.0

  /// Chorus effect - the delay params taken from William Sharkey's Interior
  /// Sounds [https://github.com/williamsharkey/William-FSound-Songs#1-interior-sounds---click-to-play]
  modulate (square 10000.0 440.0 >> chorus 44100.0 30.0 0.4 1.5) 
    (adsr 0.05 1.0 0.05 0.3 0.1 0.05) 
  >> delay 44100.0 2.0 200.0 0.9 0.5
  |> playDuration 10.0

  /// Same as the chorus effect above but with a typo in the sampling frequency
  /// in the delay and it turns out to sound completely different
  modulate (square 10000.0 440.0 >> chorus 44100.0 30.0 0.4 1.5) 
    (adsr 0.05 1.0 0.05 0.3 0.1 0.05) 
  >> delay 4410.0 2.0 200.0 0.9 0.5
  |> playDuration 2.0

let test() =
  testSinusoid()
  testSquare()
  testSaw()
  testTriangle()
  testNoise()
  testWave()
  testAdsr()
  testFilter()

