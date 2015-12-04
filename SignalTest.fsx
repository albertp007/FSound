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
#I "bin/Debug"
#r "FSound.dll"

open FSound.Signal
open FSound.IO
open FSound.Filter
open FSound.Utilities
open FSound.Play
open FSound.Plot
open System

Environment.SetEnvironmentVariable
  ("Path", 
   Environment.GetEnvironmentVariable("Path") + ";" + __SOURCE_DIRECTORY__ 
   + @"\packages\NAudio.Lame.1.0.3\content")

// Define your library scripting code here
let testWaveform waveformGen path = [ waveformGen ] |> streamToWav 44100 2 path
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
let testWave() = testWaveform (waveGenerator 44100.0 40.0) @"wave.wav"

let testAdsr() = 
  let signal = triangle 20000.0 2000.0
  let adsr = (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  testWaveform (modulate signal adsr |> generate 44100.0 2.0) 
    @"triangle-adsr.wav"

let testFilter() = 
  let r1 = 
    List.map (filter [ 1.0
                       0.0
                       0.0
                       0.5 ** 3.0 ] [ 0.0
                                      0.0
                                      0.0
                                      0.0
                                      0.9 ** 5.0 ]) 
      (1.0 :: (List.init 29 (fun _ -> 0.0)))
  
  let r2 = 
    [ 1.0; 0.0; 0.0; 0.125; 0.0; -0.59049; 0.0; 0.0; -0.07381125; 0.0; 
      0.3486784401; 0.0; 0.0; 0.04358480501; 0.0; -0.2058911321; 0.0; 0.0; 
      -0.02573639151; 0.0; 0.1215766546; 0.0; 0.0; 0.01519708182; 0.0; 
      -0.07178979877; 0.0; 0.0; -0.008973724846; 0.0 ]
  List.map2 (fun x y -> abs (x - y)) r1 r2
  |> List.forall (fun x -> x < 0.000000001)
  |> (fun x -> 
  if x then x
  else failwith "testFilter failed")

let testSum() = 
  let wf = 
    sum [ sinusoid 8000.0 10.0 0.0
          sinusoid 7200.0 25.0 0.0
          sinusoid 6400.0 50.0 0.0 ]
    |> generate 44100.0 10.0
  testWaveform wf @"sum.wav"

let testRead() = 
  let gen = sawGenerator 20000.0 440.0 44100.0 2.0
  let w1 = SoundFile(44100.0, 2, true, (Mono gen))
  let fileName = @"temp_square.wav"
  w1.WriteWav fileName
  // read the file back in
  let w2 = SoundFile.ReadWav fileName
  let (Mono m2) = w2.Samples
  let m1 = Seq.map (fun s -> float (int s)) gen
  (m1 |> Seq.toArray) = (m2 |> Seq.toArray)

let testClip() = 
  testWaveform (sinusoid 20000.0 256.0 0.0
                >> clipper 16000.0
                |> generate 44100.0 5.0) @"clip.wav"

let playWave sf t filename waveFunc = 
  let sleep (tau : int) = System.Threading.Thread.Sleep tau
  FSound.Utilities.playWave sf t waveFunc
  sleep (((int t) + 1) * 1000)
  waveFunc
  |> List.map (generate sf t)
  |> streamToWav (int sf) 2 filename

let funny() = 
  let adsr1 = adsr 0.05 1.0 0.05 0.3 0.1 0.05
  let sleep (tau : int) = System.Threading.Thread.Sleep tau
  
  let playDuration tau (waveFunc : float -> float) = 
    FSound.Utilities.playWave 44100.0 tau [ waveFunc ]
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
  >> delay 44100.0 2.0 200.0 1.0 0.15 0.4
  |> playDuration 2.0
  /// Chorus effect - the delay params taken from William Sharkey's Interior
  /// Sounds [https://github.com/williamsharkey/William-FSound-Songs#1-interior-sounds---click-to-play]
  modulate (square 10000.0 440.0 >> chorus 44100.0 30.0 0.4 1.5) 
    (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  >> delay 44100.0 2.0 200.0 1.0 0.9 0.5
  |> playDuration 10.0
  /// Same as the chorus effect above but with a typo in the sampling frequency
  /// in the delay and it turns out to sound completely different
  modulate (square 10000.0 440.0 >> chorus 44100.0 30.0 0.4 1.5) 
    (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  >> delay 4410.0 2.0 200.0 1.0 0.9 0.5
  |> playDuration 2.0

let generateSawAndStreamToWav() = 
  [ saw 10000.0 440.0 ]
  |> List.map (generate 44100.0 2.0)
  |> streamToWav 44100 2 @"C:\Users\panga\project\FSound\saw-440.wav"

let generateStereoAndStreamToWav() = 
  [ triangle 10000.0 440.0
    whiteNoise 10000.0 ]
  |> List.map (generate 44100.0 2.0)
  |> streamToWav 44100 2 @"C:\Users\panga\project\FSound\saw-noise.wav"

let playTriangle() = 
  [ triangle 10000.0 440.0 ] |> playWave 44100.0 2.0 @"samples\triangle.wav"

let playStereo() = 
  [ triangle 10000.0 440.0
    whiteNoise 10000.0 ]
  |> playWave 44100.0 2.0 @"samples\stereo.wav"

let modulateWithAdsr() = 
  [ modulate (triangle 20000.0 2000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05) ] 
  |> playWave 44100.0 1.0 @"samples\modulateWithAdsr.wav"
let modulateTriangleAdsrDelay() = 
  [ modulate (triangle 20000.0 2000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05) 
    >> delay 44100.0 2.0 200.0 1.0 0.15 0.5 ] 
  |> playWave 44100.0 1.0 @"samples\modulateTriangleAdsrDelay.wav"
let modulateNoiseAdsrDelay() = 
  [ modulate (whiteNoise 20000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05) 
    >> delay 44100.0 2.0 200.0 1.0 0.15 0.5 ] 
  |> playWave 44100.0 1.0 @"samples\modulateNoiseAdsrDelay.wav"
let noiseSmithAngell() = 
  [ whiteNoise 50000.0 >> smithAngell 44100.0 440.0 10.0 ] 
  |> playWave 44100.0 2.0 @"samples\noiseSmithAngell.wav"
let noiseSmithAngellAdsr() = 
  [ modulate (whiteNoise 50000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05) 
    >> smithAngell 44100.0 880.0 10.0 ] 
  |> playWave 44100.0 2.0 @"samples\noiseSmithAngellAdsr.wav"
let noiseHp() = 
  [ whiteNoise 10000.0 >> hp 44100.0 10000.0 ] 
  |> playWave 44100.0 1.0 @"samples\noiseHp.wav"
let noiseLfo() = 
  [ modulate (whiteNoise 10000.0) (lfo 0.05 0.0 0.8) >> lp 44100.0 220.0 ] 
  |> playWave 44100.0 50.0 @"samples\noiseLfo.wav"

let noiseLpPlot() = 
  whiteNoise 10000.0
  >> lp 44100.0 220.0
  |> generate 44100.0 1.0
  |> plotFreq 20000

let triangleVibrato() = 
  [ triangle 10000.0 440.0 >> vibrato 44100.0 7.0 2.0 ] 
  |> playWave 44100.0 5.0 @"samples\triangleVibrato.wav"
let sawFlanger() = 
  [ saw 10000.0 440.0 >> flanger 44100.0 7.0 0.15 0.5 0.2 ] 
  |> playWave 44100.0 10.0 @"samples\sawFlanger.wav"
let noiseFlanger() = 
  [ whiteNoise 10000.0 >> flanger 44100.0 7.0 0.15 0.5 0.2 ] 
  |> playWave 44100.0 10.0 @"samples\noiseFlanger.wav"
let sawChorusAdsrDelay() = 
  [ modulate (square 10000.0 440.0 >> chorus 44100.0 30.0 0.4 1.5) 
      (adsr 0.05 1.0 0.05 0.3 0.1 0.05) >> delay 44100.0 2.0 200.0 1.0 0.9 0.5 ] 
  |> playWave 44100.0 10.0 @"samples\sawChorusAdsrDelay.wav"
let typoSawChorusAdsrDelay() = 
  [ modulate (square 10000.0 440.0 >> chorus 44100.0 30.0 0.4 1.5) 
      (adsr 0.05 1.0 0.05 0.3 0.1 0.05) >> delay 4410.0 2.0 200.0 1.0 0.9 0.5 ] 
  |> playWave 44100.0 2.0 @"samples\typoSawChorusAdsrDelay.wav"

let streamToWavTest() = 
  [ modulate (square 10000.0 440.0 >> chorus 44100.0 30.0 0.4 1.5) 
      (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
    >> delay 4410.0 2.0 200.0 1.0 0.9 0.5
    |> generate 44100.0 2.0 ]
  |> streamToWav 44100 2 
       @"C:\Users\panga\project\FSound\square-chorus-adsr-delay.wav"

let karplusStrong() = 
  [ pluck2LevelRandom 10000.0 44100.0 256.0 ] 
  |> playWave 44100.0 5.0 @"samples\karplusStrong.wav"
  [ pluck2LevelRandom 10000.0 44100.0 256.0 ]
  |> List.map (generate 44100.0 5.0)
  |> streamToWav 44100 2 @"samples\karplusStrong.wav"
  [ pluck2LevelRandom 10000.0 44100.0 256.0 >> vibrato 44100.0 7.0 2.0 ] 
  |> playWave 44100.0 5.0 @"samples\karplusStrongVibrato.wav"
  [ pluck2LevelRandom 10000.0 44100.0 256.0 >> vibrato 44100.0 7.0 2.0 ]
  |> List.map (generate 44100.0 5.0)
  |> streamToWav 44100 2 @"samples\karplusStrongVibrato.wav"
  [ pluck2LevelRandom 10000.0 44100.0 256.0 >> chorus 44100.0 30.0 0.5 2.0 ] 
  |> playWave 44100.0 5.0 @"samples\karplusStrongChorus.wav"
  [ pluck2LevelRandom 10000.0 44100.0 256.0 >> chorus 44100.0 30.0 0.5 2.0 ]
  |> List.map (generate 44100.0 5.0)
  |> streamToWav 44100 2 @"samples\karplusStrongChorus.wav"
  [ pluck2LevelRandom 10000.0 44100.0 256.0 >> flanger 44100.0 7.0 0.5 0.5 0.2 ] 
  |> playWave 44100.0 5.0 @"samples\karplusStrongFlanger.wav"
  [ pluck2LevelRandom 10000.0 44100.0 256.0 >> flanger 44100.0 7.0 0.5 0.5 0.2 ]
  |> List.map (generate 44100.0 5.0)
  |> streamToWav 44100 2 @"samples\karplusStrongFlanger.wav"

let cMajor7() = 
  let gen pitch = 
    modulate (triangle 15000.0 pitch) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  // natural tuning
  let (c, e, g, b) = (gen 256.0, gen 320.0, gen 384.0, gen 480.0)
  [ arrange [ (0.0, c)
              (0.0, e)
              (0.0, g)
              (0.0, b) ] ]
  |> playWave 44100.0 1.0 @"samples\cmajor7.wav"

let pingPong() =
  let i p = modulate (triangle 10000.0 p) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  multiplex (i 256.0) (i 384.0) >> pingpong 44100.0 2.0 200.0 1.0 0.9 0.5 
  |> generate 44100.0 5.0 
  |> demultiplex 
  |> streamToWav 44100 2 @"samples\pingpong.wav";;

let convertWavToMp3 directory = 
  let makeMp3Path wavPath = 
    System.IO.Path.GetDirectoryName(wavPath) + @"\" 
    + System.IO.Path.GetFileNameWithoutExtension(wavPath) + @".mp3"
  System.IO.Directory.GetFiles(directory, @"*.wav") 
  |> Array.iter (fun input -> wavToMp3 input (makeMp3Path input))

let readmeExamples() = 
  generateSawAndStreamToWav()
  generateStereoAndStreamToWav()
  playTriangle()
  playStereo()
  modulateWithAdsr()
  modulateTriangleAdsrDelay()
  modulateNoiseAdsrDelay()
  noiseSmithAngell()
  noiseSmithAngellAdsr()
  noiseHp()
  noiseLfo()
  noiseLpPlot() |> ignore
  triangleVibrato()
  sawFlanger()
  noiseFlanger()
  sawChorusAdsrDelay()
  typoSawChorusAdsrDelay()
  streamToWavTest()
  karplusStrong()
  cMajor7()
  pingPong()
  convertWavToMp3 (__SOURCE_DIRECTORY__ + @"\samples")

let test() = 
  testSinusoid()
  testSquare()
  testSaw()
  testTriangle()
  testNoise()
  testWave()
  testAdsr()
  readmeExamples()
  if not (testRead()) then failwith "testRead() failed"
  testFilter()
