//
// FSound - F# Sound Processing Library
// Copyright (c) 2016 by Albert Pang <albert.pang@me.com> 
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
namespace FSound

module Tests = 
  open FSound.Signal
  open FSound.IO
  open FSound.Filter
  open FSound.Utilities
  open FSound.Play
  open FSound.Plotly
  open System
  
  Environment.SetEnvironmentVariable
    ("Path", 
     Environment.GetEnvironmentVariable("Path") + ";" + __SOURCE_DIRECTORY__ 
     + @"\packages\NAudio.Lame.1.0.3\content")
  
  let testWaveform waveformGen path = 
    [ waveformGen ] |> streamToWav 44100 2 path
  let testSinusoid() = 
    testWaveform (sinusoidGenerator 20000.0 440.0 0.0 44100.0 2.0) 
      @"samples\sinusoid-440.wav"
  let testSquare() = 
    testWaveform (squareGenerator 20000.0 440.0 44100.0 2.0) 
      @"samples\square-440.wav"
  let testSaw() = 
    testWaveform (sawGenerator 20000.0 440.0 44100.0 2.0) @"samples\saw-440.wav"
  let testTriangle() = 
    testWaveform (triangleGenerator 20000.0 440.0 44100.0 2.0) 
      @"samples\triangle-440.wav"
  let testNoise() = 
    testWaveform (whiteNoiseGenerator 20000.0 44100.0 2.0) 
      @"samples\whitenoise.wav"
  let testWave() = testWaveform (waveGenerator 44100.0 40.0) @"samples\wave.wav"
  
  let testAdsr() = 
    let signal = triangle 20000.0 2000.0
    let adsr = (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
    testWaveform (modulate signal adsr |> generate 44100.0 2.0) 
      @"samples\triangle-adsr.wav"
  
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
    testWaveform wf @"samples\sum.wav"
  
  let testRead() = 
    let gen = sawGenerator 20000.0 440.0 44100.0 2.0
    let w1 = SoundFile(44100.0, 2, true, (Mono gen))
    let fileName = @"samples\temp_square.wav"
    w1.WriteWav fileName
    let w2 = SoundFile.ReadWav fileName
    let (Mono m2) = w2.Samples
    let m1 = Seq.map (fun s -> float (int s)) gen
    (m1 |> Seq.toArray) = (m2 |> Seq.toArray)
  
  let testClip() = 
    testWaveform (sinusoid 20000.0 256.0 0.0
                  >> clipper 16000.0
                  |> generate 44100.0 5.0) @"samples\clip.wav"
  
  let playWave sf t filename waveFunc = 
    let sleep (tau : int) = System.Threading.Thread.Sleep tau
    FSound.Utilities.playWave sf t waveFunc
    // sleep (((int t) + 1) * 1000)
    waveFunc
    |> List.map (generate sf t)
    |> streamToWav (int sf) 2 filename
    
  let generateSawAndStreamToWav() = 
    [ saw 10000.0 440.0 ]
    |> List.map (generate 44100.0 2.0)
    |> streamToWav 44100 2 @"samples\saw-440.wav"
  
  let generateStereoAndStreamToWav() = 
    [ triangle 10000.0 440.0
      whiteNoise 10000.0 ]
    |> List.map (generate 44100.0 2.0)
    |> streamToWav 44100 2 @"samples\saw-noise.wav"
  
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
    |> plotSpectrum 20000
  
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
    |> streamToWav 44100 2 @"samples\square-chorus-adsr-delay.wav"
  
  let karplusStrong() = 
    [ pluck2LevelRandom 10000.0 44100.0 256.0 ] 
    |> playWave 44100.0 15.0 @"samples\karplusStrong.wav"

    [ pluck2LevelRandom 10000.0 44100.0 256.0 ]
    |> List.map (generate 44100.0 15.0)
    |> streamToWav 44100 2 @"samples\karplusStrong.wav"

    [ pluck2LevelRandom 10000.0 44100.0 256.0 >> vibrato 44100.0 7.0 2.0 ] 
    |> playWave 44100.0 15.0 @"samples\karplusStrongVibrato.wav"

    [ pluck2LevelRandom 10000.0 44100.0 256.0 >> vibrato 44100.0 7.0 2.0 ]
    |> List.map (generate 44100.0 15.0)
    |> streamToWav 44100 2 @"samples\karplusStrongVibrato.wav"

    [ pluck2LevelRandom 10000.0 44100.0 256.0 >> chorus 44100.0 30.0 0.5 2.0 ] 
    |> playWave 44100.0 15.0 @"samples\karplusStrongChorus.wav"

    [ pluck2LevelRandom 10000.0 44100.0 256.0 >> chorus 44100.0 30.0 0.5 2.0 ]
    |> List.map (generate 44100.0 15.0)
    |> streamToWav 44100 2 @"samples\karplusStrongChorus.wav"

    [ pluck2LevelRandom 10000.0 44100.0 256.0 >> flanger 44100.0 7.0 0.5 0.5 0.2 ] 
    |> playWave 44100.0 15.0 @"samples\karplusStrongFlanger.wav"

    [ pluck2LevelRandom 10000.0 44100.0 256.0 >> flanger 44100.0 7.0 0.5 0.5 0.2 ]
    |> List.map (generate 44100.0 15.0)
    |> streamToWav 44100 2 @"samples\karplusStrongFlanger.wav"
  
  let cMajor7() = 
    let gen pitch = 
      modulate (triangle 15000.0 pitch) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
    let (c, e, g, b) = (gen 256.0, gen 320.0, gen 384.0, gen 480.0)
    [ arrange [ (0.0, c)
                (0.0, e)
                (0.0, g)
                (0.0, b) ] ]
    |> playWave 44100.0 1.0 @"samples\cmajor7.wav"
  
  let pingPong() = 
    let piece() = 
      let i p = modulate (triangle 10000.0 p) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
      multiplex (i 256.0) (i 384.0)
      >> pingpong 44100.0 2.0 200.0 1.0 0.9 0.5 0
      |> generate 44100.0 15.0
    piece() |> FSound.Play.playStereo 44100 2
    piece() |> streamPairsToWav 44100 2 @"samples\pingpong.wav"
  
  let convertWavToMp3 directory = 
    let makeMp3Path wavPath = 
      System.IO.Path.GetDirectoryName(wavPath) + @"\" 
      + System.IO.Path.GetFileNameWithoutExtension(wavPath) + @".mp3"
    System.IO.Directory.GetFiles(directory, @"*.wav") 
    |> Array.iter (fun input -> wavToMp3 input (makeMp3Path input))
  
  let cMajor7FMajor7pingpong() = 
    let gen pitch = 
      modulate (triangle 15000.0 pitch) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
    let (c, d, e, f, g, a, b) = 
      (gen 256.0, gen 288.0, gen 320.0, gen 341.33, gen 384.0, gen 440.0, 
       gen 480.0)
    
    let piece() = 
      multiplex (arrange [ (0.0, c)
                           (0.0, b)
                           (4.0, f)
                           (4.0, a) ]) (arrange [ (0.0, e)
                                                  (0.0, g)
                                                  (4.0, c)
                                                  (4.0, e) ])
      >> pingpong 44100.0 2.0 200.0 1.0 0.9 0.5 0
      |> generate 44100.0 15.0
    piece() |> FSound.Play.playStereo 44100 2
    piece() |> streamPairsToWav 44100 2 @"samples\progression.wav"
  
  let ringingTone() = 
    // This is based on Andy's Farnell book "Designing Audio" Ch. 25 Phone Tones
    let level = 10000.0
    let fs = 44100.0
    
    let tone() = 
      sum [ osc fs level 440.0
            osc fs level 480.0 ]
    
    let telephoneLine() = 
      clipper level
      >> bp fs 2000.0 12.0
      >> split ((*) 0.5 >> bp fs 400.0 3.0) (clipper (0.4 * level) >> (*) 0.15)
      >> combine
      >> hp fs 90.0
    
    [ beep (tone() >> telephoneLine()) 2.0 4.0 ] 
    |> playWave fs 20.0 @"samples\ringing.wav"
  
  let schroederReverb() = 
    let i = modulate (triangle 10000.0 440.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
    let reverb() = 
      schroeder 44100.0 1.0 (101.0, 143.0, 165.0, 177.0) 
        (0.4, 0.37, 0.3333, 0.3)
    
    let output() = 
      i >> reverb()
      |> generate 44100.0 2.0
   
    output() |> FSound.Play.playStereo 44100 2
    output() |> streamPairsToWav 44100 2 @"samples\schroeder.wav"

  let ringModulation() =
    [triangle 10000.0 320.0 |> ring 1.0 440.0]
    |> playWave 44100.0 1.0 @"samples\ringMod.wav"

  let fmModulation() =
    [fm (Const 10000.0) 256.0 66.0 1.5 ]
    |> playWave 44100.0 1.0 @"samples\fm.wav"

  let fmBass() =
    [fm (Const 20000.0) 128.0 66.0 2.0 
    |> modulate (adsr 0.05 1.0 0.2 0.1 0.0 0.0)]
    |> playWave 44100.0 1.0 @"samples\fmBass.wav"

  let blowingEverHarder() =
    let modSinusoid1 (modA : Mod) f fm depth = 
      let pi = System.Math.PI
      fun t -> 
        let a = modA.GetValue t
        a * sin (2.0*pi*(f + depth*sin(2.0*pi*fm*t))*t)
    [modSinusoid1 (Const 1000.0) 256.0 0.5 20.0] 
    |> playWave 44100.0 14.5 @"samples\blowingEverHarder.wav"

  let chowningBrass() =
    let dur = 1.0
    [brass 20000.0 512.0 dur]
    |> playWave 44100.0 dur @"samples\chowningBrass.wav"

  let chowningBell() =
    [bell 10000.0 300.0]
    |> playWave 44100.0 15.0 @"samples\chowningBell.wav"

  let chowningGong() =
    [bell 10000.0 100.0]
    |> playWave 44100.0 15.0 @"samples\chowningGong.wav"

  let plotFreqResponse() =
    let ff = [0.206572083826148; 0.413144167652296; 0.206572083826148]
    let fb = [-0.369527377351241; 0.195815712655833]
    plotMagnitude 10000.0 ff fb 5000
    plotPhase 10000.0 ff fb 5000
    
  
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
    plotFreqResponse()
    triangleVibrato()
    sawFlanger()
    noiseFlanger()
    sawChorusAdsrDelay()
    typoSawChorusAdsrDelay()
    streamToWavTest()
    karplusStrong()
    cMajor7()
    pingPong()
    cMajor7FMajor7pingpong()
    ringingTone()
    schroederReverb()
    ringModulation()
    fmModulation()
    fmBass()
    blowingEverHarder()
    chowningBrass()
    chowningBell()
    chowningGong()
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
