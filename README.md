## Synopsis

FSound is a library for processing sound using F#

## Code Example

You can run these examples in F# interactive in Visual Studio.  Make sure you 
build the project first.   Then in Solution Explorer, right click
on the FSound project and select "Send Project Output to F# Interactive".  This
will load the FSound project assembly into F# Interactive.  After that, simply
copy and paste the code over in each of the following code examples.

Here are some of the things you can do with FSound:

#### Generate mono wav file

* Generate a .wav file containing a single channel of sawtooth wave of amplitude 10000, 440Hz, 
sampling rate 44100Hz, 1 channel, 16-bit sample (2 bytes) and which lasts for 2 seconds
  
  ```
  open FSound.Signal;;
  open FSound.IO;;
  
  [saw 10000.0 440.0] 
  |> List.map (generate 44100.0 2.0) 
  |> streamToWav 44100 2 @"blah.wav";;
  ```

#### Generate stereo wav file

* Generate a .wav file containing a triangular wave in the left channel and white noise on the right channel

  ```
  open FSound.Signal;;
  open FSound.IO;;
  
  [triangle 10000.0 440.0; whiteNoise 10000.0]
  |> List.map (generate 44100.0 2.0)
  |> streamToWav 44100 2 @"blah.wav";;
  ```
  
#### Play a mono signal

* Play a single channel of triangular wave of amplitude 10000, 440Hz, sampling rate 44100Hz, 1 channel and which lasts for 2 seconds - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/triangle.mp3)
  
  ```
  open FSound.Signal;;
  open FSound.Utilities;;
  
  triangle 10000.0 440.0 |> playWave 44100.0 2.0;;
  ```

#### Play different signals in left and right channel

* Play a triangular wave in the left channel and white noise in the right channel. ('playWave' above is a convenience function which calls the more general 'play' function which plays an arbitrary of channels, to first generate a signal and then play it in a single channel) - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/stereo.mp3)

  ```
  open FSound.Signal;;
  open FSound.Play;;
  
  [triangle 10000.0 440.0; whiteNoise 10000.0]
  |> List.map (generate 44100.0 2.0)
  |> play 44100 2;;
  ```

#### ADSR envelope

* Add an ADSR envelope to the triangular wave above - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/modulateWithAdsr.mp3)
  
  ```
  open FSound.Signal;;
  open FSound.Utilities;;
  
  [modulate (triangle 20000.0 2000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)]
  |> playWave 44100.0 1.0;;
  ```

####  Delay with ADSR envelope on triangular waveform

* Add 200ms delay, 50/50 dry/wet and 0.15 feedback gain to the triangular wave with an ADSR envelope above - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/modulateTriangleAdsrDelay.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [modulate (triangle 20000.0 2000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  >> delay 44100.0 2.0 200.0 1.0 0.15 0.5]
  |> playWave 44100.0 1.0;;
  ```

#### Delay with ADSR envelope on white noise

* The same as right above, except replace triangular wave with white noise to produce the sound of shuffling something - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/modulateNoiseAdsrDelay.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [modulate (whiteNoise 20000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  >> delay 44100.0 2.0 200.0 1.0 0.15 0.5]
  |> playWave 44100.0 1.0;;
  ```

#### Smith-Angell resonator

* Shaping white noise with a Smith-Angell resonator - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/noiseSmithAngell.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [whiteNoise 50000.0 >> smithAngell 44100.0 440.0 10.0]
  |> playWave 44100.0 2.0;;
  ```

#### Smith-Angell resonator with an ADSR envelope

* Shaping white noise first with an ADSR envelope then a Smith-Angell resonator - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/noiseSmithAngellAdsr.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [modulate (whiteNoise 50000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  >> smithAngell 44100.0 880.0 10.0]
  |> playWave 44100.0 2.0;;
  ```

#### High-pass filter on white noise

* Generate white noise and pass through a high-pass filter - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/noiseHp.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [whiteNoise 10000.0 >> hp 44100.0 10000.0]
  |> playWave 44100.0 1.0;;
  ```
  
#### Low-pass filter on white noise with an LFO

* Modulate amplitude of white noise with an LFO and pass through a low-pass filter - A crude simulation to the sound of waves - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/noiseLfo.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [modulate (whiteNoise 10000.0) (lfo 0.05 0.0 0.8) >> lp 44100.0 220.0]
  |> playWave 44100.0 50.0;;
  ```
  
#### Plotting frequency response

* Plot the magnitude response of a biquad low-pass filter with a centre frequency of 1000Hz and Q equal to 1
  To run the following in F# interactive, you need to send the Newtonsoft.Json reference to F# interactive in addition to the FSound project reference

  ```
  open FSound.Filter;;
  open FSound.Plotly;;
  
  bqlpCoeff 44100.0 1000.0 1.0
  |> plotMagnitude 44100.0 5000.0;;
  ```

  ![alt text](https://cdn.rawgit.com/albertp007/FSound/master/samples/frequencyResponse.png "Magnitude response Biquad low-pass filter 1000Hz")

#### Vibrato

* Vibrato on a triangular wave (7.0 ms max delay, 2Hz LFO) - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/triangleVibrato.mp3)
  
  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  [triangle 10000.0 440.0 >> vibrato 44100.0 7.0 2.0]
  |> playWave 44100.0 5.0;;
  ```

#### Flanging

* Flanging on saw wave (7.0 ms max delay, 0.15 feedback, 0.5 wet/dry, 0.2Hz LFO) - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/sawFlanger.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  [saw 10000.0 440.0 >> flanger 44100.0 7.0 0.15 0.5 0.2]
  |> playWave 44100.0 10.0;;
  ```

#### Flanging on white noise

* Flanging on white noise - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/noiseFlanger.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  [whiteNoise 10000.0 >> flanger 44100.0 7.0 0.15 0.5 0.2]
  |> playWave 44100.0 10.0;;
  ```

#### Chorus

* Chorus effect (44100Hz sampling frequency, 30ms max delay, 0.4 wet/dry, 1.5Hz LFO) on a square wave with an adsr envelope and delay (The delay params are taken from William Sharkey's Interior Sounds (https://github.com/williamsharkey/William-FSound-Songs#1-interior-sounds---click-to-play) - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/sawChorusAdsrDelay.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  [modulate (square 10000.0 440.0 >> chorus 44100.0 30.0 0.4 1.5) (adsr 0.05 1.0 0.05 0.3 0.1 0.05) 
  >> delay 44100.0 2.0 200.0 1.0 0.9 0.5]
  |> playWave 44100.0 10.0;;
  ```
  
* Chorus effect like the above but with a typo in the sampling frequency in the delay (this typo is beautiful, sort of) - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/typoSawChorusAdsrDelay.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  [modulate (square 10000.0 440.0 >> chorus 44100.0 30.0 0.4 1.5) (adsr 0.05 1.0 0.05 0.3 0.1 0.05) 
  >> delay 4410.0 2.0 200.0 1.0 0.9 0.5]
  |> playWave 44100.0 2.0;;  
  ```

#### Streaming sample sequences

* Streaming a sequence of samples to a wav file as the sample is being evaluated.

  ```
  open FSound.IO;;
  open FSound.Signal;;
  open FSound.Filter;;
  [modulate (square 10000.0 440.0 >> chorus 44100.0 30.0 0.4 1.5) (adsr 0.05 1.0 0.05 0.3 0.1 0.05) 
  >> delay 4410.0 2.0 200.0 1.0 0.9 0.5 
  |> generate 44100.0 2.0 ]
  |> streamToWav 44100 2 @"C:\Users\panga\project\FSound\blah.wav";;
  ```

#### Karplus-Strong algorithm

* A crude implementation of the plucked string using Karplus-Strong algorithm.  (Attenuation/Decay cannot be controlled at the moment) - Listen [**Pluck**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/karplusStrong.mp3) [**Vibrato**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/karplusStrongVibrato.mp3) [**Chorus**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/karplusStrongChorus.mp3) [**Flange**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/karplusStrongFlanger.mp3)

  ```
  open FSound.Filter;;
  open FSound.Utilities;;
  pluck2LevelRandom 10000.0 44100.0 256.0 |> playWave 44100.0 5.0;;
  // For some interesting combinations with different variations of modulated delay line
  // Note:  When copying to F# interactive, copy and execute line by line, or they will
  // all sound at the same time
  [pluck2LevelRandom 10000.0 44100.0 256.0 >> vibrato 44100.0 7.0 2.0] |> playWave 44100.0 5.0;;
  [pluck2LevelRandom 10000.0 44100.0 256.0 >> chorus 44100.0 30.0 0.5 2.0] |> playWave 44100.0 5.0;;
  [pluck2LevelRandom 10000.0 44100.0 256.0 >> flanger 44100.0 7.0 0.5 0.5 0.2] |> playWave 44100.0 5.0;;
  ```
 
#### Arrangement of instruments

* Arrange a list of generators to be played at particular time offset.  The following plays the C maj7 chord - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/cmajor7.mp3)

  ```
  open FSound.Signal;;
  open FSOund.Utilities;;
  let gen pitch = modulate (triangle 15000.0 pitch) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  // natural tuning
  let (c, e, g, b) = (gen 256.0, gen 320.0, gen 384.0, gen 480.0)
  [ arrange [(0.0, c); (0.0, e); (0.0, g); (0.0, b)] ]
  |> playWave 44100.0 1.0;;
  ```

#### Ping-pong delay

* Ping pong delay - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/pingpong.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Play;;
  let i p = modulate (triangle 10000.0 p) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  multiplex (i 256.0) (i 384.0) >> pingpong 44100.0 2.0 200.0 1.0 0.9 0.5 0
  |> generate 44100.0 15.0 
  |> playStereo 44100 2;;
  ```

#### Chord progression

* Chord progression CMaj7 to FMaj7 with ping pong delay - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/progression.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Play;;
  let gen pitch = 
    modulate (triangle 15000.0 pitch) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  let (c, d, e, f, g, a, b) = 
    (gen 256.0, gen 288.0, gen 320.0, gen 341.33, gen 384.0, gen 440.0, 
     gen 480.0) 
  
  multiplex (arrange [ (0.0, c); (0.0, b); (4.0, f); (4.0, a) ]) 
    (arrange [ (0.0, e); (0.0, g); (4.0, c); (4.0, e) ])
  >> pingpong 44100.0 2.0 200.0 1.0 0.9 0.5 0
  |> generate 44100.0 15.0
  |> playStereo 44100 2;;
  ```

#### Telephone ringing tone

* Ringing tone (based on Andy Farnell's book "Designing Sound") - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/ringing.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  let level = 10000.0;;
  let fs = 44100.0;;
  let tone() = sum [osc fs level 440.0; osc fs level 480.0];;
  let telephoneLine() =
    clipper level 
    >> bp fs 2000.0 12.0
    >> split ((*) 0.5 >> bp fs 400.0 3.0) 
         (clipper (0.4*level) >> (*) 0.15)
    >> combine
    >> hp fs 90.0;;
  [beep (tone() >> telephoneLine()) 2.0 4.0] |> playWave 44100.0 20.0;;
  ```

#### Schroeder reverberation

* Schroeder reverb - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/schroeder.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Play;;
  let signal = triangle 10000.0 440.0;;
  let envelope = adsr 0.05 1.0 0.05 0.3 0.1 0.05;;
  let inst = signal |> modulateBy envelope;;
  let reverb = schroeder 44100.0 1.0 (101.0, 143.0, 165.0, 177.0)
                 (0.4, 0.37, 0.3333, 0.3);;
  inst >> reverb |> generate 44100.0 2.0 |> playStereo 44100 2;;
  ```

#### Ring modulation

* Ring modulator - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/ringMod.mp3)

  ```
  open FSound.Signal;;
  open FSound.Play;;
  open FSound.Utilities;;
  [ triangle 10000.0 320.0 |> ring 440.0]
  |> playWave 44100.0 1.0;;
  ```

#### FM Synthesis

* FM synthesis - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/fm.mp3)

  ```
  open FSound.Signal;;
  open FSound.Utilities;;

  [fm (Const 10000.0) 256.0 66.0 1.5 ]
  |> playWave 44100.0 1.0;;
  ```

* FM Bass - an attempt - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/fmBass.mp3)

  ```
  open FSound.Signal;;
  open FSound.Utilities;;
  [fm (Const 20000.0) 128.0 66.0 2.0 |> modulate (adsr 0.05 1.0 0.2 0.1 0.0 0.0)]
  |> playWave 44100.0 1.0;;
  ```

* This damn balloon... - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/blowingEverHarder.mp3)
  
  ```
  open FSound.Signal;;
  open FSound.Utilities;;
  let modSinusoid1 (modA : Mod) f fm depth = 
    let pi = System.Math.PI
    fun t -> 
      let a = modA.GetValue t
      a * sin (2.0*pi*(f + depth*sin(2.0*pi*fm*t))*t);;
  [modSinusoid1 (Const 1000.0) 256.0 0.5 20.0] 
  |> playWave 44100.0 14.5;;
  ```

#### Chowning's FM instruments

* Simulation of brass with Chowning's FM Synthesis - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/chowningBrass.mp3)

  ```
  open FSound.Signal;;
  open FSound.Utilities;;
  let dur = 1.0;;
  let mcRatio = 1.0;;
  let depth = 5.0;;
  let depthEnvelope = adsrX 0.2 1.0 0.2 0.6 0.5 0.1 dur;;
  [chowning 20000.0 512.0 mcRatio depth depthEnvelope] 
  |> playWave 44100.0 dur;;
  ```

* Bell - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/chowningBell.mp3)

  ```
  open FSound.Signal;;
  open FSound.Utilities;;
  let mcRatio = 1.4;;
  let depth = 10.0;;
  let env = fadeExp 1.0;;
  [chowning 10000.0 300.0 mcRatio depth env |> modulateBy env]
  |> playWave 44100.0 15.0;;
  ```

* Gong - lower the frequency a bit  - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/chowningGong.mp3)

  ```
  open FSound.Signal;;
  open FSound.Utilities;;
  let mcRatio = 1.4;;
  let depth = 10.0;;
  let env = fadeExp 1.0;;
  [chowning 10000.0 100.0 mcRatio depth env |> modulateBy env]
  |> playWave 44100.0 15.0;;
  ```


## Motivation

This project arises purely out of a personal interest in learning the F#
programming language and applying it to a domain I have always found fascinating - Sound


## Installation

Simply download the project and load it in visual studio 2015.

If your version of Visual Studio does not have F#,  you may 
install the free Visual Studio 2015 Community Edition. 
The Visual F# Tools are installed automatically when you first 
create or open an F# project. See: http://fsharp.org/use/windows/

This project requires you to install a couple of dependent packages. 

To do this with Nu-Get in VS:

1. Right click References in Solution Explorer 
2. Click Manage Nu-Get packages
3. Click Restore when you see "Some NuGet packages are missing from this solution. Click to restore from your online package sources"

You should be all set.  Simply rebuild the project.  Feel free to send an email to albert.pang@me.com if you run into any problems.


## Tests

SignalTest.fsx is the only test script for now.  Load it into F# interactive:
```
C:\Users\panga\project\fsharp\FSound>fsi --load:SignalTest.fsx

Microsoft (R) F# Interactive version 14.0.23020.0
Copyright (c) Microsoft Corporation. All Rights Reserved.

For help type #help;;

[Loading C:\Users\panga\project\fsharp\FSound\SignalTest.fsx]

namespace FSI_0002
  val main : unit -> bool

> SignalTest.main();;
val it : bool = true

> exit 0;;

C:\Users\panga\project\fsharp\FSound>dir *.wav
 Volume in drive C has no label.
 Directory of C:\Users\panga\project\fsharp\FSound

01/09/15  07:17 pm           176,446 saw-440.wav
01/09/15  07:17 pm           176,446 sinusoid-440.wav
01/09/15  07:17 pm           176,446 square-440.wav
01/09/15  07:17 pm           176,446 triangle-440.wav
01/09/15  07:17 pm           882,046 wave.wav
01/09/15  07:17 pm           176,446 whitenoise.wav
               6 File(s)      1,764,276 bytes
               0 Dir(s)  30,879,870,976 bytes free
```
## Contributors
Albert Pang (albert.pang@me.com)

## Showcase
William Sharkey's compositions (https://github.com/williamsharkey/William-FSound-Songs)

## License

Released under GPL v3.0 licence
