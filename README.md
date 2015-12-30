## Synopsis

FSound is a library for processing sound using F#

## Code Example

Here are some of the things you can do with FSound:

* Generate a .wav file containing a single channel of sawtooth wave of amplitude 10000, 440Hz, 
sampling rate 44100Hz, 1 channel, 16-bit sample (2 bytes) and which lasts for 2 seconds
  
  ```
  open FSound.Signal;;
  open FSound.IO;;
  
  [saw 10000.0 440.0] 
  |> List.map (generate 44100.0 2.0) 
  |> streamToWav 44100 2 @"blah.wav";;
  ```

* Generate a .wav file containing a triangular wave in the left channel and white noise on the right channel

  ```
  open FSound.Signal;;
  open FSound.IO;;
  
  [triangle 10000.0 440.0; whiteNoise 10000.0]
  |> List.map (generate 44100.0 2.0)
  |> streamToWav 44100 2 @"blah.wav";;
  ```
  
* Play a single channel of triangular wave of amplitude 10000, 440Hz, sampling rate 44100Hz, 1 channel and which lasts for 2 seconds - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/triangle.mp3)
  
  ```
  open FSound.Signal;;
  open FSound.Utilities;;
  
  triangle 10000.0 440.0 |> playWave 44100.0 2.0;;
  ```
  
* Play a triangular wave in the left channel and white noise in the right channel. ('playWave' above is a convenience function which calls the more general 'play' function which plays an arbitrary of channels, to first generate a signal and then play it in a single channel) - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/stereo.mp3)

  ```
  open FSound.Signal;;
  open FSound.Play;;
  
  [triangle 10000.0 440.0; whiteNoise 10000.0]
  |> List.map (generate 44100.0 2.0)
  |> play 44100 2;;
  ```

* Add an ADSR envelope to the triangular wave above - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/modulateWithAdsr.mp3)
  
  ```
  open FSound.Signal;;
  open FSound.Utilities;;
  
  [modulate (triangle 20000.0 2000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)]
  |> playWave 44100.0 1.0;;
  ```

* Add 200ms delay, 50/50 dry/wet and 0.15 feedback gain to the triangular wave with an ADSR envelope above - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/modulateTriangleAdsrDelay.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [modulate (triangle 20000.0 2000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  >> delay 44100.0 2.0 200.0 1.0 0.15 0.5]
  |> playWave 44100.0 1.0;;
  ```
  
* The same as right above, except replace triangular wave with white noise to produce the sound of shuffling something - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/modulateNoiseAdsrDelay.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [modulate (whiteNoise 20000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  >> delay 44100.0 2.0 200.0 1.0 0.15 0.5]
  |> playWave 44100.0 1.0;;
  ```

* Shaping white noise with a Smith-Angell resonator - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/noiseSmithAngell.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [whiteNoise 50000.0 >> smithAngell 44100.0 440.0 10.0]
  |> playWave 44100.0 2.0;;
  ```

* Shaping white noise first with an ADSR envelope then a Smith-Angell resonator - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/noiseSmithAngellAdsr.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [modulate (whiteNoise 50000.0) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)
  >> smithAngell 44100.0 880.0 10.0]
  |> playWave 44100.0 2.0;;
  ```

* Generate white noise and pass through a high-pass filter - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/noiseHp.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [whiteNoise 10000.0 >> hp 44100.0 10000.0]
  |> playWave 44100.0 1.0;;
  ```
  
* Modulate amplitude of white noise with an LFO and pass through a low-pass filter - A crude simulation to the sound of waves - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/noiseLfo.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  
  [modulate (whiteNoise 10000.0) (lfo 0.05 0.0 0.8) >> lp 44100.0 220.0]
  |> playWave 44100.0 50.0;;
  ```
  
* Generate 1 sec of white noise samples, pass them through a low-pass filter and plot the frequency spectrum up to 20000Hz

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Plot;;
  
  whiteNoise 10000.0
  >> lp 44100.0 220.0
  |> generate 44100.0 1.0
  |> plotFreq 20000;;
  ```
  
* Vibrato on a triangular wave (7.0 ms max delay, 2Hz LFO) - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/triangleVibrato.mp3)
  
  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  [triangle 10000.0 440.0 >> vibrato 44100.0 7.0 2.0]
  |> playWave 44100.0 5.0;;
  ```
  
* Flanging on saw wave (7.0 ms max delay, 0.15 feedback, 0.5 wet/dry, 0.2Hz LFO) - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/sawFlanger.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  [saw 10000.0 440.0 >> flanger 44100.0 7.0 0.15 0.5 0.2]
  |> playWave 44100.0 10.0;;
  ```

* Flanging on white noise - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/noiseFlanger.mp3)

  ```
  open FSound.Signal;;
  open FSound.Filter;;
  open FSound.Utilities;;
  [whiteNoise 10000.0 >> flanger 44100.0 7.0 0.15 0.5 0.2]
  |> playWave 44100.0 10.0;;
  ```
  
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

* Ring modulator - [**Listen**] (https://cdn.rawgit.com/albertp007/FSound/master/samples/ringMod.mp3)

  ```
  open FSound.Signal;;
  open FSound.Play;;
  open FSound.Utilities;;
  [ triangle 10000.0 320.0 |> ring 440.0]
  |> playWave 44100.0 2.0;;
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

This project requires you to install the NAudio package. 
To do this with Nu-Get in VS:
1. Right click References in Solution Explorer 
2. Click Manage Nu-Get packages
3. Search for NAudio
4. Uninstall then Install NAudio


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
