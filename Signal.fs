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
namespace FSound

module Signal =

  open MathNet.Numerics.IntegralTransforms
  open FSound.Data

  let private random = System.Random()

  ///
  /// <summary>Generates a sequence of samples given a sampling frequency, the
  /// duration (in seconds) required and a waveform function which returns the
  /// value (float) at a given time t (float)</summary>
  /// <param name="sf">Sampling frequency</param>
  /// <param name="tau">Duration in seconds</param>
  /// <param name="waveFunc">Waveform function</param>
  /// <returns>Sequence of floats representing the sequence of samples generated
  /// </returns>
  ///
  let generate sf tau waveFunc =
    let t = seq [0.0..(1.0/sf)..tau]
    Seq.map waveFunc t

  ///
  /// <summary>Sinusoid waveform function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency</param>
  /// <param name="ph">phase</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of the waveform at time t</returns>
  ///
  let sinusoid a f ph t =
    let pi = System.Math.PI
    a * cos (2.0*pi*f*t + ph)

  ///
  /// <summary>White noise waveform function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of the waveform at time t</returns>
  ///
  let whiteNoise a (t:float) = a * (random.NextDouble() - 0.5)

  ///
  /// <summary>Square waveform function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of the waveform at time t</returns>
  ///
  let square (a:float) (f:float) (t:float) =
    let i = int ( 2.0 * f * t ) 
    if i % 2 = 0 then a else (-a)

  ///
  /// <summary>Saw-tooth waveform function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of the waveform at time t</returns>
  ///
  let saw (a:float) (f:float) (t:float) =
    let cycle = int ( f * t )
    let tau = t - float ( cycle ) / f
    -a + 2.0 * a * f * tau

  ///
  /// <summary>Triangular waveform function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of the waveform at time t</returns>
  ///
  let triangle (a:float) (f:float) (t:float) =
    let cycle = int ( 4.0 * f * t )
    let tau = t - float (cycle) / 4.0 / f
    let abs_m = 4.0 * a * f
    let (intercept, slope) =
      match cycle % 4 with
      | 0 -> (0.0, abs_m)
      | 1 -> (a, -abs_m)
      | 2 -> (0.0, -abs_m)
      | _ -> (-a, abs_m)
    intercept + tau * slope

  ///
  /// <summary>Modulator function which multiplies two signals at time t
  /// </summary>
  /// <param name="waveform">primary waveform function</param>
  /// <param name="modulator">modulator waveform function</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of multipliying the value of the primary waveform at 
  /// time t and the value of the modulator waveform at time t</returns>
  ///    
  let modulate (waveform:float->float) (modulator:float->float) (t: float) =
    (waveform t) * (modulator t)

  ///
  /// <summary>Low frequency oscillator</summary>
  /// <param name="f">Frequency(Hz)</param>
  /// <param name="depth">Depth - from 0.0 to 1.0.  When it is set to 0, the
  /// LFO always output 1.0 and therefore it has no effect.  When it is set to 1
  /// it will have full effect.  A value in between 0.0 and 1.0 means it will
  /// have some positive value at its lowest and won't cause the modulated
  /// signal to go to zero</param>
  /// <param name="t">time in seconds</param>
  /// <returns>value of the lfo at time t</returns>
  let lfo f depth t =
    ((sinusoid 1.0 f 0.0 t) + 1.0) * 0.5 * depth + (1.0 - depth)

  ///
  /// <summary>ADSR envelope</summary>
  /// <param name="at_t">Duration of attack (sec)</param>
  /// <param name="at_level">Attach level</param>
  /// <param name="decay_t">Duration of decay period (sec)</param>
  /// <param name="sus_perc">Suspension level as a percentage of attack level
  /// </param>
  /// <param name="sus_t">Duration of suspension (sec)</param>
  /// <param name="release_t">Duration of release (sec)</param>
  /// <param name="t">time in seconds</param>
  /// <returns>Value of the ADSR envelope at time t</returns>
  ///
  let adsr at_t at_level decay_t sus_perc sus_t release_t t =
    let sus_level = at_level * sus_perc
    let attack_start = 0.0
    let decay_start = at_t
    let suspend_start = decay_start + decay_t
    let release_start = suspend_start + sus_t
    let release_end = release_start + release_t
    let (intercept, slope, start_point) =
      match t with
      | t when t >= attack_start && t < decay_start-> 
        (0.0, at_level/at_t, attack_start)
      | t when t >= decay_start && t < suspend_start -> 
          (at_level, (sus_perc-1.0)*at_level/decay_t, decay_start)
      | t when t >= suspend_start && t < release_start -> 
          (sus_level, 0.0, suspend_start)
      | t when t >= release_start && t < release_end ->
          (sus_level, -sus_level/release_t, release_start )
      | _ -> (0.0, 0.0, release_end)
    (t - start_point)*slope + intercept

  /// <summary>Delay a sequence of samples by N sample. Note that a
  /// CircularBuffer object is created to keep track of historical samples
  /// each time the function is instantiated!</summary>
  /// <param name="n">The number of samples to delay</param>
  /// <param name="initValue">Initial value of the windowing buffer before it is
  /// fully populated</param>
  /// <returns>sequence of samples which is the summation of the original sample
  /// and the n-th sample before it</returns>
  let delay n (initValue: float) =
    let buffer = CircularBuffer<float>( n, initValue )
    fun sample -> 
      let res = sample + buffer.Get()
      buffer.Push( sample )
      res

  ///
  /// <summary>Waveform generation function to simulate the sound of waves
  /// with an lfo modulating white noise and post-processed by a delay of 1 
  /// sample as a very crude low-pass filter</summary>
  ///
  let filter filterFunc waveFormFunc t = waveFormFunc t |> filterFunc
          
  ///
  /// <summary>Convenience function which combines sinusoid waveform with
  /// the generate function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency of the sinusoid</param>
  /// <param name="ph">phase of the sinusoid</param>
  /// <param name="sf">sampling frequency</param>
  /// <param name="tau">duration of the samples to be generated</param>
  /// <returns>Sequence of samples</returns>
  ///
  let sinusoidGenerator a f ph sf tau =
    sinusoid a f ph |> generate sf tau

  ///
  /// <summary>Convenience function which combines whitenoise waveform with
  /// the generate function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="sf">sampling frequency</param>
  /// <param name="tau">duration of the samples to be generated</param>
  /// <returns>Sequence of samples</returns>
  ///
  let whiteNoiseGenerator a sf tau =
    whiteNoise a |> generate sf tau

  ///
  /// <summary>Implements a very crude model of the sound of waves by modulating
  /// white noise waveform with a LFO</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">LFO frequency</param>
  /// <param name="sf">sampling frequency</param>
  /// <param name="tau">duration of the samples to be generated</param>
  /// <returns>Sequence of samples</returns>
  ///
  let waveGenerator sf tau =
    let delay' = delay 1 0.0
    let wf t = (whiteNoise 10000.0 t) * (lfo 0.05 0.8 t)
    let seaWave t = delay' (wf t)
    seaWave
    |> generate sf tau

  ///
  /// <summary>Convenience function which combines square waveform with
  /// the generate function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency of the square waveform</param>
  /// <param name="sf">sampling frequency</param>
  /// <param name="tau">duration of the samples to be generated</param>
  /// <returns>Sequence of samples</returns>
  ///
  let squareGenerator a f sf tau =
    square a f |> generate sf tau

  ///
  /// <summary>Convenience function which combines saw-tooth waveform with
  /// the generate function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency of the saw-tooth waveform</param>
  /// <param name="sf">sampling frequency</param>
  /// <param name="tau">duration of the samples to be generated</param>
  /// <returns>Sequence of samples</returns>
  ///
  let sawGenerator a f sf tau =
    saw a f |> generate sf tau

  ///
  /// <summary>Convenience function which combines triangular waveform with
  /// the generate function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency of the triangular waveform</param>
  /// <param name="sf">sampling frequency</param>
  /// <param name="tau">duration of the samples to be generated</param>
  /// <returns>Sequence of samples</returns>
  ///
  let triangleGenerator a f sf tau =
    triangle a f |> generate sf tau

  ///
  /// <summary>Folding with an index</summary>
  /// <param name="f">function which takes a state, an integer which is the
  /// index of the element in the sequence, the element itself and returns a new
  /// state</param>
  /// <param name="acc">initial state</param>
  /// <param name="xs">list of elements to be folded</param>
  /// <returns></returns>
  ///
  let foldi f acc xs = 
    let rec foldi' f i acc xs =
      match xs with
      | [] -> acc
      | h::t -> foldi' f (i+1) (f acc i h) t
    foldi' f 0 acc xs

  ///
  /// <summary>Naive implementation of the discrete fourier transform. Use at
  /// your own peril - it does not perform well and only amplitude is calculated
  /// </summary>
  /// <param name="samples">list of samples</param>
  /// <returns>list of frequency component amplitudes</returns>
  ///
  let naiveDft samples =
 
    let dftComponent k s =
      let N = Seq.length s
      let w = 2.0*System.Math.PI*(float k)/(float N)
      foldi (fun (re,im) i x-> (re + x*cos(w*(float i)), 
                                im + x*sin(w*(float i)))) 
                                (0.0, 0.0) s
    
    Seq.mapi (fun i _ -> dftComponent i samples) samples

  /// <summary>Wrapper for the MathNet.Numerics (3.7.0) fourier transform.
  /// First convert the float samples to System.Numerics.Complex.  Then
  /// call MathNet.Numerics.IntegralTransforms.Fourier.Forward which modifies
  /// the input inline</summary>
  /// <param name="samples">sequence of real float samples</param>
  /// <returns>complex array</returns>
  let fft samples =

    let cmplxSamples = 
      samples 
      |> Seq.map (fun x -> System.Numerics.Complex(x, 0.0))
      |> Seq.toArray

    Fourier.Forward(cmplxSamples)
    cmplxSamples |> Array.map (fun x -> x.Real)