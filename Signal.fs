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
  open MathNet.Numerics.LinearAlgebra.Double
  open FSound.Data
  open FSound.IO
  
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
    seq { 
      for t in 0.0..(1.0 / sf)..tau -> waveFunc t
    }
  
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
    a * cos (2.0 * pi * f * t + ph)
  
  ///
  /// <summary>White noise waveform function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of the waveform at time t</returns>
  ///
  let whiteNoise a (t : float) = 
    // The random number generator needs to be outside of the scope of this
    // function in order for calling it in a seq expression to actually
    // generate a different number every time.  If the random number generator
    // is defined within the scope of this function, then calling it in a
    // sequence expression will actually generate a sequence of the same number
    // Looks like there is a binding somewhere which I am not able to understand
    2.0 * a * (random.NextDouble() - 0.5) + t * 0.0

  ///
  /// <summary>Returns a function which generates an on-off signal</summary>
  /// <param name="on">The on value</param>
  /// <param name="off">The off value </param>
  /// <param name="onTime">The duration in seconds when the output value is set
  /// the on value</param>
  /// <param name="offTime">The duration in seconds when the output value is set
  /// to the off value</param>
  /// <returns>A signal function that takes time as a parameter</returns>
  ///
  let onoff (on : float) (off : float) (onTime : float) (offTime : float) = 
    let totalTime = onTime + offTime
    fun t -> 
      if t % totalTime <= onTime then on
      else off
  
  ///
  /// <summary>Square waveform function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of the waveform at time t</returns>
  ///
  let square (a : float) (f : float) (t : float) = 
    let square' = onoff a (-a) (0.5 / f) (0.5 / f)
    square' t
  
  ///
  /// <summary>Saw-tooth waveform function from -a to a</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of the waveform at time t</returns>
  ///
  let saw (a : float) (f : float) (t : float) = 
    let tau = t % (1.0/f)
    -a + 2.0 * a * f * tau

  /// <summary>
  /// A ramp which is basically just a sawtooth from 0 to a
  /// </summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of the waveform at time t</returns>
  let ramp a f t =
    (a + saw a f t) * 0.5

  ///
  /// <summary>Triangular waveform function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of the waveform at time t</returns>
  ///
  let triangle (a : float) (f : float) (t : float) = 
    let cycle = int (4.0 * f * t)
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
  let modulate (waveform : float -> float) (modulator : float -> float) 
      (t : float) = (waveform t) * (modulator t)

  ///
  /// <summary>
  /// Same as modulate but with order of waveform and modulator reversed for
  /// easier piping
  /// </summary>
  /// <param name="modulator">modulator waveform function</param>
  /// <param name="waveform">primary waveform function</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of multipliying the value of the primary waveform at 
  /// time t and the value of the modulator waveform at time t</returns>
  ///
  let modulateBy modulator waveform t = modulate waveform modulator t
  
  ///
  /// <summary>Sums a sequence of waveform functions at time t</summary>
  /// <param name="waveforms">Sequence of waveform functions</param>
  /// <param name="t">time in seconds</param>
  /// <returns>the value of summing the value of all the waveform functions in
  /// the list at time t</returns>
  ///
  let sum waveforms (t : float) = 
    Seq.fold (fun acc wf -> acc + wf t) 0.0 waveforms
  
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
  let lfo f phase depth t = 
    // short circuit for depth = 0.0
    if depth = 0.0 then 1.0
    else ((sinusoid 1.0 f phase t) + 1.0) * 0.5 * depth + (1.0 - depth)
  
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
      | t when t >= attack_start && t < decay_start -> 
        (0.0, at_level / at_t, attack_start)
      | t when t >= decay_start && t < suspend_start -> 
        (at_level, (sus_perc - 1.0) * at_level / decay_t, decay_start)
      | t when t >= suspend_start && t < release_start -> 
        (sus_level, 0.0, suspend_start)
      | t when t >= release_start && t < release_end -> 
        (sus_level, -sus_level / release_t, release_start)
      | _ -> (0.0, 0.0, release_end)
    (t - start_point) * slope + intercept
  
  ///
  /// <summary>Hard-clips a sample</summary>
  /// <param name="bottom">the minimum level</param>
  /// <param name="top">the maximum level</param>
  /// <returns>the value of the sample after clipping
  ///
  let clipper2 bottom top = max bottom >> min top

  ///
  /// <summary>Hard-clips a sample symmetrically by +/- level</summary>
  /// <param name="level">the level at which the sample will be clipped both
  /// up and down</param>
  /// <returns>the value of the sample after clipping
  ///
  let clipper (level:float) =
    let l = abs level
    clipper2 -l l
 
  /// <summary>Convenience function which combines sinusoid waveform with
  /// the generate function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency of the sinusoid</param>
  /// <param name="ph">phase of the sinusoid</param>
  /// <param name="sf">sampling frequency</param>
  /// <param name="tau">duration of the samples to be generated</param>
  /// <returns>Sequence of samples</returns>
  ///
  let sinusoidGenerator a f ph sf tau = sinusoid a f ph |> generate sf tau
  
  ///
  /// <summary>Convenience function which combines whitenoise waveform with
  /// the generate function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="sf">sampling frequency</param>
  /// <param name="tau">duration of the samples to be generated</param>
  /// <returns>Sequence of samples</returns>
  ///
  let whiteNoiseGenerator a sf tau = whiteNoise a |> generate sf tau
  
  ///
  /// <summary>Convenience function which combines square waveform with
  /// the generate function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency of the square waveform</param>
  /// <param name="sf">sampling frequency</param>
  /// <param name="tau">duration of the samples to be generated</param>
  /// <returns>Sequence of samples</returns>
  ///
  let squareGenerator a f sf tau = square a f |> generate sf tau
  
  ///
  /// <summary>Convenience function which combines saw-tooth waveform with
  /// the generate function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency of the saw-tooth waveform</param>
  /// <param name="sf">sampling frequency</param>
  /// <param name="tau">duration of the samples to be generated</param>
  /// <returns>Sequence of samples</returns>
  ///
  let sawGenerator a f sf tau = saw a f |> generate sf tau
  
  ///
  /// <summary>Convenience function which combines triangular waveform with
  /// the generate function</summary>
  /// <param name="a">amplitude</param>
  /// <param name="f">frequency of the triangular waveform</param>
  /// <param name="sf">sampling frequency</param>
  /// <param name="tau">duration of the samples to be generated</param>
  /// <returns>Sequence of samples</returns>
  ///
  let triangleGenerator a f sf tau = triangle a f |> generate sf tau
  
  ///
  /// <summary>A signal generator which arranges a sequence of other signal
  /// generators to be played at a specific time.  As this is actually a signal
  /// generator itself, it returns a function which takes one parameter t and
  /// returns the value of the sample at t by summing up the value generated
  /// by all the generators at time t</summary>
  /// <param name="generatorAtTimeList">List of pairs of (timeOffset, generator)
  /// The timeOffset is in seconds and the generator is simply another signal
  /// generator which takes a time parameter.  The generator will generate a
  /// value only at a time after timeOffset seconds.  For all time t before
  /// timeOffset, the value generated is 0.0, which means the particular
  /// generator is silent</param>
  /// <param name="t">Time in seconds<param>
  /// <returns>The sum of the sample value of each arranged generators at time t
  /// </returns>
  ///
  let arrange generatorAtTimeList t = 
    let c (f : float -> float) = 
      fun s -> 
        if s < 0.0 then 0.0
        else f s
    Seq.fold (fun v (s, gen) -> v + (c gen) (t - s)) 0.0 generatorAtTimeList

  /// <summary>
  /// Generates a signal function which beeps a given waveform on and off
  /// </summary>
  /// <param name="waveform">The input waveform function</param>
  /// <param name="onTime">The duration of on in seconds</param>
  /// <param name="offTime">The duration of off in seconds</param>
  /// <returns>A signal function</returns>
  let beep waveform onTime offTime =
    waveform |> modulateBy (onoff 1.0 0.0 onTime offTime)
