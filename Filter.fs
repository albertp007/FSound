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

module Filter =

  open MathNet.Numerics.LinearAlgebra.Double
  open FSound.Data
  open FSound.IO
  open FSound.Signal

  ///
  /// <summary>Filter with feedforward and feedback coefficients
  /// y(n) = ff0 * x(n) + ff1 * x(n-1) + ... + ffm * x(n-m) -
  ///        (fb0 * y(n-1) + fb1 * y(n-2) + ... + fbm * x(n-m-1))
  /// </summary>
  /// <param name="ffcoeff">feed forward coefficients for the input samples
  /// </param>
  /// <param name="fbcoeff">feed back coefficients for the output samples
  /// </param>
  /// <returns>A function which takes in a float as a sample and returns y(n)
  /// </returns>
  ///
  let filter ffcoeff fbcoeff =
    let makeMovingWindow n = MovingWindow<float>(Seq.init n (fun _ -> 0.0))
    let ff_w = makeMovingWindow (Seq.length ffcoeff)
    let fb_w = makeMovingWindow (Seq.length fbcoeff)
    let ff_only = Seq.toList fbcoeff |> Seq.forall (fun x -> x = 0.0)
    let rev_ff = DenseVector(List.rev ffcoeff |> Seq.toArray)
    let rev_fb = DenseVector(List.rev fbcoeff |> Seq.toArray)
    function s -> ff_w.Push(s) |> ignore
                  let s' = rev_ff.DotProduct( DenseVector(ff_w.GetArray()))
                  let w' = if ff_only then s' else 
                           s' - rev_fb.DotProduct( DenseVector(fb_w.GetArray()))
                  fb_w.Push(w')

  ///
  /// <summary>Generate impulse response for a filter</summary>
  /// <param name="n">Number of samples</param>
  /// <param name="filter">the filter function</param>
  /// <returns>impulse response of length n</returns>
  ///
  let impulseResponse n (filter:float->float) =
    let impulse = 1.0::(List.init n (fun _ -> 0.0))
    Seq.map filter impulse

  ///
  /// <summary>A simple resonator</summary>
  /// <param name="fs">sampling frequency</param>
  /// <param name="fc">center frequency</param>
  /// <param name="q">Q factor</param>
  /// <returns>resonator function</returns>
  ///
  let simpleResonator fs fc q =
    let theta = 2.0 * System.Math.PI * fc / fs
    let w = fc / q
    let b2 = exp (-2.0*System.Math.PI*w/fs)
    let b1 = -4.0 * b2 / (1.0 + b2) * (cos theta)
    let a0 = ( 1.0 - b2 ) * sqrt ( 1.0 - b1*b1/4.0/b2)
    filter [a0] [b1; b2]

  ///
  /// <summary>Smith-Angell resonator</summary>
  /// <param name="fs">sampling frequency</param>
  /// <param name="fc">center frequency</param>
  /// <param name="q">Q factor</param>
  /// <returns>resonator function</returns>
  ///
  let smithAngell fs fc q =
    let theta = 2.0 * System.Math.PI * fc / fs
    let w = fc / q
    let b2 = exp ( -2.0 * System.Math.PI * w / fs )
    let b1 = -4.0 * b2 / ( 1.0 + b2 ) * (cos theta)
    let a0 = 1.0 - sqrt b2
    let a2 = -a0
    filter [a0; 0.0; a2] [b1; b2]
      
  ///
  /// <summary>First order single pole low pass filter</summary>
  /// <param name="fs">sampling frequency</param>
  /// <param name="fc">cutoff frequencey</param>
  /// <returns>low pass filter function</returns>
  ///
  let lp fs fc =
    let theta = 2.0 * System.Math.PI * fc / fs
    let b1 = -exp (-theta)
    let a0 = 1.0 + b1
    filter [a0] [b1]

  ///
  /// <summary>First order single pole high pass filter</summary>
  /// <param name="fs">sampling frequency</param>
  /// <param name="fc">cutoff frequencey</param>
  /// <returns>high pass filter function</returns>
  ///
  let hp fs fc =
    let theta = 2.0 * System.Math.PI * fc / fs
    let k = tan (theta/2.0)
    let alpha = 1.0 + k
    let a0 = 1.0
    let a1 = -(1.0-k)/alpha
    let b0 = 1.0/alpha
    let b1 = -1.0/alpha
    filter [b0; b1] [a1]

  ///
  /// <summary>Cubic interpolation between t=0 and t=1</summary>
  /// <param name="yMinus1">y(-1)</param>
  /// <param name="y0">y0</param>
  /// <param name="y1">y1</param>
  /// <param name="y2">y2</param>
  /// <param name="dx">x - x0 but x <= x1</param>
  /// <returns>Intepolated value of x which is equal to x0 + dx</returns>
  ///
  let cubicInterpolate yMinus1 y0 y1 y2 (dt:float) =
    if dt < 0.0 || dt > 1.0 then failwith "dt must be between 0.0 and 1.0"
    let c0 = y0
    let c1 = y1 - yMinus1
    let c2 = yMinus1 - y0 - c0
    let c3 = -yMinus1 + y0 - y1 + y2
    let dtSq = dt * dt
    let dtCube = dtSq * dt
    c3*dtCube + c2*dtSq + c1*dt + c0
     
  ///
  /// <summary>Vanilla delay line implemented by circular buffer</summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="bufferSec">Size of circular buffer in number of seconds
  /// </param>
  /// <param name="delayMs">Delay in number of milli-seconds</param>
  /// <param name="feedback">Feedback multiplier</param>
  /// <param name="wet">Number between 0.0 and 1.0 to control the ratio of the
  /// wet and dry samples</param>
  /// <returns>Function which takes a sample and returns a response with delay
  /// </returns>
  ///
  let delay fs bufferSec delayMs feedback wet =
    if wet < 0.0 || wet > 1.0 then failwith "wet must be between 0.0 and 1.0"
    let bufferSize = int (fs * bufferSec)
    let delaySamples = delayMs / 1000.0 * fs
    let delayNumSamples = int delaySamples
    let fractionalDelay = delaySamples - float delayNumSamples
    let buffer = CircularBuffer(bufferSize, delayNumSamples, 0.0)
    fun sample ->
      let yn = 
        if delayNumSamples = 0 && fractionalDelay = 0.0 then sample 
        else cubicInterpolate (buffer.GetOffset -1) (buffer.Get()) 
               (buffer.GetOffset 1) (buffer.GetOffset 2) fractionalDelay
      let xn = sample
      buffer.Push (xn + feedback * yn)
      wet * yn + (1.0 - wet) * sample

  ///
  /// <summary>Delay line with the number of delayed samples "modulatable" by
  /// an LFO</summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="bufferSec">Size of circular buffer in number of seconds
  /// </param>
  /// <param name="delayMs">Delay in number of milli-seconds</param>
  /// <param name="feedback">Feedback multiplier</param>
  /// <param name="wet">Number between 0.0 and 1.0 to control the ratio of the
  /// wet and dry samples</param>
  /// <param name="lfo">Function that takes time t and return a value</param>
  /// <returns>Function which takes a sample and returns a response with delay
  /// </returns>
  ///
  let mod_delay fs bufferSec delayMs feedback wet lfo =
    if wet < 0.0 || wet > 1.0 then failwith "wet must be between 0.0 and 1.0"
    if bufferSec * 1000.0 < delayMs then failwith "buffer size not large enough"
    let bufferSize = int (fs * bufferSec)
    let delaySamples = delayMs / 1000.0 * fs
    let delayNumSamples = int delaySamples
    let buffer = CircularBuffer(bufferSize, 0, 0.0)
    let mutable n = 0
    fun sample ->
      n <- n + 1
      let t = (float n)/fs
      let d = (lfo t) * delaySamples
      let d' = ceil d
      let frac = d' - d
      buffer.SetLag (int d')
      // TODO: fractional delay handling
      let yn = if abs d < 0.0000001 then sample else buffer.Get()
      let xn = sample
      buffer.Push (xn + feedback * yn)
      wet * yn + (1.0 - wet) * sample

  ///
  /// <summary>Flanger</summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="maxDelayMs">maximum delay in milliseconds</param>
  /// <param name="feedback">feedback gain</param>
  /// <param name="wet">Number between 0.0 and 1.0 to control the ratio of the
  /// wet and dry samples</param>
  /// <param name="sweepFreq">the frequency of the LFO which modulate the number
  /// of delayed samples from 0 to the maxDelayMs</param>
  /// <returns>Function which takes a sample and returns a sample which makes
  /// up the sequence of samples of the flanger effect</returns>
  ///
  let flanger fs maxDelayMs feedback wet sweepFreq =
    let bufferSec = maxDelayMs / 1000.0 * 2.0
    mod_delay fs bufferSec maxDelayMs feedback wet (lfo sweepFreq 0.0 1.0)

  ///
  /// <summary>Vibrato</summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="maxDelayMs">maximum delay in milliseconds</param>
  /// <param name="sweepFreq">the frequency of the LFO which modulate the number
  /// of delayed samples from 0 to the maxDelayMs</param>
  /// <returns>Function which takes a sample and returns a sample which makes
  /// up the sequence of samples of the vibrato effect</returns>
  ///
  let vibrato fs maxDelayMs sweepFreq =
    let bufferSec = maxDelayMs / 1000.0 * 2.0
    mod_delay fs bufferSec maxDelayMs 0.0 1.0 (lfo sweepFreq 0.0 1.0)
