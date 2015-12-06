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
    function 
    | s -> 
      ff_w.Push(s) |> ignore
      let s' = rev_ff.DotProduct(DenseVector(ff_w.GetArray()))
      
      let w' = 
        if ff_only then s'
        else s' - rev_fb.DotProduct(DenseVector(fb_w.GetArray()))
      fb_w.Push(w')
  
  ///
  /// <summary>Generate impulse response for a filter</summary>
  /// <param name="n">Number of samples</param>
  /// <param name="filter">the filter function</param>
  /// <returns>impulse response of length n</returns>
  ///
  let impulseResponse n (filter : float -> float) = 
    let impulse = 1.0 :: (List.init n (fun _ -> 0.0))
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
    let b2 = exp (-2.0 * System.Math.PI * w / fs)
    let b1 = -4.0 * b2 / (1.0 + b2) * (cos theta)
    let a0 = (1.0 - b2) * sqrt (1.0 - b1 * b1 / 4.0 / b2)
    filter [ a0 ] [ b1; b2 ]
  
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
    let b2 = exp (-2.0 * System.Math.PI * w / fs)
    let b1 = -4.0 * b2 / (1.0 + b2) * (cos theta)
    let a0 = 1.0 - sqrt b2
    let a2 = -a0
    filter [ a0; 0.0; a2 ] [ b1; b2 ]
  
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
    filter [ a0 ] [ b1 ]
  
  ///
  /// <summary>First order single pole high pass filter</summary>
  /// <param name="fs">sampling frequency</param>
  /// <param name="fc">cutoff frequencey</param>
  /// <returns>high pass filter function</returns>
  ///
  let hp fs fc = 
    let theta = 2.0 * System.Math.PI * fc / fs
    let k = tan (theta / 2.0)
    let alpha = 1.0 + k
    let a0 = 1.0
    let a1 = -(1.0 - k) / alpha
    let b0 = 1.0 / alpha
    let b1 = -1.0 / alpha
    filter [ b0; b1 ] [ a1 ]
  
  ///
  /// <summary>Cubic interpolation between t=0 and t=1</summary>
  /// <param name="yMinus1">y(-1)</param>
  /// <param name="y0">y0</param>
  /// <param name="y1">y1</param>
  /// <param name="y2">y2</param>
  /// <param name="dx">x - x0 but x <= x1</param>
  /// <returns>Intepolated value of x which is equal to x0 + dx</returns>
  ///
  let cubicInterpolate yMinus1 y0 y1 y2 (dt : float) = 
    if dt < 0.0 || dt > 1.0 then failwith "dt must be between 0.0 and 1.0"
    let c0 = y0
    let c1 = y1 - yMinus1
    let c2 = yMinus1 - y0 - c0
    let c3 = -yMinus1 + y0 - y1 + y2
    let dtSq = dt * dt
    let dtCube = dtSq * dt
    c3 * dtCube + c2 * dtSq + c1 * dt + c0
  
  ///
  /// <summary>
  /// Private helper function for calculating delay params
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="bufferSec">Buffer size in terms of number of seconds</param>
  /// <param name="delayMs">Delay in milliseconds</param>
  /// <returns>Tuple of buffer size in number of samples, the delay in number
  /// of samples, rounded down delay in number of samples to the nearest int and
  /// fractional delay</returns>
  ///
  let private calcDelayParams fs bufferSec delayMs = 
    let bufferSize = int (fs * bufferSec)
    let delaySamples = delayMs / 1000.0 * fs
    let delayNumSamples = int delaySamples
    let fractionalDelay = delaySamples - float delayNumSamples
    (bufferSize, delayNumSamples, fractionalDelay)
  
  ///
  /// <summary>
  /// Private helper function which calculates the output of the delay line by
  /// cubic interpolation given fractional delay
  /// </summary>
  /// <param name="delaySamples">Delay in number of samples</param>
  /// <param name="fraction">Fractional delay</param>
  /// <param name="sample">The input sample</param>
  /// <param name="buffer">The circular buffer in the delay line</param>
  /// <returns>The interpolated delayed sample</returns>
  /// 
  let interpolateDelay delaySamples fraction sample 
      (buffer : CircularBuffer<float>) = 
    if delaySamples = 0 && fraction = 0.0 then sample
    else 
      cubicInterpolate buffer.[-1] buffer.[0] buffer.[1] buffer.[2] 
        fraction
  
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
  let delay fs bufferSec delayMs gain feedback wet = 
    if wet < 0.0 || wet > 1.0 then failwith "wet must be between 0.0 and 1.0"
    let (bufferSize, delaySamples, fraction) = 
      calcDelayParams fs bufferSec delayMs
    let buf = CircularBuffer(bufferSize, delaySamples, (fun _ -> 0.0))
    fun sample -> 
      let yn = interpolateDelay delaySamples fraction sample buf
      let xn = sample
      buf.Push(gain * xn + feedback * yn)
      wet * yn + (1.0 - wet) * sample
  
  ///
  /// <summary>
  /// Creates a ping pong delay line which generates a pair of samples, one for
  /// the left channel and one for the right, given a pair of input samples for
  /// the left and right channel
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="bufferSec">The size of the delay buffer in number of seconds
  /// </param>
  /// <param name="delayMs">Delay in milliseconds</param>
  /// <param name="gain">Gain multiplier on the raw input sample</param>
  /// <param name="feedback">Feed multiplier</param>
  /// <param name="wet">Number between 0.0 and 1.0 to control the ratio of the
  /// wet and dry samples</param>
  /// <returns>A function which takes a pair of samples and return a pair of
  /// samples output by the delay lines</returns>
  ///
  let pingpong fs bufferSec delayMs gain feedback wet = 
    if wet < 0.0 || wet > 1.0 then failwith "wet must be between 0.0 and 1.0"
    let (bufferSize, delaySamples, fraction) = 
      calcDelayParams fs bufferSec delayMs
    let lBuf = CircularBuffer(bufferSize, delaySamples, (fun _ -> 0.0))
    let rBuf = CircularBuffer(bufferSize, delaySamples, (fun _ -> 0.0))
    fun (l, r) -> 
      let leftY = interpolateDelay delaySamples fraction r lBuf
      let rightY = interpolateDelay delaySamples fraction l rBuf
      lBuf.Push(gain * r + feedback * rightY)
      rBuf.Push(gain * l + feedback * leftY)
      (wet * leftY + (1.0 - wet) * l, wet * rightY + (1.0 - wet) * r)
  
  let lcr fs bufferSec delayMs gain feedback wet lpf hpf = 
    if wet < 0.0 || wet > 1.0 then failwith "wet must be between 0.0 and 1.0"
    let (bufferSize, delaySamples, fractionalDelay) = 
      calcDelayParams fs bufferSec delayMs
    let lBuf = CircularBuffer(bufferSize, delaySamples, (fun _ -> 0.0))
    let cBuf = CircularBuffer(bufferSize, delaySamples, (fun _ -> 0.0))
    let rBuf = CircularBuffer(bufferSize, delaySamples, (fun _ -> 0.0))
    let hp = hp fs hpf
    let lp = lp fs lpf
    fun (l, r) -> 
      let centerY = 
        interpolateDelay delaySamples fractionalDelay (l + r) cBuf
      cBuf.Push(gain * (l + r) + (feedback * centerY)
                |> hp
                |> lp)
      lBuf.Push(gain * l)
      rBuf.Push(gain * r)
      let leftY = interpolateDelay delaySamples fractionalDelay l lBuf
      let rightY = interpolateDelay delaySamples fractionalDelay r rBuf
      (l * (1.0 - wet) + leftY * wet + centerY * wet), 
      (r * (1.0 - wet) + rightY * wet + centerY * wet)
  
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
  let mod_delay fs bufferSec delayMs gain feedback wet lfo = 
    if wet < 0.0 || wet > 1.0 then failwith "wet must be between 0.0 and 1.0"
    if bufferSec * 1000.0 < delayMs then failwith "buffer size not large enough"
    let bufferSize = int (fs * bufferSec)
    let delaySamples = delayMs / 1000.0 * fs
    let buffer = CircularBuffer(bufferSize, 0, fun _ -> 0.0)
    let n = ref 0
    fun sample -> 
      n := !n + 1
      let t = (float !n) / fs
      // Add an extra one sample to the delay to make sure there is one sample 
      // ahead for linear interpolation
      let d = (lfo t) * delaySamples + 1.0
      let d' = ceil d
      let frac = d' - d
      buffer.SetLag(int d')
      // printfn "frac: %f d: %f d': %f x0: %f x1: %f v: %f" frac d d' (buffer.Get() )
      //  (buffer.GetOffset 1) (buffer.Get() * (1.0 - frac) + (buffer.GetOffset 1) * frac)
      let yn = 
        if abs (d - 1.0) < 0.0000001 then sample
        else buffer.[0] * (1.0 - frac) + (buffer.[1]) * frac
      
      let xn = sample
      buffer.Push(gain * xn + feedback * yn)
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
    mod_delay fs bufferSec maxDelayMs 1.0 feedback wet 
      (lfo sweepFreq System.Math.PI 1.0)
  
  ///
  /// <summary>Vibrato</summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="maxDelayMs">maximum delay in milliseconds</param>
  /// <param name="sweepFreq">the frequency of the LFO which modulates the 
  /// number of delayed samples from 0 to the maxDelayMs</param>
  /// <returns>Function which takes a sample and returns a sample which makes
  /// up the sequence of samples of the vibrato effect</returns>
  ///
  let vibrato fs maxDelayMs sweepFreq = 
    let bufferSec = maxDelayMs / 1000.0 * 2.0
    mod_delay fs bufferSec maxDelayMs 1.0 0.0 1.0 
      (lfo sweepFreq System.Math.PI 1.0)
  
  ///
  /// <summary>Chorus - largely the same as the vibrato except that the lfo
  /// starts in the mid-point between 0 and max delay</summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="sweepFreq">the frequency of the LFO which modulates the
  /// number of delayed samples from 0 to maxDelayMs</param>
  /// <returns>Function which takes a sample and returns a sample which makes
  /// up the sequence of samples of the chorus effect</returns>
  /// 
  let chorus fs maxDelayMs wet sweepFreq = 
    let bufferSec = maxDelayMs / 1000.0 * 2.0
    mod_delay fs bufferSec maxDelayMs 1.0 0.0 wet 
      (lfo sweepFreq (System.Math.PI / 2.0) 1.0)
  
  ///
  /// <summary>Models the sound of a plucked string using the Karplus-Strong
  /// algorithm.  This is a first attempt and there is no control for
  /// attenuation at the moment</summary>
  /// <param name="a">Maximum amplitude in the wave table</param>
  /// <param name="fs">Sampling frequencey in Hz</param>
  /// <param name="f">Frequency in Hz</param>
  /// <param name="blend"> 1.0 - plucked string, 0.5 - snare drum</param>
  /// <param name="initBufferFunc">Function to initialize the wave table</param>
  /// <returns>Function which takes one dummy argument and generates samples
  /// that sounds like a plucked string using the Karplus Strong algorithm
  /// </returns>
  ///
  let pluckInitBuffer a (fs : float) f blend (initBufferFunc : int -> float) = 
    let random = System.Random()
    let nSample = (int (round fs / f))
    let lag = nSample
    let wavetable = CircularBuffer(nSample, lag, initBufferFunc)
    fun (s : float) -> 
      let output = wavetable.[0]
      let y = 0.5 * (wavetable.[0] + wavetable.[-1])
      
      let y' = 
        if blend = 1.0 then y
        else 
          (if random.NextDouble() <= blend then 1.0
           else -1.0)
          * y
      wavetable.Push(y')
      output
  
  ///
  /// <summary>Two level randomness generator function to initialize the
  /// wavetable in the pluckInitBuffer function</summary>
  /// <param name="a">Maximum amplitude</param>
  /// <returns>Init function for an array</returns>
  ///
  let init2LevelRandom a = 
    let random = System.Random()
    fun _ -> 
      (if random.Next(2) = 0 then -1.0
       else 1.0)
      * a
  
  ///
  /// <summary>Function to initialize the wavetable in the pluckInitBuffer 
  /// function with white noise</summary>
  /// <param name="a">Maximum amplitude</param>
  /// <returns>Init function for an array</returns>
  ///
  let initWhiteNoise a = 
    let dummy = 0.0
    fun _ -> whiteNoise a dummy
  
  ///
  /// <summary>Pluck string function with 2 level randomness initialization.
  /// This simply calls pluckInitBuffer with init2LevelRandom</summary>
  /// <param name="a">Maximum amplitude</param>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="f">Frequency in Hz</param>
  /// <returns>Function which takes one dummy argument and generates samples
  /// that sounds like a plucked string using the Karplus Strong algorithm
  /// </returns>
  ///
  let pluck2LevelRandom a (fs : float) f = 
    pluckInitBuffer a fs f 1.0 (init2LevelRandom a)
  
  ///
  /// <summary>Pluck string function with white noise initialization. This 
  /// simply calls pluckInitBuffer with initWhiteNoise</summary>
  /// <param name="a">Maximum amplitude</param>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="f">Frequency in Hz</param>
  /// <returns>Function which takes one dummy argument and generates samples
  /// that sounds like a plucked string using the Karplus Strong algorithm
  /// </returns>
  ///
  let pluckWhiteNoise a (fs : float) f = 
    pluckInitBuffer a fs f 1.0 (initWhiteNoise a)
  
  /// <summary>
  /// Create a generator by combining a pair of generators, one for the left and
  /// and the other for the right channel.  Instead of returning one sample
  /// like the other generators,  it returns a pair of samples with the first
  /// value for the left channel and the second value for the right channel 
  /// </summary>
  /// <param name="gen1">The generator for the left channel</param>
  /// <param name="gen2">The generator for the right channel</param>
  /// <param name="t"></param>
  /// <returns>A function that generates a pair of samples given a time t
  /// </returns>
  let multiplex leftGen rightGen = fun t -> (leftGen t, rightGen t)
  
  /// <summary>
  /// Given a sequence of pair of samples representing the left and right
  /// channel, create a list with two sequences of samples in it.  The first
  /// element in the list represents the sequence of samples for the left
  /// channel and the second element that of the right channel.  The result
  /// can then be fed into the various play function and stream function
  /// </summary>
  /// <param name="seqPairs">A sequence of pairs</param>
  /// <returns>A list of two sequences</returns>
  let demultiplex (seqPairs : seq<'a * 'a>) = 
    [ seqPairs |> Seq.map fst
      seqPairs |> Seq.map snd ]
