//
// FSound - F# Sound Processing Library
// Copyright (c) 2022 by Albert Pang <albert.pang@me.com> 
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
  open System.Numerics
  
  ///
  /// <summary>Filter with feedforward and feedback coefficients
  /// y(n) = ff0 * x(n) + ff1 * x(n-1) + ... + ffm * x(n-m) -
  ///        (fb0 * y(n-1) + fb1 * y(n-2) + ... + fbm * x(n-m-1))
  /// </summary>
  /// <param name="ffcoeff">feed forward coefficients for the input samples
  /// </param>
  /// <param name="fbcoeff">feed back coefficients for the output samples
  /// </param>
  /// <param name="ffinit">initialization function for the feedforward buffer
  /// </param>
  /// <param name="fbinit">initialization function for the feedback buffer
  /// </param
  /// <returns>A function which takes in a float as a sample and returns y(n)
  /// </returns>
  ///
  let filter_with_init ffcoeff fbcoeff ffinit fbinit = 
    let makeMovingWindow n init = MovingWindow<float>(Seq.init n init)
    let ff_w = makeMovingWindow (Seq.length ffcoeff) ffinit
    let fb_w = makeMovingWindow (Seq.length fbcoeff) fbinit
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
  /// <summary>Filter with feedforward and feedback coefficients
  /// y(n) = ff0 * x(n) + ff1 * x(n-1) + ... + ffm * x(n-m) -
  ///        (fb0 * y(n-1) + fb1 * y(n-2) + ... + fbm * x(n-m-1))
  /// It simply defers to filter_with_init with zero init function
  /// </summary>
  /// <param name="ffcoeff">feed forward coefficients for the input samples
  /// </param>
  /// <param name="fbcoeff">feed back coefficients for the output samples
  /// </param>
  /// <returns>A function which takes in a float as a sample and returns y(n)
  /// </returns>
  ///
  let filter ffcoeff fbcoeff = 
    let zero _ = 0.0
    filter_with_init ffcoeff fbcoeff zero zero

  /// <summary>
  /// Same as filter, but with the coefficients passed in as a pair
  /// </summary>
  /// <param name="ffcoeff">feed forward coefficients for the input samples
  /// </param>
  /// <param name="fbcoeff">feed back coefficients for the output samples
  /// </param>
  /// <returns>A function which takes in a float as a sample and returns y(n)
  /// </returns>
  let filterP (ffcoeff, fbcoeff) = filter ffcoeff fbcoeff

  /// <summary>
  /// Build a filter with a function which calculates the feedforward and
  /// feedback coefficients given the sampling frequency, the center frequency
  /// and the Q-factor
  /// </summary>
  /// <param name="coeffFunc">A function which takes in sampling frequency, the
  /// centre frequency and the Q factor and returns a pair of list of floats
  /// representing the list of feedforward coefficients and feedback
  /// coefficients respectively</param>
  /// <param name="fs">Sampling frequency in Hz, simply passed through to the
  /// coefficient function</param>
  /// <param name="fc">Centre frequency in Hz, simply passed through to the
  /// coefficient function</param>
  /// <param name="q">Q factor, simply passed through to the coefficient
  /// function</param>
  let buildFilter coeffFunc fs fc q = coeffFunc fs fc q |> filterP

  /// <summary>
  /// Build a filter with a function which calculates the feedforward and
  /// feedback coefficients given the sampling frequency, the center frequency
  /// </summary>
  /// <param name="coeffFunc">A function which takes in sampling frequency, the
  /// centre frequency and returns a pair of list of floats representing the 
  /// list of feedforward coefficients and feedback coefficients respectively
  /// </param>
  /// <param name="fs">Sampling frequency in Hz, simply passed through to the
  /// coefficient function</param>
  /// <param name="fc">Centre frequency in Hz, simply passed through to the
  /// coefficient function</param>
  /// function</param>
  let buildFilter2 coeffFunc fs fc = coeffFunc fs fc |> filterP  

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
  let simpleResonatorCoeff fs fc q = 
    let theta = 2.0 * System.Math.PI * fc / fs
    let w = fc / q
    let b2 = exp (-2.0 * System.Math.PI * w / fs)
    let b1 = -4.0 * b2 / (1.0 + b2) * (cos theta)
    let a0 = (1.0 - b2) * sqrt (1.0 - b1 * b1 / 4.0 / b2)
    ([ a0 ],  [ b1; b2 ])

  let simpleResonator = simpleResonatorCoeff |> buildFilter
  
  ///
  /// <summary>Smith-Angell resonator</summary>
  /// <param name="fs">sampling frequency</param>
  /// <param name="fc">center frequency</param>
  /// <param name="q">Q factor</param>
  /// <returns>resonator function</returns>
  ///
  let smithAngellCoeff fs fc q = 
    let theta = 2.0 * System.Math.PI * fc / fs
    let w = fc / q
    let b2 = exp (-2.0 * System.Math.PI * w / fs)
    let b1 = -4.0 * b2 / (1.0 + b2) * (cos theta)
    let a0 = 1.0 - sqrt b2
    let a2 = -a0
    [ a0; 0.0; a2 ], [ b1; b2 ]

  let smithAngell = smithAngellCoeff |> buildFilter
  
  /// <summary>
  /// Biquad filter template for lp, hp, bp, notch and allpass filters.  Supply
  /// an equation to calculate the zeroes (i.e. the feedforward coefficients)
  /// (The poles are all the same)
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="fc">Center frequency in Hz</param>
  /// <param name="q">Q factor</param>
  /// <param name="designEq">Function which takes w, the angular frequency and
  /// alpha and return a triplet of feedforward parameters</param>
  /// <returns>Vavrious bi-quad filter function according to the design eq
  /// </returns>
  let biquadCoeff fs fc q designEq = 
    let w = 2.0 * System.Math.PI * fc / fs
    let cosw = cos w
    let alpha = sin w / 2.0 / q
    let (b0, b1, b2) = designEq w alpha
    let a0 = 1.0 + alpha
    let a1 = -2.0 * cosw
    let a2 = 1.0 - alpha
    [ b0 / a0; b1 / a0; b2 / a0 ], [ a1 / a0; a2 / a0 ]

  let biquad fs fc q designEq = biquadCoeff fs fc q designEq |> filterP
  
  /// <summary>
  /// Bi-quad low pass filter
  /// </summary>
  /// <param name="fs">Sampling frequcny in Hz</param>
  /// <param name="fc">Center frequcny in Hz</param>
  /// <param name="q">Q factor</param>
  /// <returns>A bi-quad low pass filter function</returns>
  let bqlpCoeff fs fc q = 
    (fun w alpha -> 
    let b1 = 1.0 - cos w
    (b1 / 2.0, b1, b1 / 2.0))
    |> biquadCoeff fs fc q

  let bqlp = bqlpCoeff |> buildFilter
  
  /// <summary>
  /// Bi-quad high pass filter
  /// </summary>
  /// <param name="fs">Sampling frequcny in Hz</param>
  /// <param name="fc">Center frequcny in Hz</param>
  /// <param name="q">Q factor</param>
  /// <returns>A bi-quad high pass filter function</returns>
  let bqhpCoeff fs fc q = 
    (fun w alpha -> 
    let b1 = -(1.0 + cos w)
    (-b1 / 2.0, b1, -b1 / 2.0))
    |> biquadCoeff fs fc q

  let bqhp = bqhpCoeff |> buildFilter
  
  /// <summary>
  /// Bi-quad band-pass filter
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="fc">Center frequency</param>
  /// <param name="q">Q factor</param>
  /// <returns>band-pass filter function</returns>
  let bpCoeff fs fc q = 
    (fun w alpha -> 
    let sinw = sin w
    (sinw / 2.0, 0.0, -sinw / 2.0))
    |> biquadCoeff fs fc q
  
  let bp = bpCoeff |> buildFilter

  /// <summary>
  /// Bi-quad notch filter
  /// </summary>
  /// <param name="fs">Sampling frequcny in Hz</param>
  /// <param name="fc">Center frequcny in Hz</param>
  /// <param name="q">Q factor</param>
  /// <returns>A bi-quad notch filter function</returns>
  let notchCoeff fs fc q = 
    (fun w alpha -> 
    let b1 = -2.0 * cos w
    (1.0, b1, 1.0))
    |> biquadCoeff fs fc q
  
  let notch = notchCoeff |> buildFilter

  /// <summary>
  /// Bi-quad all pass filter
  /// </summary>
  /// <param name="fs">Sampling frequcny in Hz</param>
  /// <param name="fc">Center frequcny in Hz</param>
  /// <param name="q">Q factor</param>
  /// <returns>A bi-quad all pass filter function</returns>
  let allpassCoeff fs fc q = 
    (fun w alpha -> (1.0 - alpha, -2.0 * cos w, 1.0 + alpha)) 
    |> biquadCoeff fs fc q

  let allpass = allpassCoeff |> buildFilter
  
  ///
  /// <summary>First order single pole low pass filter</summary>
  /// <param name="fs">sampling frequency</param>
  /// <param name="fc">cutoff frequencey</param>
  /// <returns>low pass filter function</returns>
  ///
  let lpCoeff fs fc = 
    let theta = 2.0 * System.Math.PI * fc / fs
    let b1 = -exp (-theta)
    let a0 = 1.0 + b1
    [ a0 ], [ b1 ]

  let lp = lpCoeff |> buildFilter2
  
  ///
  /// <summary>First order single pole high pass filter</summary>
  /// <param name="fs">sampling frequency</param>
  /// <param name="fc">cutoff frequencey</param>
  /// <returns>high pass filter function</returns>
  ///
  let hpCoeff fs fc = 
    let theta = 2.0 * System.Math.PI * fc / fs
    let k = tan (theta / 2.0)
    let alpha = 1.0 + k
    let a0 = 1.0
    let a1 = -(1.0 - k) / alpha
    let b0 = 1.0 / alpha
    let b1 = -1.0 / alpha
    [ b0; b1 ], [ a1 ]

  let hp = hpCoeff |> buildFilter2

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
    else cubicInterpolate buffer.[-1] buffer.[0] buffer.[1] buffer.[2] fraction
  
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
  /// <param name="repeat">Repeatedly push the input signal into the delay line
  /// the specified number of times.  Then push it in one final time and take
  /// the result as the return value.  This means that the input signal is run
  /// through the delay line once even when repeat is set to 0
  /// <returns>A function which takes a pair of samples and return a pair of
  /// samples output by the delay lines</returns>
  ///
  let pingpong fs bufferSec delayMs gain feedback wet repeat = 
    if wet < 0.0 || wet > 1.0 then failwith "wet must be between 0.0 and 1.0"
    let (bufferSize, delaySamples, fraction) = 
      calcDelayParams fs bufferSec delayMs
    let lBuf = CircularBuffer(bufferSize, delaySamples, (fun _ -> 0.0))
    let rBuf = CircularBuffer(bufferSize, delaySamples, (fun _ -> 0.0))
    
    let calc (l, r) = 
      // printfn "pp: %A" (l, r)
      let leftY = interpolateDelay delaySamples fraction r lBuf
      let rightY = interpolateDelay delaySamples fraction l rBuf
      lBuf.Push(gain * r + feedback * rightY)
      rBuf.Push(gain * l + feedback * leftY)
      (wet * leftY + (1.0 - wet) * l, wet * rightY + (1.0 - wet) * r)
    fun pair -> 
      for i in [ 0..(repeat - 1) ] do
        calc pair |> ignore
      calc pair
  
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
      let centerY = interpolateDelay delaySamples fractionalDelay (l + r) cBuf
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
  let modDelay fs bufferSec delayMs gain feedback wet lfo = 
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
    modDelay fs bufferSec maxDelayMs 1.0 feedback wet 
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
    modDelay fs bufferSec maxDelayMs 1.0 0.0 1.0 
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
    modDelay fs bufferSec maxDelayMs 1.0 0.0 wet 
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
  /// A simple but more efficient implementation of a sinusoid using a filter 
  /// and initial conditions.  It calls the more computational intensive sin 
  /// function exactly twice instead of for every sample
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="a">Amplitude</param>
  /// <param name="f">Frequency in Hz</param>
  let osc fs a f = 
    let theta = 2.0 * System.Math.PI * f / fs
    let b1 = -2.0 * cos (theta)
    let b2 = 1.0
    let init n = sin (float (-n - 1) * theta)
    let zero _ = 0.0
    filter_with_init [ 0.0 ] [ b1; b2 ] zero init >> (*) a
  
  /// <summary>
  /// Split the processing of a sample by passing it to two different functions
  /// producing a pair
  /// </summary>
  /// <param name="f1">Processing function 1</param>
  /// <param name="f2">Processing function 2</param>
  /// <param name="s">The sample value</param>
  /// <returns>A pair</returns>
  let split f1 f2 (s : float) : float * float = (f1 s, f2 s)
  
  /// <summary>
  /// Combine the result of a split by adding up the values in the pair
  /// </summary>
  /// <param name="s"></param>
  /// <param name="t"></param>
  let combine (s : float, t : float) = s + t
  
  /// <summary>
  /// Alternate between two signals at a specified frequency
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="f">Frequency of alternating between the two signals</param>
  /// <param name="s1">First signal</param>
  /// <param name="s2">Second signal</param>
  let alternate fs f (s1, s2) = 
    let lfo = 
      osc fs 100000.0 f
      >> clipper2 0.0 1.0
      >> bqlp fs 70.0 1.0
    fun t -> 
      let l = lfo t
      (1.0 - l) * (s1 t) + l * (s2 t)
  
  /// <summary>
  /// Schroeder reverb - stereo input
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="bufferSec"></param>
  /// <param name="dA">Delay in ms of first delay line</param>
  /// <param name="dB">Delay in ms of second delay line</param>
  /// <param name="dC">Delay in ms of third delay line</param>
  /// <param name="dD">Delay in ms of fourth delay line</param>
  /// <param name="gA">gain before pushing sum of sums into first delay line
  /// </param>
  /// <param name="gB">gain before pushing sum of differences into second delay
  /// line</param>
  /// <param name="gC">gain before pushing difference of sums into third delay
  /// line</param>
  /// <param name="gD">gain before pushing difference of differences into fourth
  /// delay line</param>
  /// <returns>A function which takes a pair of samples and returns a
  /// pair of samples representing the left and right channel of the schroeder
  /// reverb effect</returns>
  let schroeder2 fs bufferSec (dA, dB, dC, dD) (gA, gB, gC, gD) = 
    let zero = fun _ -> 0.0
    let quad = 
      Quad(dA, dB, dC, dD) |> Tuple.map(makeCircularBuffer fs bufferSec zero)
    match quad with
    | Quad(b1, b2, b3, b4) -> 
      fun (l, r) -> 
        let (oL, oR) = l + b1.Get(), r + b2.Get()
        let sum12 = oL + oR
        let diff12 = oL - oR
        let sum34 = b3.Get() + b4.Get()
        let diff34 = b3.Get() - b4.Get()
        b1.Push((sum12 + sum34) * gA)
        b2.Push((diff12 + diff34) * gB)
        b3.Push((sum12 - sum34) * gC)
        b4.Push((diff12 - diff34) * gD)
        (oL, oR)
    | _ -> failwith "This cannot happen"
  
  /// <summary>
  /// Schroeder reverb - single channel input
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="bufferSec"></param>
  /// <param name="dA">Delay in ms of first delay line</param>
  /// <param name="dB">Delay in ms of second delay line</param>
  /// <param name="dC">Delay in ms of third delay line</param>
  /// <param name="dD">Delay in ms of fourth delay line</param>
  /// <param name="gA">gain before pushing sum of sums into first delay line
  /// </param>
  /// <param name="gB">gain before pushing sum of differences into second delay
  /// line</param>
  /// <param name="gC">gain before pushing difference of sums into third delay
  /// line</param>
  /// <param name="gD">gain before pushing difference of differences into fourth
  /// delay line</param>
  /// <returns>A function which takes a (single channel) sample and returns a
  /// pair of samples representing the left and right channel of the schroeder
  /// reverb effect</returns>
  let schroeder fs bufferSec (dA, dB, dC, dD) (gA, gB, gC, gD) = 
    let r = schroeder2 fs bufferSec (dA, dB, dC, dD) (gA, gB, gC, gD)
    fun s -> r (s, s)

  /// <summary>
  /// Transfer function of a filter in real frequency domain
  /// 1.0 is pre-pended to the list of feedback coefficients
  /// </summary>
  /// <param name="fs">Sampling frequency</param>
  /// <param name="ffcoeff">List of feed-forward coefficients</param>
  /// <param name="fbcoeff">List of feed-back coefficients</param>
  /// <returns>The transfer function taking frequency as a parameter</returns>
  let transfer fs ffcoeff fbcoeff =
    let w = 2.0 * System.Math.PI / fs
    let term f i coeff = exp (Complex(0.0, -w * f * float i)) * coeff
    let sum cs = List.fold (+) (Complex(0.0, 0.0)) cs
    let toComplex f = Complex(f, 0.0)
    let ff = List.map toComplex ffcoeff
    let fb = List.map toComplex (1.0::fbcoeff)
    fun f ->
      let zeros = List.mapi (term f) ff
      let poles = List.mapi (term f) fb
      sum zeros / sum poles