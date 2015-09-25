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