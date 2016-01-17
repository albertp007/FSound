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

module Plotly = 

  open FSound.Utilities
  open FSound.Filter
  open XPlot.Plotly
  open System.Numerics

  //
  // <summary>Run FFT on signal and plot frequency content using FSharp.Charting
  // </summary>
  // <param name="signal">Sequence of floats representing the samples</param>
  // <returns>unit</returns>
  //
  let plotSpectrum toFreq samples = 
    let y = samples
            |> fft
            |> magnitudes
            |> Seq.take (toFreq + 1)
    let x = seq {0..toFreq}
    [Scatter(x=x, y=y, mode="lines")] 
    |> Plotly.Plot 
    |> Plotly.Show
  
  ///
  /// <summary>Generate impulse response of a given length and pass through
  /// the filter.  Plot the response using FSharp.Charting</summary>
  /// <param name="n">the length of the impulse (inclusive of the initial 1)
  /// </param>
  /// <param name="filter">filter function</param>
  /// <returns>unit</returns>
  ///
  let plotImpulse n filter = 
    let y = impulseResponse n filter
    let x = Seq.zip {0..(Seq.length(y)-1)}
    [Scatter(x=x, y=y, mode="lines")]
    |> Plotly.Plot
    |> Plotly.Show

  /// <summary>
  /// Plot the frequency response of a filter given feedback and feedforward
  /// coefficients and a function that transforms the complex result before
  /// plotting
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="ffcoeff">Feedforward coefficients</param>
  /// <param name="fbcoeff">Feedback coefficients</param>
  /// <param name="func">Function which takes a complex number and returns
  /// a float</param>
  /// <param name="toFreq">Frequcncy to plot up to</param>
  let plotFreq fs ffcoeff fbcoeff func toFreq =
    let H = transfer fs ffcoeff fbcoeff
    let toFreq' = (float (int toFreq))

    let x = {0.0..toFreq}
    let y = Seq.map (H >> func) x
    [Scatter(x=x, y=y, mode="lines")]
    |> Plotly.Plot
    |> Plotly.Show
  
  /// <summary>
  /// Plot the magnitude response of a filter given its feedforward and
  /// feedback coefficients.  Note that 1.0 is appended to the list of
  /// feedback coefficients
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="ffcoeff">Feedforward coefficients</param>
  /// <param name="fbcoeff">Feedback coefficients</param>
  /// <param name="toFreq">Frequcncy to plot up to</param>  
  let plotMagnitude fs ffcoeff fbcoeff toFreq =
    plotFreq fs ffcoeff fbcoeff (Complex.Abs) toFreq

  /// <summary>
  /// Plot the phase response of a filter given its feedforward and
  /// feedback coefficients.  Note that 1.0 is appended to the list of
  /// feedback coefficients
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="ffcoeff">Feedforward coefficients</param>
  /// <param name="fbcoeff">Feedback coefficients</param>
  /// <param name="toFreq">Frequcncy to plot up to</param>
  let plotPhase fs ffcoeff fbcoeff toFreq =
    let phase (c:Complex) = c.Phase
    plotFreq fs ffcoeff fbcoeff phase toFreq




