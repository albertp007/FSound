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

  //
  // <summary>Run FFT on signal and plot frequency content using FSharp.Charting
  // </summary>
  // <param name="signal">Sequence of floats representing the samples</param>
  // <returns>unit</returns>
  //
  let plotFreq toFreq samples = 
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


