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

module Plot = 
  open XPlot.GoogleCharts
  open XPlot.GoogleCharts.WpfExtensions
  open XPlot.Plotly
  open FSound.Utilities
  open FSound.Filter
  open System.Numerics
  
  let Show(chart : GoogleChart) = chart.Show()
  
  /// <summary>
  /// Calculate the data points for the spectrum of the samples up to the
  /// specified frequency
  /// </summary>
  /// <param name="toFreq">Spectrum data up to toFreq in Hz</param>
  /// <param name="samples">Sequence of samples</param>
  /// <returns>A pair of sequences representing the x and y data points
  /// </returns>
  let calcSpectrum (toFreq: float) samples = 
    let toFreq' = int toFreq
    ({ 0.0..(float toFreq' + 1.0) }, 
     samples
     |> fft
     |> magnitudes
     |> Seq.take (toFreq' + 1))
  
  /// <summary>
  /// Calculate the data points for the frequency response of a filter given
  /// its feed-forward and feedback coefficients
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="ffcoeff">Feed-forward coefficients</param>
  /// <param name="fbcoeff">Feedback coefficients</param>
  /// <param name="func">A conversion function from complex to float, the
  /// result of which will be the y axis data points</param>
  /// <param name="toFreq">Calculate up to toFreq</param>
  /// <returns>A pair of sequences representing the x and y data points
  /// </returns>
  let calcFreqRes fs ffcoeff fbcoeff func (fromFreq: float) (toFreq: float) = 
    let H = transfer fs ffcoeff fbcoeff
    { fromFreq..toFreq }, Seq.map (H >> func) { fromFreq..toFreq }
  
  /// <summary>
  /// Calculate the data points for the magnitude response of a filter given
  /// its feed-forward and feedback coefficients
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="ffcoeff">Feed-forward coefficients</param>
  /// <param name="fbcoeff">Feedback coefficients</param>
  /// result of which will be the y axis data points</param>
  /// <param name="toFreq">Calculate up to toFreq</param>
  /// <returns>A pair of sequences representing the x and y data points
  /// </returns>
  let calcMagnitude fs ffcoeff fbcoeff toFreq = 
    calcFreqRes fs ffcoeff fbcoeff (Complex.Abs
                                    >> log10
                                    >> ((*) 20.0)) toFreq
  
  /// <summary>
  /// Calculate the data points for the phase response of a filter given
  /// its feed-forward and feedback coefficients
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="ffcoeff">Feed-forward coefficients</param>
  /// <param name="fbcoeff">Feedback coefficients</param>
  /// result of which will be the y axis data points</param>
  /// <param name="toFreq">Calculate up to toFreq</param>
  /// <returns>A pair of sequences representing the x and y data points
  /// </returns>
  let calcPhase fs ffcoeff fbcoeff toFreq = 
    calcFreqRes fs ffcoeff fbcoeff 
      (fun c -> 180.0 * (atan2 c.Imaginary c.Real) / System.Math.PI) toFreq
      
  /// <summary>
  /// Calculate the data points for the group delay of a filter
  /// </summary>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="ffcoeff">Feed-forward coefficients</param>
  /// <param name="fbcoeff">Feedback coefficients</param>
  /// <param name="toFreq">Calculate up to toFreq</param>
  let calcGroupDelay fs ffcoeff fbcoeff toFreq =
    let phase fromFreq toFreq = calcPhase fs ffcoeff fbcoeff fromFreq toFreq
    let (x0, y0) = phase 0.0 toFreq
    let windowed = Seq.windowed 2 y0
    let y' = windowed |> Seq.map (fun a -> (a.[0] - a.[1]) * toFreq / 180.0)
    (x0, 0.0::(Seq.toList y'))

  /// <summary>
  /// Calculate the data points for the impulse response of a filter
  /// </summary>
  /// <param name="n"></param>
  /// <param name="filter"></param>
  /// <returns>A pair of sequences representing the x and y data points
  /// </returns>
  let calcImpulse n filter = { 0..(n - 1) }, impulseResponse n filter
  
  type PlotContainer = 
    | Window
    | Browser
  
  /// <summary>
  /// Make a 2D line plot and show it in a window using Google Chart
  /// </summary>
  /// <param name="title">Title of the plot</param>
  /// <param name="titleX">Title of the x axis</param>
  /// <param name="titleY">Title of the y axis</param>
  /// <param name="x">Sequence of data points for the x-axis</param>
  /// <param name="y">Sequence of data points for the y-axis</param>
  let plot2dwpf title titleX titleY (x, y) = 
    let xaxis = Axis()
    let yaxis = Axis()
    xaxis.title <- titleX
    yaxis.title <- titleY
    let options = 
      Options
        (curveType = "function", title = title, hAxis = xaxis, vAxis = yaxis)
    Seq.zip x y
    |> Chart.Line
    |> Chart.WithOptions options
    |> Chart.Show
    |> ignore

  /// <summary>
  /// Make a 2D line plot and show it in the browser using PlotLy
  /// </summary>
  /// <param name="title">Title of the plot</param>
  /// <param name="titleX">Title of the x axis</param>
  /// <param name="titleY">Title of the y axis</param>
  /// <param name="x">Sequence of data points for the x-axis</param>
  /// <param name="y">Sequence of data points for the y-axis</param>
  let plotly2d title titleX titleY (x, y) = 
    let layout = Layout()
    layout.title <- title
    let yaxis = Yaxis()
    yaxis.title <- titleY
    layout.yaxis <- yaxis
    [ Scatter(x = x, y = y, mode = "lines") ]
    |> fun data -> Plotly.Plot(data, layout)
    |> Plotly.Show
  
  /// <summary>
  /// Convenient function to make a 2D plot and show either in a window or in a
  /// browser depending on the container type being passed in
  /// </summary>
  /// <param name="container">Either window or browser</param>
  /// <param name="title">Title of the plot</param>
  /// <param name="titleX">Title of the x axis</param>
  /// <param name="titleY">Title of the y axis</param>
  /// <param name="data">Pair of sequences</param>
  let plot2d container title titleX titleY data = 
    match container with
    | Window -> plot2dwpf title titleX titleY data
    | Browser -> plotly2d title titleX titleY data

  /// <summary>
  /// Plot the spectrum of a sequence of samples
  /// </summary>
  /// <param name="container">Either window or browser</param>
  /// <param name="toFreq"></param>
  /// <param name="samples"></param>
  let plotSpectrum' container toFreq samples = 
    calcSpectrum toFreq samples 
    |> plot2d container "Frequency spectrum" "Frequency (Hz)" ""
  
  /// <summary>
  /// Plot the spectrum of a sequence of samples and show the plot in a window
  /// </summary>
  /// <param name="toFreq"></param>
  /// <param name="samples"></param>
  let plotSpectrum toFreq samples = plotSpectrum' Window toFreq samples

  /// <summary>
  /// Plot the impulse response of a filter
  /// </summary>
  /// <param name="container">Either window or browser</param>
  /// <param name="n"></param>
  /// <param name="filter"></param>
  let plotImpulse' container n filter = 
    calcImpulse n filter |> plot2d container "Impulse response" "" ""
  
  /// <summary>
  /// Plot the impulse response of a filter in a window
  /// </summary>
  let plotImpulse = plotImpulse' Window
  
  /// <summary>
  /// Plot the magnitude response of a filter given its feedforward and
  /// feedback coefficients.  Note that 1.0 is appended to the list of
  /// feedback coefficients
  /// </summary>
  /// <param name="container">Either window or browser</param>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="ffcoeff">Feedforward coefficients</param>
  /// <param name="fbcoeff">Feedback coefficients</param>
  /// <param name="toFreq">Frequcncy to plot up to</param>
  let plotMagnitude' container fs toFreq (ffcoeff, fbcoeff) = 
    let title = 
      sprintf "Frequency response\nff=%A\nfb=%A\nfs=%f" ffcoeff fbcoeff fs
    calcMagnitude fs ffcoeff fbcoeff 0.0 toFreq 
    |> plot2d container title "Frequency (Hz)" "Magnitude (dB)"
  
  /// <summary>
  /// Plot the magnitude response of a filter and show it in a window
  /// </summary>
  let plotMagnitude = plotMagnitude' Window
  
  /// <summary>
  /// Plot the phase response of a filter given its feedforward and
  /// feedback coefficients.  Note that 1.0 is appended to the list of
  /// feedback coefficients
  /// </summary>
  /// <param name="container">Either window or browser</param>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="ffcoeff">Feedforward coefficients</param>
  /// <param name="fbcoeff">Feedback coefficients</param>
  /// <param name="toFreq">Frequcncy to plot up to</param>
  let plotPhase' container fs toFreq (ffcoeff, fbcoeff) = 
    let title = 
      sprintf "Frequency response\nff=%A\nfb=%A\nfs=%f" ffcoeff fbcoeff fs
    calcPhase fs ffcoeff fbcoeff 0.0 toFreq 
    |> plot2d container title "Frequency (Hz)" "Angle (degrees)"
  
  /// <summary>
  /// Plot the phase response of a filter and show it in a window
  /// </summary>
  let plotPhase = plotPhase' Window

  /// <summary>
  /// Plot the group delay of a filter
  /// </summary>
  /// <param name="container">Either window or browser</param>
  /// <param name="fs">Sampling frequency in Hz</param>
  /// <param name="ffcoeff">Feed-forward coefficients</param>
  /// <param name="fbcoeff">Feedback coefficients</param>
  /// <param name="toFreq">Frequency to plot up to</param>
  let plotGroupDelay' container fs toFreq (ffcoeff, fbcoeff) =
    calcGroupDelay fs ffcoeff fbcoeff toFreq
    |> plot2d container "Group Delay" "Frequency (Hz)" "Group Delay (samples)"

  /// <summary>
  /// Plot the group delay of a filter and show it in a window
  /// </summary>
  let plotGroupDelay = plotGroupDelay' Window
