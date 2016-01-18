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
  open FSound.Plot
  
  /// <summary>
  /// Plot the spectrum of a sequence of samples and show the plot in a browser
  /// </summary>
  /// <param name="toFreq"></param>
  /// <param name="samples"></param>
  let plotSpectrum toFreq samples = plotSpectrum' Browser toFreq samples 
  
  /// <summary>
  /// Plot the impulse response of a filter in a browser
  /// </summary>
  let plotImpulse = plotImpulse' Browser
    
  /// <summary>
  /// Plot the magnitude response of a filter and show it in a browser
  /// </summary>
  let plotMagnitude = plotMagnitude' Browser
  
  /// <summary>
  /// Plot the phase response of a filter and show it in a browser
  /// </summary>
  let plotPhase = plotPhase' Browser