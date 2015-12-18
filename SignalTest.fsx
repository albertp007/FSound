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
// Define your library scripting code here
// read the file back in
// a triangle wave with an adsr
// saw wave with smith angell resonator at 1024
// noise with resonator
// noise with resonator + adsr - the sound of, umm, hitting air?
// sound of waves using a low pass filter at 200Hz
// Vanilla delay effect
/// Chorus effect - the delay params taken from William Sharkey's Interior
/// Sounds [https://github.com/williamsharkey/William-FSound-Songs#1-interior-sounds---click-to-play]
/// Same as the chorus effect above but with a typo in the sampling frequency
/// in the delay and it turns out to sound completely different
// natural tuning
// natural tuning
module SignalTest

#I "bin/Debug"
#r "FSound.dll"

open FSound.Tests
open System

Environment.SetEnvironmentVariable
  ("Path", 
    Environment.GetEnvironmentVariable("Path") + ";" + __SOURCE_DIRECTORY__ 
    + @"\packages\NAudio.Lame.1.0.3\content")

let main () =
  test()

