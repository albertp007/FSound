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
module SignalTest

#I "bin/Debug"
#r "FSound.dll"

open FSound.Tests
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
Environment.SetEnvironmentVariable
  ("Path", 
    Environment.GetEnvironmentVariable("Path") + ";" + __SOURCE_DIRECTORY__ 
    + @"\packages\NAudio.Lame.1.0.3\content")

let main () =
  test()

