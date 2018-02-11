//
// FSound - F# Sound Processing Library
// Copyright (c) 2017 by Albert Pang <albert.pang@me.com> 
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

open FSound.Tonal

module Tuning =

  module ET =

    let baseNote = N A
    let baseNoteFreq = 440.0
    let semitoneFreq = 2.0 ** (1.0/12.0)

    /// <summary>
    /// This function converts a note into its frequency under equal temperament
    /// tuning
    /// </summary>
    /// <param name="note"></param>
    let note2Freq note =
      let dSemi = diffNotes baseNote note
      baseNoteFreq * (semitoneFreq ** (float)dSemi)

    /// <summary>
    /// This function converts a sequence of notes into a sequence of
    /// frequencies
    /// </summary>
    /// <param name="notes"></param>
    let notes2Freq notes = notes |> Seq.map note2Freq
      

