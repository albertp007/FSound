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

module Data =

  open System.Collections.Generic

  /// <summary>Simple implementation of circular buffer using an array</summary>
  /// <param name="n">Number of slots in the window, used to initialize the
  /// size of the array used as buffer</param>
  /// <param name="initValue">initial value to populate every slot of the
  /// array during initialization</param>
  type CircularBuffer<'T> (n, initValue:'T) =
    let window = Array.init n (fun i -> initValue)
    let size = n
    let mutable current = 0
    let mutable numSlot = n
    let moveIndex m = current <- (current + m) % size
    do printfn "New instance of CircularBuffer"
    ///
    /// <summary>Push an item into the circular buffer</summary>
    ///
    member t.Push item = 
      window.[current] <- item
      moveIndex 1
      if numSlot > 0 then numSlot <- numSlot - 1
    ///
    /// <summary>Check if the buffer is fully populated</summary>
    ///
    member t.IsFull() = numSlot <= 0
    ///
    /// <summary>Get the value currently being pointed to and will be replaced
    /// by a call to push</summary>
    ///
    member t.Get() = window.[current]
    ///
    /// <summary>Returns a copy of the buffer as an array</summary>
    ///
    member t.GetBuffer() = window
    ///
    /// <summary>Returns the current index.  The item the current index is
    /// pointing at will be returned by a call to Get() and will be replaced
    /// with a new item by a call to Push()</summary>
    ///
    member t.CurrentIndex() = current

  /// <summary>Simple implementation of a moving window using .Net Queue<'T>
  /// </summary>
  type MovingWindow<'T> (init:seq<'T>) =
    let window = Queue<'T>(init)
    let size = window.Count
    do printfn "New instance of MovingWindow"
    ///
    /// <summary>Push an item into the window. If the queue is already full,
    /// an item will first be dequeued before the new item is pushed into it
    /// </summary>
    ///
    member t.Push item =
      window.Dequeue() |> ignore
      window.Enqueue( item )
      item

    ///
    /// <summary>Converts the queue object representing the window to a sequence
    /// Note that the first element of the seq is the oldest one</summary>
    ///
    member t.Get() = window :> seq<'T>

    ///
    /// <summary>Converts the queue object representing the window to an array
    /// Note that the first element of the array is the oldest one</summary>
    ///
    member t.GetArray() = window.ToArray()

    ///
    /// <summary>Get the count of elements in the window</summary>
    ///
    member t.Count() = window.Count

    ///
    /// <summary>Checks if the window is fully populated</summary>
    ///
    member t.IsFull() = ( t.Count() = size )
