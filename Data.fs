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
  type CircularBuffer<'T> (n, lag, initValue:'T) =
    let buffer = Array.init n (fun i -> initValue)
    let size = n
    
    let calcPos m current =
      let toPos = m + current
      if toPos >= size then toPos % size 
      else if toPos < 0 then (size + toPos % size)
      else toPos
   
    let mutable posW = 0
    // read position lags behind write position by lag
    let mutable posR = calcPos (-lag) posW
    let mutable numSlot = n

    let moveIndex m = 
      posW <- calcPos m posW
      posR <- calcPos m posR

    /// do printfn "New instance of CircularBuffer"
    do if lag < 0 then failwith "Lag must be larger than or equal to zero"

    ///
    /// <summary>Push an item into the circular buffer</summary>
    ///
    member t.Push item = 
      buffer.[posW] <- item
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
    member t.Get() = buffer.[posR]
    ///
    /// <summary>Get the value of the current read position offset by n
    /// </summary>
    ///
    member t.GetOffset n = buffer.[(calcPos n posR)]
    ///
    /// <summary>Returns a copy of the buffer as an array</summary>
    ///
    member t.GetBuffer() = buffer
    ///
    /// <summary>Returns the current write index.  The item the current index is
    /// pointing at will be replaced with a new item by a call to Push()
    /// </summary>
    ///
    member t.CurrentWrite() = posW
    ///
    /// <summary>Returns the current read index.  The item the current index is
    /// pointing at will be returned by a call to Get()</summary>
    ///
    member t.CurrentRead() = posR
    ///
    /// <summary>Increase the spread between write index and read index
    /// i.e. move the read index backwards</summary>
    /// <param name="n">the number of items to move.  If n is positive, the
    /// spread between read and write is increased, i.e. the read index is moved
    /// backward from its current position.  Otherwise if n is negative, the
    /// spread is decreased and the read index is moved forward to be closer to
    /// the write index</param>
    ///
    member t.AddLag n =
      posR <- calcPos n posR

    ///
    /// <summary>Set the lag in terms of number of samples between the read 
    /// index and the write index</summary>
    ///
    member t.SetLag n =
      posR <- calcPos -(abs n) posW 

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
