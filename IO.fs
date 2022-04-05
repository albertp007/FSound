﻿//
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
namespace FSound

module IO = 
  open System.IO
  open System.Collections.Generic
  
  ///
  /// <summary>Zip a list of sequence from http://fssnip.net/kz by Samuel Bosch
  /// </summary>
  /// <param name="lstSequences">the list of sequences to be zipped</param>
  /// <returns>A sequence of a list of elements of type in the sequences being
  /// zipped</returns>
  ///
  let zipSeq (lstSequences : list<seq<'a>>) = 
    let enumerators = 
      lstSequences |> List.map (fun (s : seq<'a>) -> (s.GetEnumerator()))
    seq { 
      let hasNext() = 
        enumerators
        |> List.exists (fun e -> not (e.MoveNext()))
        |> not
      while hasNext() do
        yield enumerators |> List.map (fun e -> e.Current)
    }
  
  ///
  /// <summary>Clip a sample in float to fit the range of the specified depth
  /// in number of bytes</summary>
  /// <param name="byteDepth">bit depth but in number of bytes</param>
  /// <param name="sample">the sample value which is a float</param>
  /// <returns>clipped sample value that fits in the range of the number of
  /// bytes</returns>
  ///
  let clip byteDepth sample = 
    let range = 2.0 ** (float (abs byteDepth) * 8.0)
    let minValue = -range / 2.0
    let maxValue = range / 2.0 - 1.0
    min (max minValue sample) maxValue
  
  ///
  /// <summary>Pack a sample in float to the number of bytes specified. Only
  /// support up to 8 bytes</summary>
  /// <param name="byteDepth">number of bytes to fit the sample in. Any sample
  /// falling outside of the range supported by the specified number of bytes
  /// is clipped</param>
  /// <param name="sample">the sample value which is a float</param>
  /// <returns>A sequence of (unsigned) bytes of length specified in byteDepth
  /// </returns>
  ///
  let pack byteDepth proc sample = 
    if abs byteDepth > 8 then failwith "Depth larger than 8 bytes not supported"
    let clipped = int64 (clip byteDepth sample)
    for i in 0..(byteDepth - 1) do
      proc (byte (clipped >>> (i * 8) &&& (int64 0xFF)))
  
  ///
  /// <summary>Convert a byte array to float. Most significant byte is assumed
  /// to be at the end of the array (Little endian). This function is the
  /// reciprocal of the pack function. Bytes are first converted to an int64
  /// then casted to float.  As such, array larger than 8 elements is not
  /// supported</summary>
  /// <param name="bytes">Array of bytes to convert to float</param>
  /// <returns>Float value converted from array of bytes</returns>
  ///
  let bytesToFloat (bytes : byte []) = 
    let maxByte = 8
    let length = Array.length bytes
    if length > maxByte then failwith "Does not support more than 8 bytes"
    let isNegative = bytes.[length - 1] &&& 0x80uy > 0uy
    
    let init = 
      if isNegative then -1L
      else 0L
    
    let rec pushShift (init : int64) (bs : byte list) = 
      match bs with
      | [] -> init
      | h :: t -> pushShift (init <<< 8 ||| (int64 h)) t
    
    bytes
    |> Array.rev
    |> Array.toList
    |> pushShift init
    |> float
  
  /// <summary>
  /// Enumerator for SampleSeq type
  /// </summary>
  type SampleSeqEnumerator<'a> = 
    | MonoEnum of IEnumerator<'a>
    | StereoEnum of IEnumerator<'a> * IEnumerator<'a>
    | Stereo2Enum of IEnumerator<'a * 'a>
    | MultiEnum of IEnumerator<'a> list

    /// <summary>
    /// Calls MoveNext() on all the enumerators of sequences in the SampleSeq
    /// </summary>
    member e.MoveNext() = 
      match e with
      | MonoEnum e -> e.MoveNext()
      | StereoEnum(l, r) -> l.MoveNext() && r.MoveNext()
      | Stereo2Enum e -> e.MoveNext()
      | MultiEnum e -> 
        e
        |> List.exists (fun x -> not (x.MoveNext()))
        |> not
  
  ///
  /// <summary>Abstract sequence of samples for mono (sequence of floats), 
  /// stereo (a pair of sequences of floats) and multi-channel (a list of
  /// sequence of floats)</summary>
  ///
  type SampleSeq = 
    | Mono of seq<float>
    | Stereo of seq<float> * seq<float>
    | Stereo2 of seq<float * float>
    | Multi of seq<float> list
    
    ///
    /// <summary>Member function which packs the sequence of samples into a
    /// writer</summary>
    /// <param name="byteDepth">Bit depth in number of bytes</param>
    /// <param name="writer">Binary writer to write the bytes to</param>
    /// <returns>Number of writes written</returns>
    ///
    member x.Pack byteDepth (writer : BinaryWriter) = 
      let bytesWritten = ref 0
      
      let proc (b : byte) = 
        writer.Write(b)
        bytesWritten := !bytesWritten + 1
      
      // pack one single sample
      let pack1 = pack byteDepth proc
      
      // pack left sample, then right sample
      let pack2 (l, r) = 
        pack byteDepth proc l
        pack byteDepth proc r
      
      // pack a sequence of samples, each sample in a separate channel
      let packN channels = Seq.iter pack1 channels
      match x with
      | Mono sequence -> Seq.iter pack1 sequence
      | Stereo(left, right) -> Seq.iter pack2 (Seq.zip left right)
      | Stereo2 seq -> Seq.iter pack2 seq
      | Multi channels -> Seq.iter packN (zipSeq channels)
      !bytesWritten
    
    ///
    /// <summary>Number of channels property</summary>
    ///
    member x.NumChannels = 
      match x with
      | Mono _ -> 1
      | Stereo _ | Stereo2 _ -> 2
      | Multi s -> Seq.length s
    
    /// <summary>
    /// Create an instance of SampleSeqEnumerator
    /// </summary>
    member x.GetEnumerator() = 
      match x with
      | Mono s -> MonoEnum(s.GetEnumerator())
      | Stereo(l, r) -> StereoEnum(l.GetEnumerator(), r.GetEnumerator())
      | Stereo2 s -> Stereo2Enum(s.GetEnumerator())
      | Multi s -> MultiEnum(s |> List.map (fun x -> x.GetEnumerator()))
  
  ///
  /// <summary>Converts a list of sequences to a SampleSeq object</summary>
  /// <param name="sequences">List of sequences to be converted</param>
  /// <returns>a SampleSeq object</returns>
  ///
  let seqToSampleSeq sequences = 
    match sequences with
    | [] -> failwith "List of sequences must not be empty"
    | [ mono ] -> Mono mono
    | [ left; right ] -> Stereo(left, right)
    | multi -> Multi multi
  
  ///
  /// <summary>Converts raw bytes read from a wav file to SampleSeq object
  /// </summary>
  /// <param name="numChannels">Number of channels in the raw bytes</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="raw">Raw bytes to be converted</param>
  /// <returns>SampleSeq object</returns>
  ///
  let rawBytesToSampleSeq numChannels bytesPerSample (raw : byte []) = 
    let length = raw.Length
    let numSamples = length / numChannels / bytesPerSample
    let bytesPerChannel = bytesPerSample * numChannels
    
    let s = 
      seq { 
        for i in [ 0..(numChannels - 1) ] -> 
          seq { 
            for j in [ (i * bytesPerSample)..bytesPerChannel..(numSamples 
                                                               * bytesPerChannel 
                                                               - 1) ] do
              yield raw.[j..(j + bytesPerSample - 1)] |> bytesToFloat
          }
      }
    Seq.toList s |> seqToSampleSeq
  
  ///
  /// <summary>Reads a PCM wave file and return a SoundFile object</summary>
  /// <param name="path">Path of the file</param>
  /// <returns>a SoundFile object representing the data in the wav file. If the
  /// file has only one channel, then the first of the tuple in Data contains
  /// the data and the second of the tuple is empty</returns>
  ///
  let readWavFile (path : string) = 
    use fileStream = new System.IO.FileStream(path, System.IO.FileMode.Open)
    use reader = new System.IO.BinaryReader(fileStream)
    // read chunk id - 4 bytes
    let id = reader.ReadBytes(4)
    if id <> "RIFF"B then failwith "Not a WAV file"
    // ignore next 12 bytes - chunk size: 4, format: 4, subChunkId: 4
    reader.ReadBytes(12) |> ignore
    let sizeChunk1 = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)
    let audioFormat = int (System.BitConverter.ToInt16(reader.ReadBytes(2), 0))
    let numChannels = int (System.BitConverter.ToInt16(reader.ReadBytes(2), 0))
    let samplingRate = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)
    // ignore byte rate: 4, block align: 2
    reader.ReadBytes(6) |> ignore
    let bitsPerSample = 
      int (System.BitConverter.ToInt16(reader.ReadBytes(2), 0))
    // ignore the rest of chunk 1
    reader.ReadBytes(sizeChunk1 - 16) |> ignore
    if int audioFormat <> 1 then 
      // Non-PCM
      reader.ReadBytes(4) |> ignore
      let nExtraBytes = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)
      reader.ReadBytes(nExtraBytes) |> ignore
    let idChunk2 = reader.ReadBytes(4)
    if idChunk2 <> "data"B then 
      failwith "Incorrect format: chunk 2 id not 'data'"
    let nBytes = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)
    let raw = reader.ReadBytes(nBytes)
    let bytesPerSample = bitsPerSample / 8
    (float samplingRate, bytesPerSample, (audioFormat = 1), 
     (rawBytesToSampleSeq numChannels bytesPerSample raw))
  
  ///
  /// <summary>Private helper function which writes the header of a wav file
  /// to a file stream via BinaryWriter</summary>
  /// <param name="samplingRate">Sampling rate in Hz</param>
  /// <param name="numChannels">Number of channels</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="numBytes">Total number of sample bytes written. This should
  /// be equal to number of samples times bytesPerSample * numChannels</param>
  /// <returns>unit</returns>
  ///
  let writeWavHeader samplingRate numChannels bytesPerSample numBytes 
      (writer : System.IO.BinaryWriter) = 
    let chunkID = "RIFF"B
    let chunkSize = 36 + numBytes // * seek and update after numBytes is known
    let format = "WAVE"B
    let subChunk1ID = "fmt "B
    let subChunk1Size = 16
    let audioFormat = 1s // only support PCM at the moment
    let nc = int16 numChannels
    let bitsPerSample = int16 (bytesPerSample * 8)
    let blockAlign = int16 (numChannels * bytesPerSample)
    let byteRate = samplingRate * numChannels * bytesPerSample
    let subChunk2Id = "data"B
    let subChunk2Size = numBytes // * seek and update after numBytes is known
    writer.Write(chunkID) // 0
    writer.Write(chunkSize) // 4 (*)
    writer.Write(format) // 8
    writer.Write(subChunk1ID) // 12
    writer.Write(subChunk1Size) // 16
    writer.Write(audioFormat) // 20
    writer.Write(nc) // 22
    writer.Write(samplingRate : int) // 24
    writer.Write(byteRate) // 28
    writer.Write(blockAlign) // 32
    writer.Write(bitsPerSample) // 34
    writer.Write(subChunk2Id) // 36
    writer.Write(subChunk2Size) // 40 (*)
  
  ///
  /// <summary>Pack a sequence of samples. It simply forwards to the member
  /// function to make pipelining easier</summary>
  /// <param name="byteDepth">bit depth in number of bytes</param>
  /// <param name="writer">Binary writer to write bytes to</param>
  /// <param name="samples">SampleSeq</param>
  ///
  let packSampleSequence byteDepth writer (samples : SampleSeq) = 
    samples.Pack byteDepth writer
  
  ///
  /// <summary>Stream a sequence of samples to a wav format file</summary>
  /// <param name="samplingRate">Sampling rate in Hz</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="path">Path of the wav file to be created.  N.B. Any existing
  /// file is overwritten!</param>
  /// <returns>unit</returns>
  ///
  let streamSeqToWav samplingRate bytesPerSample path (samples : SampleSeq) = 
    use fileStream = new System.IO.FileStream(path, System.IO.FileMode.Create)
    use writer = new System.IO.BinaryWriter(fileStream)
    let numChannels = samples.NumChannels
    // write header
    writeWavHeader samplingRate numChannels bytesPerSample 0 writer
    // pack and write the stream
    let bytesWritten = packSampleSequence bytesPerSample writer samples
    // now we should know the number of bytes
    fileStream.Seek(4L, SeekOrigin.Begin) |> ignore
    writer.Write(36 + bytesWritten)
    fileStream.Seek(32L, SeekOrigin.Current) |> ignore
    writer.Write(bytesWritten)
  
  ///
  /// <summary>Stream a list of sequence of samples to a wave file.  Each
  /// sequence in the list represents one channel.  This function iterates
  /// through each of the sample sequence and thus will evaulate them in their
  /// entirety after it is done.  However, as and after each sample in each 
  /// channel is evaluated, it is converted to bytes and written to file
  /// and thus will avoid the memory overhead of first converting the whole
  /// sequence to an array if a SoundFile object is used instead.  If the raw
  /// sample value falls out of the range of the byteDepth, it will be clipped.
  /// For long sequence of samples e.g. a mix down, this function should be 
  /// preferred over using the SoundFile object.  Note however, the sequence 
  /// itself still occupies memory after evaluation completes</summary>
  /// <param name="samplingRate">Sampling rate in Hz</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="path">Path of the wav file to be created.  N.B. Any existing
  /// file is overwritten!</param>
  /// <param name="channels">Sequence of sequence of samples. Each inner
  /// sequence represents one channel</param>
  /// <returns>unit</returns>
  ///
  let streamToWavMultiple samplingRate bytesPerSample path channels = 
    streamSeqToWav samplingRate bytesPerSample path (Multi channels)
  
  ///
  /// <summary>Stream one sequence of samples to a wav file.  There is only
  /// one channel, hence "Mono".  This is similar to streamToWavMultiple but is
  /// less memory-hungry since the number of channels is fixed to 1, there is no
  /// overhead required to run map for the case when the number of sequence of
  /// samples isn't known before hand.  This uses 15% of the memory 
  /// footprint of streamToWav according to the .Net memory profiling tool
  /// </summary>
  /// <param name="samplingRate">Sampling rate in Hz</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="path">Path of the wav file to be created.  N.B. Any existing
  /// file is overwritten!</param>
  /// <param name="samples">sequence of samples</param>
  /// <returns>unit</returns>
  ///
  let streamToWavMono samplingRate bytesPerSample path samples = 
    streamSeqToWav samplingRate bytesPerSample path (Mono samples)
  
  ///
  /// <summary>Stream two sequence of samples to a wav file - left and right
  /// hence "LR".  This is similar to streamToWav but is less 
  /// memory-hungry since the number of channels is fixed to 1, there is no
  /// overhead required to run map for the case when the number of sequence of
  /// samples isn't known before hand.  This uses only 35% of the memory 
  /// footprint of streamToWav according to the .Net memory profiling tool
  /// </summary>
  /// <param name="samplingRate">Sampling rate in Hz</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="path">Path of the wav file to be created.  N.B. Any existing
  /// file is overwritten!</param>
  /// <param name="samples">list of sequence of samples</param>
  /// <returns>unit</returns>
  ///
  let streamToWavLR samplingRate bytesPerSample path (left, right) = 
    streamSeqToWav samplingRate bytesPerSample path (Stereo(left, right))
  
  ///
  /// <summary>Maps function to each element in a pair for facilitating the use
  /// of the streamToWavLR function for which the left and right sequence of 
  /// samples are represented as a pair which types check better than using a
  /// list</summary>
  /// <param name="f">function to be applied</param>
  /// <param name="a">first element in the pair</param>
  /// <param name="b">second element in the pair</param>
  /// <returns>A pair of elements which are the results of applying the function
  /// f to the original elements in the input pair</returns>
  ///
  let pairMap f (a, b) = (f a, f b)
  
  ///
  /// <summary>Chooser function which picks a more memory efficient version of
  /// the stream function according to the number of channels</summary>
  /// <param name="samplingRate">Sampling rate in Hz</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="path">Path of the wav file to be created.  N.B. Any existing
  /// file is overwritten!</param>
  /// <param name="channels">Sequence of sequence of samples. Each inner
  /// sequence represents one channel</param>
  /// <returns>unit</returns>
  ///
  let streamToWav samplingRate bytesPerSample path channels = 
    match channels with
    | [] -> ()
    | [ mono ] -> streamToWavMono samplingRate bytesPerSample path mono
    | [ l; r ] -> streamToWavLR samplingRate bytesPerSample path (l, r)
    | rest -> streamToWavMultiple samplingRate bytesPerSample path rest
  
  let streamPairsToWav samplingRate bytesPerSample path channels = 
    streamSeqToWav samplingRate bytesPerSample path (Stereo2 channels)
  
  ///
  /// <summary>Type to represent a sound file</summary>
  /// <param name="samplingRate">sampling frequency</param>
  /// <param name="numChannels">number of channels</param>
  /// <param name="bitDepth">number of bits per sample</param>
  /// <param name="isPCM">whether the format is PCM, for writing, it must be
  /// true</param>
  /// <param name="samples">A Samples value which can be either Raw byte[] or
  /// interpreted as 8-bit or 16-bit signed sample. If there are more than one
  /// channels, sample for each channel is concatenated one after the other
  /// <returns>SoundFile object</returns>
  ///
  type SoundFile(samplingRate : float, bytesPerSample : int, isPCM : bool, samples : SampleSeq) = 
    member f.SamplingRate = samplingRate
    member f.BytesPerSample = bytesPerSample
    member f.Samples = samples
    member f.IsPCM = isPCM
    member f.NumChannels = samples.NumChannels
    member f.WriteWav path = 
      streamSeqToWav (int f.SamplingRate) f.BytesPerSample path f.Samples
    static member ReadWav(path : string) = SoundFile(readWavFile path)
  
  /// <summary>
  /// Wrapping a SampleSeq object with a read-only stream interface
  /// </summary>
  /// <param name="samples">The SampleSeq object</param>
  /// <param name="convert">A function to convert the type stored in the
  /// sequences to the type to be written to the read buffer</param>
  /// <returns>A SampleSeqStream object</returns>
  type SampleSeqStream(samples : SampleSeq, convert) = 
  
    let nChannels = samples.NumChannels
    let es = samples.GetEnumerator()
  
    /// <summary>
    /// Read a chunk of size specified by count from the sample sequence
    /// and write that to the buffer array provided
    /// </summary>
    /// <param name="buffer"></param>
    /// <param name="offset"></param>
    /// <param name="count"></param>
    member p.Read(buffer : float32 [], offset, count) = 
      let mutable index = offset
      let mutable n = count / nChannels
      while n > 0 && es.MoveNext() do
        let nWritten =
          match es with
          | MonoEnum e -> 
            buffer.[index] <- convert e.Current
            1
          | StereoEnum(l, r) -> 
            buffer.[index] <- convert l.Current
            buffer.[index + 1] <- convert r.Current
            2
          | Stereo2Enum e -> 
            let (l, r) = e.Current
            buffer.[index] <- convert l
            buffer.[index + 1] <- convert r
            2
          | MultiEnum e -> 
            e |> List.iteri (fun i x -> buffer.[index + i] <- convert x.Current)
            List.length e
        index <- index + nWritten
        n <- n - 1
      index - offset
