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

module IO =

  open System.IO

  ///
  /// <summary>This type represents Samples which can be one of the three cases
  /// 1. Raw bytes
  /// 2. Bit8 which is a 8 bit sample (interpreted as signed byte)
  /// 3. Bit16 which is a 16-bit sample (interpreted as int16)
  /// </summary>
  /// 
  type Samples =
    | Raw of byte[]
    | Bit8 of sbyte[]
    | Bit16 of int16[]
    member s.Length = match s with
                      | Raw b -> b.Length
                      | Bit8 b -> b.Length
                      | Bit16 b-> b.Length
    member s.NumBytes = match s with
                        | Bit16 _ -> 2 * s.Length
                        | _ -> s.Length

  let samplesToBytes (samples:Samples) =
    match samples with
    | Raw bs -> bs
    | others -> use ms = new MemoryStream(samples.NumBytes)
                use writer = new BinaryWriter(ms)
                match others with
                | Bit8 bs -> bs |> Array.iter writer.Write
                             ms.GetBuffer()
                | Bit16 bs -> bs |> Array.iter writer.Write
                              ms.GetBuffer()
                | _ -> failwith "Should never have come here"

  let private genIndices numChannels bytesPerSample channel lengthSamples =
    let bytesPerChannel = bytesPerSample * numChannels
    let lengthSamples' = lengthSamples/bytesPerChannel*bytesPerChannel
    let offset = bytesPerSample*(channel%numChannels)
    seq { offset..(bytesPerChannel)..(lengthSamples'-1+offset) }

  /// <summary>Reads a PCM wave file and return a SoundFile object</summary>
  /// <param name="path">Path of the file</param>
  /// <returns>a SoundFile object representing the data in the wav file. If the
  /// file has only one channel, then the first of the tuple in Data contains
  /// the data and the second of the tuple is empty</returns>
  let private readWavFile (path: string) =
    use fileStream = new System.IO.FileStream( path, System.IO.FileMode.Open )
    use reader = new System.IO.BinaryReader(fileStream)
    // read chunk id - 4 bytes
    let id = reader.ReadBytes(4)
    if id <> "RIFF"B then
      failwith "Not a WAV file"
    // ignore next 12 bytes - chunk size: 4, format: 4, subChunkId: 4
    reader.ReadBytes(12) |> ignore

    let sizeChunk1 = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)
    let audioFormat = int(System.BitConverter.ToInt16(reader.ReadBytes(2), 0))
    let numChannels = int(System.BitConverter.ToInt16(reader.ReadBytes(2), 0))
    let samplingRate = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)

    // ignore byte rate: 4, block align: 2
    reader.ReadBytes(6) |> ignore
    let bitsPerSample = 
      int(System.BitConverter.ToInt16(reader.ReadBytes(2), 0))

    // ignore the rest of chunk 1
    reader.ReadBytes(sizeChunk1-16) |> ignore
      
    if int audioFormat <> 1 then
      // Non-PCM
      reader.ReadBytes(4) |> ignore
      let nExtraBytes = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)
      reader.ReadBytes(nExtraBytes) |> ignore

    let idChunk2 = reader.ReadBytes(4)
    if idChunk2 <> "data"B then
      failwith "Incorrect format: chunk 2 id not 'data'"
    let nBytes = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)  

    (float samplingRate, numChannels, bitsPerSample, (audioFormat = 1),
      (Raw (reader.ReadBytes(nBytes))))

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
  let private writeWavHeader samplingRate numChannels bytesPerSample 
    numBytes (writer:System.IO.BinaryWriter) =
    let chunkID = "RIFF"B
    let chunkSize = 36 + numBytes   // * seek and update after numBytes is known
    let format = "WAVE"B
    let subChunk1ID = "fmt "B
    let subChunk1Size = 16
    let audioFormat = 1s            // only support PCM at the moment
    let nc = int16 numChannels
    let bitsPerSample = int16 (bytesPerSample*8)
    let blockAlign = int16 (numChannels * bytesPerSample)
    let byteRate = samplingRate * numChannels * bytesPerSample
    let subChunk2Id = "data"B
    let subChunk2Size = numBytes    // * seek and update after numBytes is known
    writer.Write(chunkID)           // 0
    writer.Write(chunkSize)         // 4 (*)
    writer.Write(format)            // 8
    writer.Write(subChunk1ID)       // 12
    writer.Write(subChunk1Size)     // 16
    writer.Write(audioFormat)       // 20
    writer.Write(nc)                // 22
    writer.Write(samplingRate:int)  // 24
    writer.Write(byteRate)          // 28
    writer.Write(blockAlign)        // 32
    writer.Write(bitsPerSample)     // 34
    writer.Write(subChunk2Id)       // 36
    writer.Write(subChunk2Size)     // 40 (*)

  ///
  /// <summary>Writes raw data and associated properties to a PCM wav file
  /// </summary>
  /// <param name="path">path of the wav file.  Caution: if the file already
  /// exists, it will be overwritten</param>
  /// <param name="sf">sampling frequency - int</param>
  /// <param name="nc">number of channels - short/int16</param>
  /// <param name="bps">bits per sample</param>
  /// <param name="samples">Samples value</param>
  /// <returns>unit</returns>
  ///
  let private writeWavFile path (sf, nc, bps, samples: Samples) =
    use fileStream = new System.IO.FileStream(path, System.IO.FileMode.Create)
    use writer = new System.IO.BinaryWriter(fileStream)
    writeWavHeader sf nc (int bps/8) samples.NumBytes writer
    match samples with
    | Raw bs -> writer.Write(bs)
    | Bit8 bs -> bs |> Array.iter writer.Write 
    | Bit16 bs -> bs |> Array.iter writer.Write
  
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
  type SoundFile (samplingRate: float, 
                  numChannels: int, 
                  bitDepth: int,
                  isPCM: bool,
                  samples: Samples) =

    let read samples n = 
      match samples with
      | Raw bs -> match bitDepth with
                  | 8 -> bs.[n] |> int8 |> float
                  | 16 -> float(System.BitConverter.ToInt16(bs, n))
                  | _ -> failwith "Bit depth must be either 8 or 16"
      | Bit8 bs -> float bs.[n]
      | Bit16 bs -> float bs.[n]

    //
    // if samples is Raw, then we need bitDepth to calculate number of elements
    // in the array belonging to the same sample.  if samples is Bit8 or Bit16
    // then it means bitDepth is already taken into account in the array and
    // therefore the number of elements per sample is simply 1
    //
    let numElementsPerSample = match samples with
                               | Raw _ -> bitDepth / 8
                               | _ -> 1
    
    member f.SamplingRate = samplingRate
    member f.BitDepth = bitDepth
    member f.Samples = samples
    member f.IsPCM = isPCM
    member f.NumChannels = numChannels
    member f.Channels = [|
      for i in [0..(numChannels-1)] ->
      samples.Length
      |> genIndices numChannels numElementsPerSample i
      |> Seq.map (fun n -> read samples n) 
      |]
    member f.WriteWav path = 
      writeWavFile path (int f.SamplingRate, f.NumChannels, f.BitDepth, 
        f.Samples)
    // static member ReadWav (path:string) = readWavFile path |> SoundFile
    static member ReadWav (path:string) = SoundFile (readWavFile path)

  ///
  /// <summary>Utility function to write a SoundFile object to a wav file
  /// given a path</summary>
  /// <param name="path">path of the wav file to be written to. Caution: if the
  /// file already exists, it will be overwritten</param>
  /// <param name="soundFile">SoundFile object</param>
  /// <returns>unit</returns>
  ///
  let toWav path (soundFile:SoundFile) = soundFile.WriteWav path

  ///
  /// <summary>Utility function to convert a sequence of floats to a Samples
  /// value, ready to be written to a wav file by the WriteWav function in
  /// SoundFile</summary>
  /// <param name="f">sequence of floats to be converted</param>
  /// <returns>Samples value</returns>
  ///
  // Previously, floatTo16 converts to a byte array because that's the only way
  // samples are represented in SoundFile.  With the Samples discriminated union
  // however, samples can be represented by interpreted 8-bit or 16-bit samples
  // directly.  The WriteWav function then uses BinaryWriter which is capable of
  // writing sbyte or int16 directly.  This cuts down on a call to Seq.collect
  // used to flatten the byte[] returned from System.BitConverter.GetBytes.
  // Without this call, GC gen0 is cut down from 10 to 3 based on the
  // SignalTest script testcases
  //
  let floatTo16 (f:seq<float>) =
    f |> Seq.map int16 |> Seq.toArray |> Bit16

  ///
  /// <summary>Utility function to create a SoundFile instance, being a function
  /// it can be curried</summary>
  /// <param name="samplingRate">sampling rate - float</param>
  /// <param name="numChannels">number of channels - int</param>
  /// <param name="bitDepth">bits per sample - int</param>
  /// <param name="isPCM">whether the data is PCM format - bool</param>
  /// <param name="rawData">the raw data bytes - byte[]</param>
  /// <returns>a SoundFile object</returns>
  ///
  let makeSoundFile samplingRate numChannels bitDepth isPCM rawData =
    SoundFile(samplingRate, numChannels, bitDepth, isPCM, rawData)
   
  ///
  /// <summary>Clip a sample in float to fit the range of the specified depth
  /// in number of bytes</summary>
  /// <param name="byteDepth">bit depth but in number of bytes</param>
  /// <param name="sample">the sample value which is a float</param>
  /// <returns>clipped sample value that fits in the range of the number of
  /// bytes</returns>
  ///
  let clip byteDepth sample =   
    let range = 2.0**(float (abs byteDepth) * 8.0)
    let minValue = -range/2.0
    let maxValue = range/2.0 - 1.0
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
    for i in 0..(byteDepth-1) do 
      proc (byte (clipped >>> (i * 8) &&& (int64 0xFF)))

  ///
  /// <summary>Zip a list of sequence from http://fssnip.net/kz by Samuel Bosch
  /// </summary>
  /// <param name="lstSequences">the list of sequences to be zipped</param>
  /// <returns>A sequence of a list of elements of type in the sequences being
  /// zipped</returns>
  ///
  let zipSeq (lstSequences:seq<seq<'a>>) = 
    let enumerators = lstSequences 
                      |> Seq.map (fun (s:seq<'a>) -> (s.GetEnumerator()))
    seq {
      let hasNext() = enumerators 
                      |> Seq.exists (fun e -> not (e.MoveNext())) |> not
      while hasNext() do
        yield enumerators |> Seq.map (fun e -> e.Current)
    }

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
  /// <param name="samples">Sequence of sequence of samples. Each inner
  /// sequence represents one channel</param>
  /// <returns>unit</returns>
  ///
  let streamToWav samplingRate bytesPerSample path channels =
    use fileStream = new System.IO.FileStream(path, System.IO.FileMode.Create)
    use writer = new System.IO.BinaryWriter(fileStream)
    let mutable numSamples = 0
    let proc (b:byte) = writer.Write(b)
    let packChannels samples =
      Seq.iter (pack bytesPerSample proc) samples
      numSamples <- numSamples + 1
    let numChannels = Seq.length channels
    // write header
    writeWavHeader samplingRate numChannels bytesPerSample 0 writer
    // pack and write the stream
    Seq.iter packChannels (zipSeq channels)
    // now we should know the number of bytes
    let numBytes = numSamples * bytesPerSample * numChannels
    fileStream.Seek(4L, SeekOrigin.Begin) |> ignore
    writer.Write(36+numBytes)
    fileStream.Seek(32L, SeekOrigin.Current) |> ignore
    writer.Write(numBytes)