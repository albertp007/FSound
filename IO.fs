namespace FSound

module IO =

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
      reader.ReadBytes(nBytes))

  ///
  /// <summary>Writes raw data and associated properties to a PCM wav file
  /// </summary>
  /// <param name="path">path of the wav file.  Caution: if the file already
  /// exists, it will be overwritten</param>
  /// <param name="sf">sampling frequency - int</param>
  /// <param name="nc">number of channels - short/int16</param>
  /// <param name="bps">bits per sample</param>
  /// <param name="raw">byte array as raw data</param>
  /// <returns>unit</returns>
  ///
  let private writeWavFile path (sf, nc, bps, raw) =
    let chunkID = "RIFF"B
    let chunkSize = 36 + Array.length raw
    let format = "WAVE"B
    let subChunk1ID = "fmt "B
    let subChunk1Size = 16
    let audioFormat = 1s // only support PCM at the moment
    let numChannels = int16 nc
    let samplingRate = sf
    let bitsPerSample = int16 bps
    let blockAlign = int16 (nc * bps  / 8)
    let byteRate = samplingRate * nc * bps /8
    let subChunk2Id = "data"B
    let subChunk2Size = Array.length raw
    use fileStream = new System.IO.FileStream(path, System.IO.FileMode.Create)
    use writer = new System.IO.BinaryWriter(fileStream)
    writer.Write(chunkID)
    writer.Write(chunkSize)
    writer.Write(format)
    writer.Write(subChunk1ID)
    writer.Write(subChunk1Size)
    writer.Write(audioFormat)
    writer.Write(numChannels)
    writer.Write(samplingRate:int)
    writer.Write(byteRate)
    writer.Write(blockAlign)
    writer.Write(bitsPerSample)
    writer.Write(subChunk2Id)
    writer.Write(subChunk2Size)
    writer.Write(raw: byte[])

  ///
  /// <summary>Type to represent a sound file</summary>
  /// <param name="samplingRate">sampling frequency</param>
  /// <param name="numChannels">number of channels</param>
  /// <param name="bitDepth">number of bits per sample</param>
  /// <param name="isPCM">whether the format is PCM, for writing, it must be
  /// true</param>
  /// <param name="rawData">byte array representing the raw sample data. A
  /// sample contains number of bits equal to bitDepth bits per channel. 
  /// Multiple channels are concatenated one after the other within the same
  /// sample</param>
  /// <returns>SoundFile object</returns>
  ///
  type SoundFile (samplingRate: float, 
                  numChannels: int, 
                  bitDepth: int,
                  isPCM: bool,
                  rawData: byte[]) =

    let read bytes n = 
      match bitDepth with
      | 8 -> rawData.[n] |> int8 |> float
      | 16 -> float(System.BitConverter.ToInt16(rawData, n))
      | _ -> failwith "Bit depth must be either 8 or 16"
    member f.SamplingRate = samplingRate
    member f.BitDepth = bitDepth
    member f.RawData = rawData
    member f.IsPCM = isPCM
    member f.NumChannels = numChannels
    member f.Channels = [|
      for i in [0..(numChannels-1)] ->
      f.RawData
      |> Array.length
      |> genIndices numChannels (bitDepth/8) i
      |> Seq.map (fun n -> read rawData n) 
      |]
    member f.WriteWav path = 
      writeWavFile path (int f.SamplingRate, f.NumChannels, f.BitDepth, 
        f.RawData)
    static member ReadWav (path:string) = readWavFile path |> SoundFile

  ///
  /// <summary>Utility function to convert a sequence of floats to a sequence
  /// of shorts, ready to be written to a wav file</summary>
  /// <param name="f">sequence of floats to be converted</param>
  /// <returns>sequence of short (int16)</returns>
  ///
  let floatTo16 (f:seq<float>) =
    f
    |> Seq.map int16
    |> Seq.collect (fun x -> System.BitConverter.GetBytes(x))
    |> Seq.toArray

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
    