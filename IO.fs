namespace FSound

module IO =

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
    let chunkID = "RIFF"B
    let chunkSize = 36 + samples.NumBytes
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
    let subChunk2Size = samples.NumBytes
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
    