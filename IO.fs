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

      (float samplingRate, numChannels, bitsPerSample, reader.ReadBytes(nBytes),
       (audioFormat = 1))

  type SoundFile (samplingRate: float, 
                  numChannels: int, 
                  bitDepth: int,
                  rawData: byte [],
                  isPCM: bool) =
    let read bytes n = match bitDepth with
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
    static member ReadWav (path:string) = readWavFile path |> SoundFile