namespace FSound

module IO =

  type SoundFile (samplingRate: float, 
                  numChannels: int, 
                  data: (float list * float list),
                  isPCM: bool) =
    member f.SamplingRate = samplingRate
    member f.Data = data
    member f.IsPCM = isPCM
    member f.NumChannels = numChannels
  
  /// <summary>Reads a PCM wave file and return a SoundFile object</summary>
  /// <param name="path">Path of the file</param>
  /// <returns>a SoundFile object representing the data in the wav file. If the
  /// file has only one channel, then the first of the tuple in Data contains
  /// the data and the second of the tuple is empty</returns>
  let readWavFile (path: string) =
      use fileStream = new System.IO.FileStream( path, System.IO.FileMode.Open )
      use reader = new System.IO.BinaryReader(fileStream)
      // read chunk id - 4 bytes
      let id = reader.ReadBytes(4)
      if id <> "RIFF"B then
        failwith "Not a WAV file"
      // ignore next 12 bytes - chunk size: 4, format: 4, subChunkId: 4
      reader.ReadBytes(12) |> ignore

      let sizeChunk1 = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)
      let audioFormat = System.BitConverter.ToInt16(reader.ReadBytes(2), 0)
      let numChannels = System.BitConverter.ToInt16(reader.ReadBytes(2), 0)
      let samplingRate = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)

      // ignore rest of chunk 1
      reader.ReadBytes(sizeChunk1-8) |> ignore

      if int audioFormat <> 1 then
        // Non-PCM
        reader.ReadBytes(4) |> ignore
        let nExtraBytes = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)
        reader.ReadBytes(nExtraBytes) |> ignore

      let idChunk2 = reader.ReadBytes(4)
      if idChunk2 <> "data"B then
        failwith "Incorrect format: chunk 2 id not 'data'"
      let nBytes = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)  
      let data = Array.foldBack (fun x (l,r) -> ((float x)::r, l)) 
                   (reader.ReadBytes(nBytes)) ([],[])

      SoundFile (float samplingRate, int numChannels, data, 
        (int audioFormat = 1))