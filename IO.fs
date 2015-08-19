namespace FSound

module IO =

  type SoundFile (samplingRate: float, 
                  numChannels: int, 
                  data: seq<byte>, 
                  isPCM: bool) =
    member f.SamplingRate = samplingRate
    member f.Data = data
    member f.IsPCM = isPCM
    member f.NumChannels = numChannels

  let readWavFile (path: string) =
      use fileStream = new System.IO.FileStream( path, System.IO.FileMode.Open )
      use reader = new System.IO.BinaryReader(fileStream)
      // read chunk id - 4 bytes
      let id = System.Text.Encoding.Default.GetString(reader.ReadBytes(4))
      if id.ToString() <> "RIFF" then
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

      let idChunk2 = System.Text.Encoding.Default.GetString(reader.ReadBytes(4))
      if idChunk2 <> "data" then
        failwith "Incorrect format: chunk 2 id not 'data'"
      let nBytes = System.BitConverter.ToInt32(reader.ReadBytes(4), 0)    
      SoundFile (float samplingRate, int numChannels, reader.ReadBytes(nBytes),
        (int audioFormat = 1))

