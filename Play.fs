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

module Play =

  open NAudio.Wave
  open FSound.IO
  open System.IO
  open System.ComponentModel

  ///
  /// <summary>Play a Samples type using NAudio as a background job</summary>
  /// <param name="sampleRate">sampling rate</param>
  /// <param name="bitDepth">Bits per sample</param>
  /// <param name="nChannel">number of channels</param>
  /// <returns>unit</returns>
  ///
  let play sampleRate bitDepth nChannel (samples:Samples) =
    let b = samplesToBytes samples
    let worker = new BackgroundWorker()
    worker.DoWork.Add( fun args ->
      let format = WaveFormat(sampleRate, bitDepth, nChannel )
      use memstream = new MemoryStream( b )
      use wavestream = new RawSourceWaveStream( memstream, format )
      use wo = new WaveOut() 
      wo.Init(wavestream)
      wo.Play()
      System.Threading.Thread.Sleep(samples.Length/sampleRate*1000)
    )
    worker.RunWorkerAsync()

  ///
  /// <summary>Plays a SoundFile using NAudio as a background job</summary>
  /// <param name="sf">SoundFile object</param>
  /// <returns>unit</returns>
  ///
  let playSoundFile (sf: SoundFile) =
    sf.Samples |> play (int sf.SamplingRate) sf.BitDepth sf.NumChannels 

  ///
  /// <summary>Plays a float array using ISampleProvider.  This is experimental
  /// and does not work yet</summary>
  /// <param name="sampleRate">Sampling rate</param>
  /// <param name="samples">array of floats as samples</param>
  /// <returns>unit</returns>
  ///
  let play2 sampleRate (samples:float[]) =
    let worker = new BackgroundWorker()
    let duration = samples.Length/sampleRate
    let mutable posRead = 0
    let provider = {
      new ISampleProvider with 

        member p.Read(buffer, offset, count) = 
          let samples' = samples |> Array.map float32
          let size = samples.Length
          let count' = if posRead+count <= size then count else size-posRead
          Array.blit samples' posRead buffer offset count'
          posRead <- posRead + count'
          if count' < count then 0 else count'
        member p.WaveFormat = WaveFormat.CreateIeeeFloatWaveFormat(sampleRate, 
                                                                    1)
    }
    worker.DoWork.Add( fun _ ->
      use wo = new WaveOut()
      wo.Init(SampleProviders.SampleToWaveProvider16(provider)) 
      wo.Play()
      System.Threading.Thread.Sleep(duration*1000)
    )
    worker.RunWorkerAsync()