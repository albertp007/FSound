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
  /// <summary>Play a Samples type using NAudio as a background job</summary>
  /// <param name="sampleRate">sampling rate</param>
  /// <param name="bitDepth">Bits per sample</param>
  /// <param name="nChannel">number of channels</param>
  /// <returns>unit</returns>
  ///
  let play2 sampleRate byteDepth channels =
    use ms1 = new MemoryStream()
    use writer = new System.IO.BinaryWriter(ms1)
    let nChannel = Seq.length channels
    let format = WaveFormat(sampleRate, byteDepth*8, nChannel )
    let mutable numSamples = 0
    let proc (b:byte) = writer.Write(b)
    let packChannels samples =
      Seq.iter (pack byteDepth proc) samples
      numSamples <- numSamples + 1
    
    // write to memory stream
    Seq.iter packChannels (zipSeq channels)

    let worker = new BackgroundWorker()
    worker.DoWork.Add( fun args ->
      use ms2 = new MemoryStream( ms1.GetBuffer() )
      use wavestream = new RawSourceWaveStream( ms2, format )
      use wo = new WaveOut() 
      wo.Init(wavestream)
      wo.Play()
      System.Threading.Thread.Sleep(numSamples/sampleRate/nChannel*1000)
    )
    worker.RunWorkerAsync()

  ///
  /// <summary>Plays a SoundFile using NAudio as a background job</summary>
  /// <param name="sf">SoundFile object</param>
  /// <returns>unit</returns>
  ///
  let playSoundFile (sf: SoundFile) =
    sf.Samples |> play (int sf.SamplingRate) sf.BitDepth sf.NumChannels 

  type SineWaveProvider32 (sr, a:float, f:float) =  
    inherit WaveProvider32() with
    let mutable nSample = 0
    override t.Read((buffer:float32[]), offset, count) =
      printfn "offset: %d, count: %d, f: %f, a: %f" offset count f a
      for n in 0..count do
        let s = float32 (a* sin(2.0*System.Math.PI*(float nSample)*f/(float sr)))
        buffer.[offset+n] <- s
        nSample <- nSample + 1
        if nSample >= sr then nSample <- 0
        // printfn "sample: %f, n: %d, nSample: %d" s n nSample
      count

  ///
  /// <summary>Plays a float array using ISampleProvider.  This is experimental
  /// and does not work yet</summary>
  /// <param name="sampleRate">Sampling rate</param>
  /// <param name="samples">array of floats as samples</param>
  /// <returns>unit</returns>
  ///
  let play3 sampleRate =
    let worker = new BackgroundWorker()
    let posRead = ref 0
    let f = 1000.0
    let a = 10000.0
    
    worker.DoWork.Add( fun _ ->
      use wo = new WaveOut()
      wo.Init(SineWaveProvider32(44100, 10000.0, 440.0)) 
      wo.Play()
      System.Threading.Thread.Sleep(1000)
    )
    worker.RunWorkerAsync()