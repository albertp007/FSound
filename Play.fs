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
  
  ///
  /// <summary>Play a SampleSeq object using NAudio</summary>
  /// <param name="sampleRate">Sampling frequency in Hz</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="samples">Sample sequence object to be played</param>
  /// <returns>unit</returns>
  ///
  let playSampleSeq sampleRate bytesPerSample (samples : SampleSeq) = 
    async { 
      let nChannel = samples.NumChannels
      let format = WaveFormat(sampleRate, bytesPerSample * 8, nChannel)
      use ms = new MemoryStream()
      use writer = new BinaryWriter(ms)
      let bytesWritten = packSampleSequence bytesPerSample writer samples
      // reset stream position to 0
      ms.Position <- 0L
      use wavestream = new RawSourceWaveStream(ms, format)
      use wo = new WaveOut()
      wo.Init(wavestream)
      wo.Play()
      do! Async.Sleep(bytesWritten / nChannel / sampleRate * 1000)
    }
    |> Async.Start
  
  ///
  /// <summary>Play a list of sequence of samples, each sequence represents a
  /// channel</summary>
  /// <param name="sampleRate">Sampling frequency in Hz</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="sequences">List of sequence of samples (float)</param>
  /// <returns>unit</returns>
  ///
  let play sampleRate bytesPerSample sequences = 
    match sequences with
    | [] -> ()
    | [ mono ] -> playSampleSeq sampleRate bytesPerSample (Mono mono)
    | [ left; right ] -> 
      playSampleSeq sampleRate bytesPerSample (Stereo(left, right))
    | multi -> playSampleSeq sampleRate bytesPerSample (Multi multi)
  
  /// <summary>
  /// Convenience function to play a sequence of floats as samples
  /// </summary>
  /// <param name="sampleRate">Sampling frequency in Hz</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="sequence">Sequence of floats to play</param>
  /// <returns>unit</returns>
  let playMono sampleRate bytesPerSample sequence = 
    playSampleSeq sampleRate bytesPerSample (Mono sequence)
  
  /// <summary>
  /// Convenience function to play a sequence of pairs of floats as sample
  /// values for the left and right channel
  /// </summary>
  /// <param name="sampleRate">Sampling frequency in Hz</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="sequences">Sequence of pairs of floats</param>
  /// <returns>unit</returns>
  let playStereo sampleRate bytesPerSample sequences = 
    playSampleSeq sampleRate bytesPerSample (Stereo2 sequences)
  
  ///
  /// <summary>Plays a SoundFile using NAudio as a background job</summary>
  /// <param name="sf">SoundFile object</param>
  /// <returns>unit</returns>
  ///
  let playSoundFile (sf : SoundFile) = 
    sf.Samples |> playSampleSeq (int sf.SamplingRate) sf.BytesPerSample
  
  ///
  /// <summary>Plays a float array using ISampleProvider of NAudio.  Note that
  /// the amplitude of the sequence of floats accepted by NAudio is normalized
  /// to be between 0.0 and 1.0, meaning that each sample value is required to
  /// be between -1.0 and 1.0</summary>
  /// <param name="sampleRate">Sampling rate</param>
  /// <param name="samples">SampleSeq object</param>
  /// <returns>unit</returns>
  ///
  let play2 sampleRate (samples: SampleSeq) =

    let stream = SampleSeqStream(samples, float32)
    let nChannels = samples.NumChannels

    let provider = 
      { new ISampleProvider with
          member p.Read(buffer : float32 [], offset, count) =
            stream.Read(buffer, offset, count)
             
          member p.WaveFormat = 
            WaveFormat.CreateIeeeFloatWaveFormat(sampleRate, nChannels) 
      }
    
    let wo = new WaveOut()
    wo.Init(provider)
    wo.Play()