//
// FSound - F# Sound Processing Library
// Copyright (c) 2016 by Albert Pang <albert.pang@me.com> 
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

  /// <summary>
  /// Creates an instance of ISampleProvider based on the samples in the given
  /// SampleSeq object
  /// </summary>
  /// <param name="sampleRate">Sampling rate in Hz</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="samples">SampleSeq object</param>
  let getSampleSeqProvider sampleRate bytesPerSample (samples: SampleSeq) =
    let maxAmp = 2.0 ** (float (bytesPerSample * 8 - 1))
    let convert s = float32 ( s / maxAmp )
    let stream = SampleSeqStream(samples, convert)
    let nChannels = samples.NumChannels

    { new ISampleProvider with
      member p.Read(buffer : float32 [], offset, count) =
        stream.Read(buffer, offset, count)
             
      member p.WaveFormat = 
        WaveFormat.CreateIeeeFloatWaveFormat(sampleRate, nChannels) 
    }

  ///
  /// <summary>
  /// Plays a SampleSeq using ISampleProvider of NAudio synchronously.
  /// </summary>
  /// <param name="sampleRate">Sampling rate</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="samples">SampleSeq object</param>
  /// <returns>unit</returns>
  ///
  let playSync sampleRate bytesPerSample (samples: SampleSeq) =

    let provider = getSampleSeqProvider sampleRate bytesPerSample samples    
    use wo = new WaveOutEvent()
    wo.Init(provider)
    wo.Play()
    while wo.PlaybackState = PlaybackState.Playing do
      System.Threading.Thread.Sleep(100);

  ///
  /// <summary>
  /// Plays a SampleSeq using ISampleProvider of NAudio asynchronously
  /// </summary>
  /// <param name="sampleRate">Sampling rate</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="samples">SampleSeq object</param>
  /// <returns>unit</returns>
  ///  
  let playAsync sampleRate bytesPerSample (samples: SampleSeq) =
    let provider = getSampleSeqProvider sampleRate bytesPerSample samples    
    let wo = new WaveOutEvent()
    wo.Init(provider)
    wo.Play()

  ///
  /// <summary>Play a SampleSeq object using NAudio synchronously</summary>
  /// <param name="sampleRate">Sampling frequency in Hz</param>
  /// <param name="bytesPerSample">Bit depth in number of bytes</param>
  /// <param name="samples">Sample sequence object to be played</param>
  /// <returns>unit</returns>
  ///
  let playSampleSeq sampleRate bytesPerSample (samples : SampleSeq) =     
    playSync sampleRate bytesPerSample samples

  /// <summary>
  /// Obsolete play function which calculates whole of sample sequence then
  /// write to a memory stream before playing.  Less efficient than the
  /// latest play2 function which uses the ISampleProvider interface in
  /// NAudio which allows the function to compute and read the samples by
  /// small blocks, hence less latency before playback actually starts
  /// </summary>
  /// <param name="sampleRate"></param>
  /// <param name="bytesPerSample"></param>
  /// <param name="samples"></param>
  let playSampleSeq0 sampleRate bytesPerSample (samples : SampleSeq) = 
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
 