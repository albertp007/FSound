
namespace FSound

module Plot =

  open FSharp.Charting
  open FSound.Utilities

  //
  // <summary>Run FFT on signal and plot frequency content using FSharp.Charting
  // </summary>
  // <param name="signal">Sequence of floats representing the samples</param>
  // <returns>unit</returns>
  //
  let plotFreq toFreq samples =
    samples
    |> fft
    |> magnitudes
    |> Seq.take toFreq
    |> Chart.Point
    |> fun c -> c.ShowChart()



