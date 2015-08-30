namespace FSound

module Signal =

  open MathNet.Numerics.IntegralTransforms

  let private random = System.Random()

  let generate sf t genFunc =
    let t = seq [0.0..(1.0/sf)..t]
    Seq.map genFunc t

  let sinusoid a f ph t =
    let pi = System.Math.PI
    a * cos (2.0*pi*f*t + ph)

  let whiteNoise a (t:float) =
    a * (random.NextDouble() - 0.5)

  let square (a:float) (f:float) (t:float) =
    let i = int ( 2.0 * f * t ) 
    if i % 2 = 0 then a else -a
    
  let modulate (signal:float->float) (modulator:float->float) (t: float) =
    let primary = signal t
    (signal t) * (modulator t)

  let sinusoidGenerator a f ph sf t =
    sinusoid a f ph |> generate sf t

  let whiteNoiseGenerator a sf t =
    whiteNoise a |> generate sf t

  let waveGenerator a f sf t =
    modulate (whiteNoise a) (sinusoid 1.0 f 0.0)
    |> generate sf t

  let squareGenerator a f sf t =
    square a f |> generate sf t

  let foldi f acc xs = 
    let rec foldi' f i acc xs =
      match xs with
      | [] -> acc
      | h::t -> foldi' f (i+1) (f acc i h) t
    foldi' f 0 acc xs

  let dft samples =
 
    let dftComponent k s =
      let N = Seq.length s
      let w = 2.0*System.Math.PI*(float k)/(float N)
      foldi (fun (re,im) i x-> (re + x*cos(w*(float i)), 
                                im + x*sin(w*(float i)))) 
                                (0.0, 0.0) s
    
    Seq.mapi (fun i _ -> dftComponent i samples) samples

  /// <summary>Wrapper for the MathNet.Numerics (3.7.0) fourier transform.
  /// First convert the float samples to System.Numerics.Complex.  Then
  /// call MathNet.Numerics.IntegralTransforms.Fourier.Forward which modifies
  /// the input inline</summary>
  /// <param name="samples">sequence of real float samples</param>
  /// <returns>complex array</returns>
  let fft samples =

    let cmplxSamples = 
      samples 
      |> Seq.map (fun x -> System.Numerics.Complex(x, 0.0))
      |> Seq.toArray

    Fourier.Forward(cmplxSamples)
    cmplxSamples |> Array.map (fun x -> x.Real)

