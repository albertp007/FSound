namespace FSound

module Signal =

  let sinewave amp freq phase samplingFreq startT endT =
    let t = seq [startT..(1.0/samplingFreq)..endT]
    let pi = System.Math.PI
    let x t = amp * cos (2.0*pi*freq*t + phase)
    Seq.map x t

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
