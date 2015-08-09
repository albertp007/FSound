namespace FSound

module Signal =

  let sinewave amp freq phase samplingFreq startT endT =
    let t = seq [startT..(1.0/samplingFreq)..endT]
    let pi = System.Math.PI
    let x t = amp * cos (2.0*pi*freq*t + phase)
    Seq.map x t
