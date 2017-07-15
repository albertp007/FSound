namespace FSound

open System;
open 

module Tonal =

  /// <summary>
  /// This type represents the 7 available note letters for western tonal music.
  /// Distinguish between a Letter and a Note.  A Letter and a note qualifier
  /// together forms a Note.  See the Note type below
  /// </summary>
  type Letter = C | D | E | F | G | A | B

  /// <summary>
  /// This type represents the 5 possible types of modifiers on any note letter
  /// to form a note:
  /// Sh - Sharp
  /// X - Double sharp
  /// XSh - Triple sharp
  /// Fl -> Flat
  /// Bb -> Double flat
  /// Bbb -> Triple flat
  /// N -> Natural
  /// </summary>
  type Note = 
    | Sh of Letter
    | X of Letter
    | XSh of Letter
    | Fl of Letter
    | Bb of Letter
    | Bbb of Letter
    | N of Letter

  /// <summary>
  /// This type represents the different modes of a scale.  Currently only
  /// Major and Minor
  /// </summary>
  type Mode = Major | Minor

  /// <summary>
  /// This type represents all the common interval sizes
  /// </summary>
  type IntervalSize =
    | Unison
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth
    | Seventh
    | Octave
    | Ninth
    | Eleventh
    | Thirteenth

  /// <summary>
  /// This type represents all the possible quality modifiers of an interval
  /// size to become an interval
  /// </summary>
  type IntervalQuality = 
    | Maj 
    | Min
    | Perfect
    | Aug
    | Dim

  /// <summary>
  /// This type represents a combination of interval quality and interval size
  /// to form an interval.  There is however no way to forbid during compile
  /// time the exceptions, yet accept others.  e.g. there is no minor 5th, but
  /// there is diminished 5th.  Also we are treating the perfect intervals to be
  /// the same as the major intervals with the same size.  e.g. Perfect 5th is
  /// the same as Major 5th
  /// </summary>
  type Interval =
    | Perfect of IntervalSize
    | Maj of IntervalSize
    | Min of IntervalSize
    | Aug of IntervalSize
    | Dim of IntervalSize

  /// <summary>
  /// This function returns false to all invalid combinations of interval
  /// quality and interval size.  Otherwise, it returns true.
  /// </summary>
  /// <param name="i"></param>
  let isIntervalValid i =
    match i with
    | Min Unison -> false
    | Min Fourth -> false
    | Min Fifth -> false
    | Min Octave -> false
    | _ -> true

  /// <summary>
  /// This function simply returns the ordinal number of the 7 available note
  /// letters.  This is useful for calculating the resulting node letter (but
  /// not the full note with the qualifiers) when adding an interval to a note
  /// </summary>
  /// <param name="l"></param>
  let letterToInt l =
    match l with
    | C -> 0
    | D -> 1
    | E -> 2
    | F -> 3
    | G -> 4
    | A -> 5
    | B -> 6

  /// <summary>
  /// This function returns the number of semitone of a note letter from the
  /// note C (natural C), defining C as 0 and treating the note letter as the
  /// natural of the letter itself, without any sharps or flats.
  /// </summary>
  /// <param name="l"></param>
  let letterToSemitone l =
    match l with
    | C -> 0
    | D -> 2
    | E -> 4
    | F -> 5
    | G -> 7
    | A -> 9
    | B -> 11

  /// <summary>
  /// This function extracts the note letter from a note, ignoring its qualifier
  /// </summary>
  /// <param name="n"></param>
  let getNoteLetter n =
    match n with
    | Sh l -> l
    | XSh l -> l
    | Fl l -> l
    | X l -> l
    | Bb l -> l
    | Bbb l -> l
    | N l -> l

  /// <summary>
  /// This function maps an interval size to an integer representing its ordinal
  /// number.  This is used to calculate the note letter when adding an interval
  /// to a note.  E.g. adding a third to A something, regardless of whether it
  /// is A sharp or A flat will give us C something, also regardless of what
  /// kind of third it is.
  ///
  /// A sharp + augmented 3rd -> C triple sharp (which is not supported)
  /// A sharp + major 3rd -> C double sharp
  /// A sharp + minor 3rd -> C sharp
  /// A sharp + diminished 3rd -> C
  /// A + augmented 3rd -> C double sharp
  /// A + major 3rd -> C sharp
  /// A + minor 3rd -> C
  /// A + diminished 3rd -> C flat
  /// A flat + augmented 3rd -> C sharp
  /// A flat + major 3rd -> C
  /// A flat + minor 3rd -> C flat
  /// A flat + diminished 3rd -> C double flat
  ///
  /// In all cases, any notes with letter A plus any interval of third, will
  /// result in a note of C something.  Converting an interval size to its
  /// ordinal number allows to do this.
  /// </summary>
  /// <param name="sz"></param>
  let intervalSizeToInt sz =
    match sz with
    | Unison -> 0
    | Second -> 1
    | Third -> 2
    | Fourth -> 3
    | Fifth -> 4
    | Sixth -> 5
    | Seventh -> 6
    | Octave -> 7
    | Ninth -> 8
    | Eleventh -> 10
    | Thirteenth -> 12

  /// <summary>
  /// This function calculates the size of an interval size in terms of the
  /// number of semitones from unison
  /// </summary>
  /// <param name="sz"></param>
  let intervalSizeToSemitone sz =
    match sz with
    | Unison -> 0
    | Second -> 2
    | Third -> 4
    | Fourth -> 5
    | Fifth -> 7
    | Sixth -> 9
    | Seventh -> 11
    | Octave -> 12
    | Ninth -> 14
    | Eleventh -> 17
    | Thirteenth -> 21

  /// <summary>
  /// This function converts an interval to the number of semitone its interval
  /// size contains
  /// </summary>
  /// <param name="i"></param>
  let intervalToSemitone i =
    match i with
    | Perfect x -> x |> intervalSizeToSemitone
    | Maj x -> x |> intervalSizeToSemitone
    | Min x -> x |> intervalSizeToSemitone
    | Aug x -> x |> intervalSizeToSemitone
    | Dim x -> x |> intervalSizeToSemitone

  /// <summary>
  /// This function extracts the interval size from an interval. e.g. from the
  /// interval Perfect 5th, Fifth is returned
  /// </summary>
  /// <param name="i"></param>
  let getIntervalSize i =
    match i with
    | Perfect x -> x
    | Maj x -> x
    | Min x -> x
    | Aug x -> x
    | Dim x -> x

  /// <summary>
  /// This function adds an interval size to a letter according to their ordinal
  /// numbers.
  /// </summary>
  /// <param name="l"></param>
  /// <param name="d"></param>
  let addIntervalSizeToLetter (l: Letter) d =
    let cir = [|C; D; E; F; G; A; B|]
    let len = Octave |> intervalSizeToInt
    let x = d |> intervalSizeToInt
    let v = l |> letterToInt
    (v + x) % len |> Array.get cir

  /// <summary>
  /// This function calculates the number of semitones from C of a particular
  /// note.  This considers the qualifier of the note as well, not just the
  /// note letter.
  /// </summary>
  /// <param name="note"></param>
  let calcSemitone note =
    let d = 
      match note with
      | Sh _ -> 1
      | X _ -> 2
      | XSh _ -> 3
      | Fl _ -> -1
      | Bb _ -> -2
      | Bbb _ -> -3
      | N _ -> 0
    note |> getNoteLetter |> letterToSemitone |> ((+) d)

  /// <summary>
  /// This function calculates the number of semitones of an interval from a 
  /// major or perfect (which is treated as a major) interval of the same size
  /// e.g. any minor interval is 1 semitone flatter than a major interval of the
  /// same size, except for unison, fourth, fifth and octave, the minor interval
  /// of which does not exist by definition.  A minor 3rd is one semitone 
  /// flatter than a major 3rd.  Note the special case of the diminished
  /// interval. Again, use diminished 3rd as an example. A diminished 3rd is one
  /// semitone flatter than the minor 3rd and therefore 2 semitones flatter than
  /// a major 3rd.  However, as mentioned above, there is no minor unison,
  /// fourth, fifth or octave.  Therefore, the diminished 5th is 1 semitone
  /// less than the perfect 5th and not 2 semitones.
  /// </summary>
  /// <param name="i"></param>
  let calcIntervalSemitone i =
    let d = 
      match i with
      | Perfect _ | Maj _ -> 0
      | Min _ -> (-1)
      | Aug _ -> 1
      | Dim x -> match x with
                 | Unison | Fourth | Fifth | Octave -> (-1)
                 | _ -> -2
    i |> intervalToSemitone |> ((+) d)

  /// <summary>
  /// With all of the fuss above, this function adds a certain interval (fully
  /// qualified with both the type and size) to a note (fully qualified with
  /// its note letter and the qualifiers) and return a note.
  /// </summary>
  /// <param name="n"></param>
  /// <param name="i"></param>
  let addIntervalToNote n i =
    let x = n |> calcSemitone
    let d = calcIntervalSemitone i
    let letter = i |> getIntervalSize 
                   |> addIntervalSizeToLetter (getNoteLetter n)
    let letterSemitone = letter |> letterToSemitone
    let targetSemitone = (x + d) % 12
    match (targetSemitone - letterSemitone) with
    | -3 | 9 -> Bbb letter
    | -2 | 10 -> Bb letter
    | -1 | 11 -> Fl letter
    | 0 -> N letter
    | 1 | -11 -> Sh letter
    | 2 | -10 -> X letter
    | 3 | -9 -> XSh letter
    | _ -> failwith "Interval not supported"

  module Tests =

    open NUnit.Framework
    
