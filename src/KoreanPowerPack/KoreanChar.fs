//
// Copyright 2019 Bang Jun-young
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
// NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

namespace KoreanPowerPack

open KoreanPowerPack.FSharp
open KoreanCharInternal

module KoreanChar =
    let isSyllable c = HangulSyllableFirst <= c && c <= HangulSyllableLast

    let isChoseong c = '\u1100' <= c && c <= '\u1112'
    let isJungseong c = '\u1161' <= c && c <= '\u1175'
    let isJongseong c = '\u11A8' <= c && c <= '\u11C2'

    let isCompatChoseong c = c |> compatChoseongToIndex |> Option.isSome
    let isCompatJungseong c = c |> compatJungseongToIndex |> Option.isSome
    let isCompatJongseong c = c |> compatJongseongToIndex |> Option.isSome

    let private getJamoWith getIndex (collection: char[]) syllable =
        if not (isSyllable syllable) then
            invalidArg (nameof syllable) $"{syllable} is not a Hangul syllable"

        collection[getIndex syllable]

    let getChoseong syllable =
        getJamoWith getChoseongIndex JamosAsChar.Choseong syllable
    let getJungseong syllable =
        getJamoWith getJungseongIndex JamosAsChar.Jungseong syllable
    let getJongseong syllable =
        getJamoWith getJongseongIndex JamosAsChar.Jongseong syllable
    let getCompatChoseong syllable =
        getJamoWith getChoseongIndex CompatJamosAsChar.Choseong syllable
    let getCompatJungseong syllable =
        getJamoWith getJungseongIndex CompatJamosAsChar.Jungseong syllable
    let getCompatJongseong syllable =
        getJamoWith getJongseongIndex CompatJamosAsChar.Jongseong syllable

    let choseongToCompatChoseong c =
        if not (isChoseong c) then
            invalidArg (nameof c) $"{c} is not a Hangul choseong"

        CompatJamosAsChar.Choseong[int c - 0x1100]

    let compatChoseongToChoseong c =
        match CompatJamosAsChar.Choseong |> Array.tryBinarySearch c with
        | None -> invalidArg (nameof c) $"{c} is not a Hangul Compatibility choseong"
        | Some index -> char (0x1100 + index)

    let tryJoinJamo jamo =
        match String.length jamo with
        | 0 -> Some '\u0000'
        | 1 -> match JoinedJamos |> Array.tryBinarySearch jamo[0] with
               | Some index -> Some JoinedJamos[index]
               | None -> None
        | 2 -> match SplittedJamos |> Array.tryBinarySearch jamo with
               | Some index -> Some JoinedJamos[index]
               | None -> None
        | _ -> None

    let joinJamo jamo =
        match tryJoinJamo jamo with
        | Some joinedJamo -> joinedJamo
        | None -> invalidArg (nameof jamo) $"{jamo} is not a Hangul jamo"

    let trySplitJamo jamo =
        match jamo with
        | '\u0000' -> Some ""
        | _ -> match JoinedJamos |> Array.tryBinarySearch jamo with
               | Some index -> Some SplittedJamos[index]
               | None -> None

    let splitJamo jamo =
        match trySplitJamo jamo with
        | Some splittedJamo -> splittedJamo
        | None -> invalidArg (nameof jamo) $"{jamo} is not a Hangul jamo"

    let compose choseong jungseong jongseong =
        let convert argName f g x =
            match f x with
            | Some index -> index
            | None ->
                match g x with
                | Some index -> index
                | None -> invalidArg argName $"{x} is not a {argName}"

        let choseongIndex, jungseongIndex, jongseongIndex =
            convert (nameof choseong) compatChoseongToIndex choseongToIndex choseong,
            convert (nameof jungseong) compatJungseongToIndex jungseongToIndex jungseong,
            convert (nameof jongseong) compatJongseongToIndex jongseongToIndex jongseong

        int HangulSyllableFirst +
            choseongIndex * JungseongCount * JongseongCount +
            jungseongIndex * JongseongCount +
            jongseongIndex
        |> char

    let composeFromStrings choseong jungseong jongseong =
        let invalidJamo argName arg =
            invalidArg argName $"{arg} is not a {argName}"

        match (tryJoinJamo choseong), (tryJoinJamo jungseong), (tryJoinJamo jongseong) with
        | Some cho, Some jung, Some jong -> compose cho jung jong
        | None, _, _ -> invalidJamo (nameof choseong) choseong
        | _, None, _ -> invalidJamo (nameof jungseong) jungseong
        | _, _, None -> invalidJamo (nameof jongseong) jongseong

    let private decomposeWith collection syllable =
        if not (isSyllable syllable) then
            invalidArg (nameof syllable) $"{syllable} is not a Hangul syllable"

        let choseong = collection.Choseong[getChoseongIndex syllable]
        let jungseong = collection.Jungseong[getJungseongIndex syllable]
        let jongseong = collection.Jongseong[getJongseongIndex syllable]

        match jongseong with
        | "" -> [|choseong; jungseong|]
        | _  -> [|choseong; jungseong; jongseong|]

    let decompose syllable =
        decomposeWith JamosAsString syllable

    let decomposeToCompat syllable =
        decomposeWith CompatJamosAsString syllable

    let decomposeToDubeolsik syllable =
        decomposeWith DubeolsikJamosAsString syllable

open KoreanChar
open System.Runtime.InteropServices

type KoreanChar private () =
    static member IsSyllable c = isSyllable c

    static member IsChoseong c = isChoseong c
    static member IsJungseong c = isJungseong c
    static member IsJongseong c = isJongseong c

    static member IsCompatChoseong c = isCompatChoseong c
    static member IsCompatJungseong c = isCompatJungseong c
    static member IsCompatJongseong c = isCompatJongseong c

    static member JoinJamo jamo = joinJamo jamo

    static member SplitJamo jamo = splitJamo jamo

    static member Compose(choseong, jungseong,
                          [<Optional; DefaultParameterValue('\u0000')>]
                          jongseong) =
        compose choseong jungseong jongseong

    static member Compose(choseong, jungseong,
                          [<Optional; DefaultParameterValue("")>]
                          jongseong) =
        composeFromStrings choseong jungseong jongseong

    static member Compose(jamo: char[]) =
        match jamo with
        | [|cho; jung|] -> compose cho jung '\u0000'
        | [|cho; jung; jong|] -> compose cho jung jong
        | _ -> invalidArg (nameof jamo) $"%A{jamo} is not a valid form"

    static member Compose(jamo: string[]) =
        match jamo with
        | [|cho; jung|] -> composeFromStrings cho jung ""
        | [|cho; jung; jong|] -> composeFromStrings cho jung jong
        | _ -> invalidArg (nameof jamo) $"%A{jamo} is not a valid form"

    static member Decompose syllable =
        decompose syllable

    static member DecomposeToCompat syllable =
        decomposeToCompat syllable

    static member DecomposeToDubeolsik syllable =
        decomposeToDubeolsik syllable
