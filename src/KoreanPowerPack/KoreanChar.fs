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

    let isCompatChoseong = compatChoseongToIndex >> Option.isSome
    let isCompatJungseong = compatJungseongToIndex >> Option.isSome
    let isCompatJongseong = compatJongseongToIndex >> Option.isSome

    let tryJoinJamo jamo =
        match String.length jamo with
        | 0 -> Some '\u0000'
        | 1 -> match jamoChars |> Array.tryBinarySearch jamo.[0] with
               | Some index -> Some jamoChars.[index]
               | None -> None
        | 2 -> jamoJoinMap |> Map.tryFind jamo
        | _ -> None

    let joinJamo jamo =
        match tryJoinJamo jamo with
        | Some joinedJamo -> joinedJamo
        | None -> invalidArg "jamo" <| sprintf "%s is not a jamo" jamo

    let trySplitJamo jamo =
        jamoSplitMap |> Map.tryFind jamo

    let splitJamo jamo =
        match trySplitJamo jamo with
        | Some splittedJamo -> splittedJamo
        | None -> invalidArg "jamo" <| sprintf "%c is not a jamo" jamo

    let compose choseong jungseong jongseong =
        let convert argName f g x =
            match f x with
            | Some index -> index
            | None ->
                match g x with
                | Some index -> index
                | None -> invalidArg argName <| sprintf "%A is not a %s" x argName

        let choIndex = convert "choseong" compatChoseongToIndex choseongToIndex choseong
        let jungIndex = convert "jungseong" compatJungseongToIndex jungseongToIndex jungseong
        let jongIndex = convert "jongseong" compatJongseongToIndex jongseongToIndex jongseong

        composeFromIndexes choIndex jungIndex jongIndex

    let composeWithStrings choseong jungseong jongseong =
        let invalidJamo argName arg =
            invalidArg argName <| sprintf "%A is not a %s" arg argName

        let cho = tryJoinJamo choseong
        let jung = tryJoinJamo jungseong
        let jong = tryJoinJamo jongseong

        match cho, jung, jong with
        | Some cho, Some jung, Some jong -> compose cho jung jong
        | None, _, _ -> invalidJamo "choseong" choseong
        | _, None, _ -> invalidJamo "jungseong" jungseong
        | _, _, None -> invalidJamo "jongseong" jongseong

    let internal decomposeWith collection syllable =
        if not (syllable |> isSyllable) then
            invalidArg "syllable" <| sprintf "%c is not a Hangul syllable" syllable

        let choIndex, jungIndex, jongIndex = syllable |> decomposeIntoIndexes

        collection.Choseong.[choIndex],
        collection.Jungseong.[jungIndex],
        collection.Jongseong.[jongIndex]

    let decompose = decomposeWith jamoCharCollection
    let decomposeCompat = decomposeWith compatJamoCharCollection
    let decomposeIntoStrings = decomposeWith jamoStringCollection
    let decomposeCompatIntoStrings = decomposeWith compatJamoStringCollection

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

    static member Compose(choseong, jungseong,
                          [<Optional; DefaultParameterValue('\u0000')>]
                          jongseong) =
        compose choseong jungseong jongseong

    static member Compose(choseong, jungseong,
                          [<Optional; DefaultParameterValue("")>]
                          jongseong) =
        composeWithStrings choseong jungseong jongseong

    static member Decompose syllable =
        decompose syllable

    static member DecomposeCompat syllable =
        decomposeCompat syllable

    static member DecomposeIntoStrings syllable =
        decomposeIntoStrings syllable

    static member DecomposeCompatIntoStrings syllable =
        decomposeCompatIntoStrings syllable
