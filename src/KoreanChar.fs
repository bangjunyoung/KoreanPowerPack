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

module Rembris.Globalization.KoreanChar

let [<Literal>] ChoseongCount = 19
let [<Literal>] JungseongCount = 21
let [<Literal>] JongseongCount = 28

let [<Literal>] HangulSyllableFirst = '가'
let [<Literal>] HangulSyllableLast = '힣'

let isSyllable c = HangulSyllableFirst <= c && c <= HangulSyllableLast

let isChoseong c = '\u1100' <= c && c <= '\u1112'
let isJungseong c = '\u1161' <= c && c <= '\u1175'
let isJongseong c = '\u11A8' <= c && c <= '\u11C2'

let choseongToIndex c =
    if c |> isChoseong then Some (int c - int '\u1100') else None
let jungseongToIndex c =
    if c |> isJungseong then Some (int c - int '\u1161') else None
let jongseongToIndex c =
    if c = '\u0000' then Some 0
    elif c |> isJongseong then Some (int c - int '\u11A8' + 1)
    else None

type JamoTable<'a> = { Choseong: 'a[]; Jungseong: 'a[]; Jongseong: 'a[] }

let jamoTable = {
    Choseong = [|"ᄀ"; "ᄁ"; "ᄂ"; "ᄃ"; "ᄄ"; "ᄅ"; "ᄆ"; "ᄇ"; "ᄈ"; "ᄉ"
                 "ᄊ"; "ᄋ"; "ᄌ"; "ᄍ"; "ᄎ"; "ᄏ"; "ᄐ"; "ᄑ"; "ᄒ"|]
    Jungseong = [|"ᅡ"; "ᅢ"; "ᅣ"; "ᅤ"; "ᅥ"; "ᅦ"; "ᅧ"; "ᅨ"; "ᅩ"; "ᅪ"
                  "ᅫ"; "ᅬ"; "ᅭ"; "ᅮ"; "ᅯ"; "ᅰ"; "ᅱ"; "ᅲ"; "ᅳ"; "ᅴ"
                  "ᅵ"|]
    Jongseong = [|""; "ᆨ"; "ᆩ"; "ᆪ"; "ᆫ"; "ᆬ"; "ᆭ"; "ᆮ"; "ᆯ"; "ᆰ"
                  "ᆱ"; "ᆲ"; "ᆳ"; "ᆴ"; "ᆵ"; "ᆶ"; "ᆷ"; "ᆸ"; "ᆹ"; "ᆺ"
                  "ᆻ"; "ᆼ"; "ᆽ"; "ᆾ"; "ᆿ"; "ᇀ"; "ᇁ"; "ᇂ"|]
}

let compatJamoTable = {
    Choseong = [|"ㄱ"; "ㄲ"; "ㄴ"; "ㄷ"; "ㄸ"; "ㄹ"; "ㅁ"; "ㅂ"; "ㅃ"; "ㅅ"
                 "ㅆ"; "ㅇ"; "ㅈ"; "ㅉ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"|]
    Jungseong = [|"ㅏ"; "ㅐ"; "ㅑ"; "ㅒ"; "ㅓ"; "ㅔ"; "ㅕ"; "ㅖ"; "ㅗ"; "ㅗㅏ"
                  "ㅗㅐ"; "ㅗㅣ"; "ㅛ"; "ㅜ"; "ㅜㅓ"; "ㅜㅔ"; "ㅜㅣ"; "ㅠ"; "ㅡ"; "ㅡㅣ"
                  "ㅣ"|]
    Jongseong = [|""; "ㄱ"; "ㄲ"; "ㄱㅅ"; "ㄴ"; "ㄴㅈ"; "ㄴㅎ"; "ㄷ"; "ㄹ"; "ㄹㄱ"
                  "ㄹㅁ"; "ㄹㅂ"; "ㄹㅅ"; "ㄹㅌ"; "ㄹㅍ"; "ㄹㅎ"; "ㅁ"; "ㅂ"; "ㅂㅅ"; "ㅅ"
                  "ㅆ"; "ㅇ"; "ㅈ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"|]
}

let compatJamoCharTable = {
    Choseong = [|'ㄱ'; 'ㄲ'; 'ㄴ'; 'ㄷ'; 'ㄸ'; 'ㄹ'; 'ㅁ'; 'ㅂ'; 'ㅃ'; 'ㅅ'
                 'ㅆ'; 'ㅇ'; 'ㅈ'; 'ㅉ'; 'ㅊ'; 'ㅋ'; 'ㅌ'; 'ㅍ'; 'ㅎ'|]
    Jungseong = [|'ㅏ'; 'ㅐ'; 'ㅑ'; 'ㅒ'; 'ㅓ'; 'ㅔ'; 'ㅕ'; 'ㅖ'; 'ㅗ'; 'ㅘ'
                  'ㅙ'; 'ㅚ'; 'ㅛ'; 'ㅜ'; 'ㅝ'; 'ㅞ'; 'ㅟ'; 'ㅠ'; 'ㅡ'; 'ㅢ'
                  'ㅣ'|]
    Jongseong = [|'\u0000'; 'ㄱ'; 'ㄲ'; 'ㄳ'; 'ㄴ'; 'ㄵ'; 'ㄶ'; 'ㄷ'; 'ㄹ'; 'ㄺ'
                  'ㄻ'; 'ㄼ'; 'ㄽ'; 'ㄾ'; 'ㄿ'; 'ㅀ'; 'ㅁ'; 'ㅂ'; 'ㅄ'; 'ㅅ'
                  'ㅆ'; 'ㅇ'; 'ㅈ'; 'ㅊ'; 'ㅋ'; 'ㅌ'; 'ㅍ'; 'ㅎ'|]
}

let compatChoseongToCharMap =
    (compatJamoTable.Choseong, compatJamoCharTable.Choseong)
    ||> Array.zip
    |> Map.ofArray
let compatJungseongToCharMap =
    (compatJamoTable.Jungseong, compatJamoCharTable.Jungseong)
    ||> Array.zip 
    |> Map.ofArray
let compatJongseongToCharMap =
    (compatJamoTable.Jongseong, compatJamoCharTable.Jongseong)
    ||> Array.zip 
    |> Map.ofArray

let compatChoseongToChar choseong =
    compatChoseongToCharMap |> Map.find choseong
let compatJungseongToChar jungseong =
    compatJungseongToCharMap |> Map.find jungseong
let compatJongseongToChar jongseong =
    compatJongseongToCharMap |> Map.find jongseong

let jamoToIndexMap jamoArray =
    jamoArray
    |> Array.mapi (fun index c -> c, index)
    |> Map.ofArray

let compatChoseongToIndexMap = jamoToIndexMap compatJamoCharTable.Choseong
let compatJungseongToIndexMap = jamoToIndexMap compatJamoCharTable.Jungseong
let compatJongseongToIndexMap = jamoToIndexMap compatJamoCharTable.Jongseong

let compatChoseongToIndex c = compatChoseongToIndexMap |> Map.tryFind c
let compatJungseongToIndex c = compatJungseongToIndexMap |> Map.tryFind c
let compatJongseongToIndex c = compatJongseongToIndexMap |> Map.tryFind c

let isCompatChoseong = compatChoseongToIndex >> Option.isSome
let isCompatJungseong = compatJungseongToIndex >> Option.isSome
let isCompatJongseong = compatJongseongToIndex >> Option.isSome

let decomposeWith table syllable =
    if not (syllable |> isSyllable) then
        invalidArg "syllable" <| sprintf "%c is not a Hangul syllable" syllable

    let decomposeIntoIndexes syllable =
        let sylIndex = int syllable - int HangulSyllableFirst
        sylIndex / (JungseongCount * JongseongCount),
        sylIndex % (JungseongCount * JongseongCount) / JongseongCount,
        sylIndex % JongseongCount

    let choIndex, jungIndex, jongIndex = syllable |> decomposeIntoIndexes
    if jongIndex = 0 
    then [|table.Choseong.[choIndex]; table.Jungseong.[jungIndex]|]
    else [|table.Choseong.[choIndex]; table.Jungseong.[jungIndex]
           table.Jongseong.[jongIndex]|]

let decompose = decomposeWith jamoTable
let decomposeCompat = decomposeWith compatJamoTable

let compose choseong jungseong jongseong = 
    let choIndex = 
        match compatChoseongToIndex choseong with
        | Some index -> index
        | None ->
            match choseongToIndex choseong with
            | Some index -> index
            | None -> invalidArg "choseong" "not a choseong"
    let jungIndex =
        match compatJungseongToIndex jungseong with
        | Some index -> index
        | None ->
            match jungseongToIndex jungseong with
            | Some index -> index
            | None -> invalidArg "jungseong" "not a jungseong"
    let jongIndex =
        match compatJongseongToIndex jongseong with
        | Some index -> index
        | None ->
            match jongseongToIndex jongseong with
            | Some index -> index
            | None -> invalidArg "jongseong" "not a jongseong"

    let composeFromIndexes choIndex jungIndex jongIndex =
        int HangulSyllableFirst +
            choIndex * (JungseongCount * JongseongCount) +
            jungIndex * JongseongCount +
            jongIndex
        |> char

    composeFromIndexes choIndex jungIndex jongIndex

let composeCompat choseong jungseong jongseong =
    compose
        (choseong |> compatChoseongToChar)
        (jungseong |> compatJungseongToChar)
        (jongseong |> compatJongseongToChar)
