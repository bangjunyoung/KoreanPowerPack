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

module internal KoreanChar =
    let [<Literal>] HangulSyllableFirst = '가'
    let [<Literal>] HangulSyllableLast = '힣'

    let isSyllable c = HangulSyllableFirst <= c && c <= HangulSyllableLast

    let [<Literal>] ChoseongCount = 19
    let [<Literal>] JungseongCount = 21
    let [<Literal>] JongseongCount = 28

    let composeIndexes choIndex jungIndex jongIndex =
        int HangulSyllableFirst +
            choIndex * (JungseongCount * JongseongCount) +
            jungIndex * JongseongCount +
            jongIndex
        |> char

    let decomposeIntoIndexes syllable =
        let sylIndex = int syllable - int HangulSyllableFirst
        sylIndex / (JungseongCount * JongseongCount),
        sylIndex % (JungseongCount * JongseongCount) / JongseongCount,
        sylIndex % JongseongCount

    type JamoCollection<'a> = { Choseong: 'a[]; Jungseong: 'a[]; Jongseong: 'a[] }

    let jamosAsString = {
        Choseong = [|
            "ᄀ"; "ᄀᄀ"; "ᄂ"; "ᄃ"; "ᄃᄃ"; "ᄅ"; "ᄆ"; "ᄇ"; "ᄇᄇ"; "ᄉ"
            "ᄉᄉ"; "ᄋ"; "ᄌ"; "ᄌᄌ"; "ᄎ"; "ᄏ"; "ᄐ"; "ᄑ"; "ᄒ"
        |]
        Jungseong = [|
            "ᅡ"; "ᅢ"; "ᅣ"; "ᅤ"; "ᅥ"; "ᅦ"; "ᅧ"; "ᅨ"; "ᅩ"; "ᅩᅡ"
            "ᅩᅢ"; "ᅩᅵ"; "ᅭ"; "ᅮ"; "ᅮᅥ"; "ᅮᅦ"; "ᅮᅵ"; "ᅲ"; "ᅳ"; "ᅳᅵ"
            "ᅵ"
        |]
        Jongseong = [|
            ""; "ᆨ"; "ᆨᆨ"; "ᆨᆺ"; "ᆫ"; "ᆫᆽ"; "ᆫᇂ"; "ᆮ"; "ᆯ"; "ᆯᆨ"
            "ᆯᆷ"; "ᆯᆸ"; "ᆯᆺ"; "ᆯᇀ"; "ᆯᇁ"; "ᆯᇂ"; "ᆷ"; "ᆸ"; "ᆸᆺ"; "ᆺ"
            "ᆺᆺ"; "ᆼ"; "ᆽ"; "ᆾ"; "ᆿ"; "ᇀ"; "ᇁ"; "ᇂ"
        |]
    }

    let jamos = {
        Choseong = [|
            'ᄀ'; 'ᄁ'; 'ᄂ'; 'ᄃ'; 'ᄄ'; 'ᄅ'; 'ᄆ'; 'ᄇ'; 'ᄈ'; 'ᄉ'
            'ᄊ'; 'ᄋ'; 'ᄌ'; 'ᄍ'; 'ᄎ'; 'ᄏ'; 'ᄐ'; 'ᄑ'; 'ᄒ'
        |]
        Jungseong = [|
            'ᅡ'; 'ᅢ'; 'ᅣ'; 'ᅤ'; 'ᅥ'; 'ᅦ'; 'ᅧ'; 'ᅨ'; 'ᅩ'; 'ᅪ'
            'ᅫ'; 'ᅬ'; 'ᅭ'; 'ᅮ'; 'ᅯ'; 'ᅰ'; 'ᅱ'; 'ᅲ'; 'ᅳ'; 'ᅴ'
            'ᅵ'
        |]
        Jongseong = [|
            '\u0000'; 'ᆨ'; 'ᆩ'; 'ᆪ'; 'ᆫ'; 'ᆬ'; 'ᆭ'; 'ᆮ'; 'ᆯ'; 'ᆰ'
            'ᆱ'; 'ᆲ'; 'ᆳ'; 'ᆴ'; 'ᆵ'; 'ᆶ'; 'ᆷ'; 'ᆸ'; 'ᆹ'; 'ᆺ'
            'ᆻ'; 'ᆼ'; 'ᆽ'; 'ᆾ'; 'ᆿ'; 'ᇀ'; 'ᇁ'; 'ᇂ'
        |]
    }

    let compatJamosAsString = {
        Choseong = [|
            "ㄱ"; "ㄱㄱ"; "ㄴ"; "ㄷ"; "ㄷㄷ"; "ㄹ"; "ㅁ"; "ㅂ"; "ㅂㅂ"; "ㅅ"
            "ㅅㅅ"; "ㅇ"; "ㅈ"; "ㅈㅈ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"
        |]
        Jungseong = [|
            "ㅏ"; "ㅐ"; "ㅑ"; "ㅒ"; "ㅓ"; "ㅔ"; "ㅕ"; "ㅖ"; "ㅗ"; "ㅗㅏ"
            "ㅗㅐ"; "ㅗㅣ"; "ㅛ"; "ㅜ"; "ㅜㅓ"; "ㅜㅔ"; "ㅜㅣ"; "ㅠ"; "ㅡ"; "ㅡㅣ"
            "ㅣ"
        |]
        Jongseong = [|
            ""
            "ㄱ"; "ㄲ"; "ㄱㅅ"; "ㄴ"; "ㄴㅈ"; "ㄴㅎ"; "ㄷ"; "ㄹ"; "ㄹㄱ"; "ㄹㅁ"
            "ㄹㅂ"; "ㄹㅅ"; "ㄹㅌ"; "ㄹㅍ"; "ㄹㅎ"; "ㅁ"; "ㅂ"; "ㅂㅅ"; "ㅅ"; "ㅆ"
            "ㅇ"; "ㅈ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"
        |]
    }

    let compatJamos = {
        Choseong = [|
            'ㄱ'; 'ㄲ'; 'ㄴ'; 'ㄷ'; 'ㄸ'; 'ㄹ'; 'ㅁ'; 'ㅂ'; 'ㅃ'; 'ㅅ'
            'ㅆ'; 'ㅇ'; 'ㅈ'; 'ㅉ'; 'ㅊ'; 'ㅋ'; 'ㅌ'; 'ㅍ'; 'ㅎ'
        |]
        Jungseong = [|
            'ㅏ'; 'ㅐ'; 'ㅑ'; 'ㅒ'; 'ㅓ'; 'ㅔ'; 'ㅕ'; 'ㅖ'; 'ㅗ'; 'ㅘ'
            'ㅙ'; 'ㅚ'; 'ㅛ'; 'ㅜ'; 'ㅝ'; 'ㅞ'; 'ㅟ'; 'ㅠ'; 'ㅡ'; 'ㅢ'
            'ㅣ'
        |]
        Jongseong = [|
            '\u0000'
            'ㄱ'; 'ㄲ'; 'ㄳ'; 'ㄴ'; 'ㄵ'; 'ㄶ'; 'ㄷ'; 'ㄹ'; 'ㄺ'; 'ㄻ'
            'ㄼ'; 'ㄽ'; 'ㄾ'; 'ㄿ'; 'ㅀ'; 'ㅁ'; 'ㅂ'; 'ㅄ'; 'ㅅ'; 'ㅆ'
            'ㅇ'; 'ㅈ'; 'ㅊ'; 'ㅋ'; 'ㅌ'; 'ㅍ'; 'ㅎ'
        |]
    }

    let isChoseong c = '\u1100' <= c && c <= '\u1112'
    let isJungseong c = '\u1161' <= c && c <= '\u1175'
    let isJongseong c = '\u11A8' <= c && c <= '\u11C2'

    let choseongToIndex (c: char) =
        let index = int c - int '\u1100'
        if 0 <= index && index < ChoseongCount then Some index
        else None
    let jungseongToIndex (c: char) =
        let index = int c - int '\u1161'
        if 0 <= index && index < JungseongCount then Some index
        else None
    let jongseongToIndex c =
        if c = '\u0000' then Some 0
        else
            let index = int c - int '\u11A8' + 1
            if 0 <= index && index < JongseongCount then Some index
            else None

    let mapToIndex source = source |> Array.mapi (fun i c -> c, i) |> Map.ofArray

    let compatChoseongToIndexMap = mapToIndex compatJamos.Choseong
    let compatJungseongToIndexMap = mapToIndex compatJamos.Jungseong
    let compatJongseongToIndexMap = mapToIndex compatJamos.Jongseong

    let compatChoseongToIndex c = compatChoseongToIndexMap |> Map.tryFind c
    let compatJungseongToIndex c = compatJungseongToIndexMap |> Map.tryFind c
    let compatJongseongToIndex c = compatJongseongToIndexMap |> Map.tryFind c

    let isCompatChoseong = compatChoseongToIndex >> Option.isSome
    let isCompatJungseong = compatJungseongToIndex >> Option.isSome
    let isCompatJongseong = compatJongseongToIndex >> Option.isSome

    let map keys values = (keys, values) ||> Array.zip |> Map.ofArray

    let bokchoseongStringToCharMap =
        map [|"ᄀᄀ"; "ᄃᄃ"; "ᄇᄇ"; "ᄉᄉ"; "ᄌᄌ"|]
            [|'ᄁ'; 'ᄄ'; 'ᄈ'; 'ᄊ'; 'ᄍ'|]
    let bokjungseongStringToCharMap =
        map [|"ᅩᅡ"; "ᅩᅢ"; "ᅩᅵ"; "ᅮᅥ"; "ᅮᅦ"; "ᅮᅵ"; "ᅳᅵ"|]
            [|'ᅪ'; 'ᅫ'; 'ᅬ'; 'ᅯ'; 'ᅰ'; 'ᅱ'; 'ᅴ'|]
    let bokjongseongStringToCharMap =
        map [|"ᆨᆨ"; "ᆨᆺ"; "ᆫᆽ"; "ᆫᇂ"; "ᆯᆨ"; "ᆯᆷ"; "ᆯᆸ"
              "ᆯᆺ"; "ᆯᇀ"; "ᆯᇁ"; "ᆯᇂ"; "ᆸᆺ"; "ᆺᆺ"|]
            [|'ᆩ'; 'ᆪ'; 'ᆬ'; 'ᆭ'; 'ᆰ'; 'ᆱ'; 'ᆲ'
              'ᆳ'; 'ᆴ'; 'ᆵ'; 'ᆶ'; 'ᆹ'; 'ᆻ'|]

    let compatBokchoseongStringToCharMap =
        map [|"ㄱㄱ"; "ㄷㄷ"; "ㅂㅂ"; "ㅅㅅ"|]
            [|'ㄲ'; 'ㄸ'; 'ㅃ'; 'ㅆ'|]
    let compatBokjungseongStringToCharMap =
        map [|"ㅗㅏ"; "ㅗㅐ"; "ㅗㅣ"; "ㅜㅓ"; "ㅜㅔ"; "ㅜㅣ"; "ㅡㅣ"|]
            [|'ㅘ'; 'ㅙ'; 'ㅚ'; 'ㅝ'; 'ㅞ'; 'ㅟ'; 'ㅢ'|]
    let compatBokjongseongStringToCharMap =
        map [|"ㄱㄱ"; "ㄱㅅ"; "ㄴㅈ"; "ㄴㅎ"; "ㄹㄱ"; "ㄹㅁ"; "ㄹㅂ"
              "ㄹㅅ"; "ㄹㅌ"; "ㄹㅍ"; "ㄹㅎ"; "ㅂㅅ"; "ㅅㅅ"|]
            [|'ㄲ'; 'ㄳ'; 'ㄵ'; 'ㄶ'; 'ㄺ'; 'ㄻ'; 'ㄼ'
              'ㄽ'; 'ㄾ'; 'ㄿ'; 'ㅀ'; 'ㅄ'; 'ㅆ'|]

    let jamoStrings = [|
        "ᄀ"; "ᄀᄀ"; "ᄂ"; "ᄃ"; "ᄃᄃ"; "ᄅ"; "ᄆ"; "ᄇ"; "ᄇᄇ"; "ᄉ"
        "ᄉᄉ"; "ᄋ"; "ᄌ"; "ᄌᄌ"; "ᄎ"; "ᄏ"; "ᄐ"; "ᄑ"; "ᄒ"

        "ᅡ"; "ᅢ"; "ᅣ"; "ᅤ"; "ᅥ"; "ᅦ"; "ᅧ"; "ᅨ"; "ᅩ"; "ᅩᅡ"
        "ᅩᅢ"; "ᅩᅵ"; "ᅭ"; "ᅮ"; "ᅮᅥ"; "ᅮᅦ"; "ᅮᅵ"; "ᅲ"; "ᅳ"; "ᅳᅵ"
        "ᅵ"

        "ᆨ"; "ᆨᆨ"; "ᆨᆺ"; "ᆫ"; "ᆫᆽ"; "ᆫᇂ"; "ᆮ"; "ᆯ"; "ᆯᆨ"; "ᆯᆷ"
        "ᆯᆸ"; "ᆯᆺ"; "ᆯᇀ"; "ᆯᇁ"; "ᆯᇂ"; "ᆷ"; "ᆸ"; "ᆸᆺ"; "ᆺ"; "ᆺᆺ"
        "ᆼ"; "ᆽ"; "ᆾ"; "ᆿ"; "ᇀ"; "ᇁ"; "ᇂ"

        "ㄱ"; "ㄱㄱ"; "ㄱㅅ"; "ㄴ"; "ㄴㅈ"; "ㄴㅎ"; "ㄷ"; "ㄷㄷ"; "ㄹ"; "ㄹㄱ"
        "ㄹㅁ"; "ㄹㅂ"; "ㄹㅅ"; "ㄹㅌ"; "ㄹㅍ"; "ㄹㅎ"; "ㅁ"; "ㅂ"; "ㅂㅂ"; "ㅂㅅ"
        "ㅅ"; "ㅅㅅ"; "ㅇ"; "ㅈ"; "ㅈㅈ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"

        "ㅏ"; "ㅐ"; "ㅑ"; "ㅒ"; "ㅓ"; "ㅔ"; "ㅕ"; "ㅖ"; "ㅗ"; "ㅗㅏ"
        "ㅗㅐ"; "ㅗㅣ"; "ㅛ"; "ㅜ"; "ㅜㅓ"; "ㅜㅔ"; "ㅜㅣ"; "ㅠ"; "ㅡ"; "ㅡㅣ"
        "ㅣ"
    |]
    let jamoChars = [|
        'ᄀ'; 'ᄁ'; 'ᄂ'; 'ᄃ'; 'ᄄ'; 'ᄅ'; 'ᄆ'; 'ᄇ'; 'ᄈ'; 'ᄉ'
        'ᄊ'; 'ᄋ'; 'ᄌ'; 'ᄍ'; 'ᄎ'; 'ᄏ'; 'ᄐ'; 'ᄑ'; 'ᄒ'

        'ᅡ'; 'ᅢ'; 'ᅣ'; 'ᅤ'; 'ᅥ'; 'ᅦ'; 'ᅧ'; 'ᅨ'; 'ᅩ'; 'ᅪ'
        'ᅫ'; 'ᅬ'; 'ᅭ'; 'ᅮ'; 'ᅯ'; 'ᅰ'; 'ᅱ'; 'ᅲ'; 'ᅳ'; 'ᅴ'
        'ᅵ'

        'ᆨ'; 'ᆩ'; 'ᆪ'; 'ᆫ'; 'ᆬ'; 'ᆭ'; 'ᆮ'; 'ᆯ'; 'ᆰ'; 'ᆱ'
        'ᆲ'; 'ᆳ'; 'ᆴ'; 'ᆵ'; 'ᆶ'; 'ᆷ'; 'ᆸ'; 'ᆹ'; 'ᆺ'; 'ᆻ'
        'ᆼ'; 'ᆽ'; 'ᆾ'; 'ᆿ'; 'ᇀ'; 'ᇁ'; 'ᇂ'

        'ㄱ'; 'ㄲ'; 'ㄳ'; 'ㄴ'; 'ㄵ'; 'ㄶ'; 'ㄷ'; 'ㄸ'; 'ㄹ'; 'ㄺ'
        'ㄻ'; 'ㄼ'; 'ㄽ'; 'ㄾ'; 'ㄿ'; 'ㅀ'; 'ㅁ'; 'ㅂ'; 'ㅃ'; 'ㅄ'
        'ㅅ'; 'ㅆ'; 'ㅇ'; 'ㅈ'; 'ㅉ'; 'ㅊ'; 'ㅋ'; 'ㅌ'; 'ㅍ'; 'ㅎ'

        'ㅏ'; 'ㅐ'; 'ㅑ'; 'ㅒ'; 'ㅓ'; 'ㅔ'; 'ㅕ'; 'ㅖ'; 'ㅗ'; 'ㅘ'
        'ㅙ'; 'ㅚ'; 'ㅛ'; 'ㅜ'; 'ㅝ'; 'ㅞ'; 'ㅟ'; 'ㅠ'; 'ㅡ'; 'ㅢ'
        'ㅣ'
    |]

    let bokjamosAsString' = [|
        "ᄀᄀ"; "ᄃᄃ"; "ᄇᄇ"; "ᄉᄉ"; "ᄌᄌ"
        "ᅩᅡ"; "ᅩᅢ"; "ᅩᅵ"; "ᅮᅥ"; "ᅮᅦ"; "ᅮᅵ"; "ᅳᅵ"
        "ᆨᆨ"; "ᆨᆺ"; "ᆫᆽ"; "ᆫᇂ"; "ᆯᆨ"; "ᆯᆷ"; "ᆯᆸ"
        "ᆯᆺ"; "ᆯᇀ"; "ᆯᇁ"; "ᆯᇂ"; "ᆸᆺ"; "ᆺᆺ"

        "ㄱㄱ"; "ㄱㅅ"; "ㄴㅈ"; "ㄴㅎ"; "ㄷㄷ"; "ㄹㄱ"; "ㄹㅁ"; "ㄹㅂ"
        "ㄹㅅ"; "ㄹㅌ"; "ㄹㅍ"; "ㄹㅎ"; "ㅂㅂ"; "ㅂㅅ"; "ㅅㅅ"
        "ㅗㅏ"; "ㅗㅐ"; "ㅗㅣ"; "ㅜㅓ"; "ㅜㅔ"; "ㅜㅣ"; "ㅡㅣ"
    |]
    let bokjamosAsChar' = [|
        'ᄁ'; 'ᄄ'; 'ᄈ'; 'ᄊ'; 'ᄍ'
        'ᅪ'; 'ᅫ'; 'ᅬ'; 'ᅯ'; 'ᅰ'; 'ᅱ'; 'ᅴ'
        'ᆩ'; 'ᆪ'; 'ᆬ'; 'ᆭ'; 'ᆰ'; 'ᆱ'; 'ᆲ'
        'ᆳ'; 'ᆴ'; 'ᆵ'; 'ᆶ'; 'ᆹ'; 'ᆻ'

        'ㄲ'; 'ㄳ'; 'ㄵ'; 'ㄶ'; 'ㄸ'; 'ㄺ'; 'ㄻ'; 'ㄼ'
        'ㄽ'; 'ㄾ'; 'ㄿ'; 'ㅀ'; 'ㅃ'; 'ㅄ'; 'ㅆ'
        'ㅘ'; 'ㅙ'; 'ㅚ'; 'ㅝ'; 'ㅞ'; 'ㅟ'; 'ㅢ'
    |]

    let jamoNormalizationMap = map jamoStrings jamoChars
    let jamoDenormalizationMap = map jamoChars jamoStrings

    let normalizeJamo jamo =
        match String.length jamo with
        | 0 -> Some '\u0000'
        | 1 -> match jamoChars |> Array.tryBinarySearch jamo.[0] with
               | Some index -> Some jamoChars.[index]
               | None -> None
        | 2 -> jamoNormalizationMap |> Map.tryFind jamo
        | _ -> None

    let denormalizeJamo jamo =
        match jamoDenormalizationMap |> Map.tryFind jamo with
        | Some jamoAsString -> jamoAsString
        | None -> string jamo

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

        composeIndexes choIndex jungIndex jongIndex

    let composeStrings choseong jungseong jongseong =
        let invalid argName arg =
            invalidArg argName <| sprintf "%A is not a %s" arg argName

        let cho = normalizeJamo choseong
        let jung = normalizeJamo jungseong
        let jong = normalizeJamo jongseong

        match cho, jung, jong with
        | Some cho, Some jung, Some jong -> compose cho jung jong
        | None, _, _ -> invalid "choseong" choseong
        | _, None, _ -> invalid "jungseong" jungseong
        | _, _, None -> invalid "jongseong" jongseong

    let decomposeInto collection syllable =
        if not (syllable |> isSyllable) then
            invalidArg "syllable" <| sprintf "%c is not a Hangul syllable" syllable

        let choIndex, jungIndex, jongIndex = syllable |> decomposeIntoIndexes

        collection.Choseong.[choIndex],
        collection.Jungseong.[jungIndex],
        collection.Jongseong.[jongIndex]

    let decompose = decomposeInto jamos
    let decomposeCompat = decomposeInto compatJamos
    let decomposeIntoStrings = decomposeInto jamosAsString
    let decomposeCompatIntoStrings = decomposeInto compatJamosAsString

open KoreanChar

type KoreanChar private () =
    static member IsSyllable c = isSyllable c

    static member IsChoseong c = isChoseong c
    static member IsJungseong c = isJungseong c
    static member IsJongseong c = isJongseong c

    static member IsCompatChoseong c = isCompatChoseong c
    static member IsCompatJungseong c = isCompatJungseong c
    static member IsCompatJongseong c = isCompatJongseong c

    static member Compose(choseong, jungseong, jongseong) =
        compose choseong jungseong jongseong

    static member Compose(choseong, jungseong, jongseong) =
        composeStrings choseong jungseong jongseong

    static member Decompose syllable =
        decompose syllable

    static member DecomposeCompat syllable =
        decomposeCompat syllable

    static member DecomposeIntoStrings syllable =
        decomposeIntoStrings syllable

    static member DecomposeCompatIntoStrings syllable =
        decomposeCompatIntoStrings syllable
