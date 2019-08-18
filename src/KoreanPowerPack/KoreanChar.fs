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

module internal KoreanChar =
    let [<Literal>] HangulSyllableFirst = '가'
    let [<Literal>] HangulSyllableLast = '힣'

    let isSyllable c = HangulSyllableFirst <= c && c <= HangulSyllableLast

    let [<Literal>] ChoseongCount = 19
    let [<Literal>] JungseongCount = 21
    let [<Literal>] JongseongCount = 28

    let composeFromIndexes choIndex jungIndex jongIndex =
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

    let jamos = {
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
            ""; "ᆨ"; "ᆩ"; "ᆪ"; "ᆫ"; "ᆬ"; "ᆭ"; "ᆮ"; "ᆯ"; "ᆰ"
            "ᆱ"; "ᆲ"; "ᆳ"; "ᆴ"; "ᆵ"; "ᆶ"; "ᆷ"; "ᆸ"; "ᆹ"; "ᆺ"
            "ᆻ"; "ᆼ"; "ᆽ"; "ᆾ"; "ᆿ"; "ᇀ"; "ᇁ"; "ᇂ"
        |]
    }

    let jamosAsChar = {
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

    let compatJamos = {
        Choseong = [|
            "ㄱ"; "ㄲ"; "ㄴ"; "ㄷ"; "ㄸ"; "ㄹ"; "ㅁ"; "ㅂ"; "ㅃ"; "ㅅ"
            "ㅆ"; "ㅇ"; "ㅈ"; "ㅉ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"
        |]
        Jungseong = [|
            "ㅏ"; "ㅐ"; "ㅑ"; "ㅒ"; "ㅓ"; "ㅔ"; "ㅕ"; "ㅖ"; "ㅗ"; "ㅗㅏ"
            "ㅗㅐ"; "ㅗㅣ"; "ㅛ"; "ㅜ"; "ㅜㅓ"; "ㅜㅔ"; "ㅜㅣ"; "ㅠ"; "ㅡ"; "ㅡㅣ"
            "ㅣ"
        |]
        Jongseong = [|
            ""; "ㄱ"; "ㄲ"; "ㄱㅅ"; "ㄴ"; "ㄴㅈ"; "ㄴㅎ"; "ㄷ"; "ㄹ"; "ㄹㄱ"
            "ㄹㅁ"; "ㄹㅂ"; "ㄹㅅ"; "ㄹㅌ"; "ㄹㅍ"; "ㄹㅎ"; "ㅁ"; "ㅂ"; "ㅂㅅ"; "ㅅ"
            "ㅆ"; "ㅇ"; "ㅈ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"
        |]
    }

    let compatJamosAsChar = {
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
            '\u0000'; 'ㄱ'; 'ㄲ'; 'ㄳ'; 'ㄴ'; 'ㄵ'; 'ㄶ'; 'ㄷ'; 'ㄹ'; 'ㄺ'
            'ㄻ'; 'ㄼ'; 'ㄽ'; 'ㄾ'; 'ㄿ'; 'ㅀ'; 'ㅁ'; 'ㅂ'; 'ㅄ'; 'ㅅ'
            'ㅆ'; 'ㅇ'; 'ㅈ'; 'ㅊ'; 'ㅋ'; 'ㅌ'; 'ㅍ'; 'ㅎ'
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

    let map key value = (key, value) ||> Array.zip |> Map.ofArray

    let choseongToCharMap = map jamos.Choseong jamosAsChar.Choseong
    let jungseongToCharMap = map jamos.Jungseong jamosAsChar.Jungseong
    let jongseongToCharMap = map jamos.Jongseong jamosAsChar.Jongseong
    let bokjongseongToCharMap = Map.ofArray [|
        "ᆨᆨ", 'ᆩ'; "ᆨᆺ", 'ᆪ'; "ᆫᆽ", 'ᆬ'; "ᆫᇂ", 'ᆭ'; "ᆯᆨ", 'ᆰ'
        "ᆯᆷ", 'ᆱ'; "ᆯᆸ", 'ᆲ'; "ᆯᆺ", 'ᆳ'; "ᆯᇀ", 'ᆴ'; "ᆯᇁ", 'ᆵ'
        "ᆯᇂ", 'ᆶ'; "ᆸᆺ", 'ᆹ'; "ᆺᆺ", 'ᆻ'
    |]

    let choseongToChar choseong = choseongToCharMap |> Map.find choseong
    let jungseongToChar jungseong = jungseongToCharMap |> Map.find jungseong
    let jongseongToChar jongseong =
        match bokjongseongToCharMap |> Map.tryFind jongseong with
        | Some jong -> jong
        | None -> jongseongToCharMap |> Map.find jongseong

    let compatChoseongToCharMap = map compatJamos.Choseong compatJamosAsChar.Choseong
    let compatJungseongToCharMap = map compatJamos.Jungseong compatJamosAsChar.Jungseong
    let compatJongseongToCharMap = map compatJamos.Jongseong compatJamosAsChar.Jongseong

    let compatChoseongToChar choseong = compatChoseongToCharMap |> Map.find choseong
    let compatJungseongToChar jungseong = compatJungseongToCharMap |> Map.find jungseong
    let compatJongseongToChar jongseong = compatJongseongToCharMap |> Map.find jongseong

    let mapJamoToIndex jamos = jamos |> Array.mapi (fun index c -> c, index) |> Map.ofArray

    let compatChoseongToIndexMap = mapJamoToIndex compatJamosAsChar.Choseong
    let compatJungseongToIndexMap = mapJamoToIndex compatJamosAsChar.Jungseong
    let compatJongseongToIndexMap = mapJamoToIndex compatJamosAsChar.Jongseong

    let compatChoseongToIndex c = compatChoseongToIndexMap |> Map.tryFind c
    let compatJungseongToIndex c = compatJungseongToIndexMap |> Map.tryFind c
    let compatJongseongToIndex c = compatJongseongToIndexMap |> Map.tryFind c

    let isCompatChoseong = compatChoseongToIndex >> Option.isSome
    let isCompatJungseong = compatJungseongToIndex >> Option.isSome
    let isCompatJongseong = compatJongseongToIndex >> Option.isSome

    let compose choseong jungseong jongseong =
        let convert argName f g x =
            match f x with
            | Some index -> index
            | None ->
                match g x with
                | Some index -> index
                | None -> invalidArg argName ("not a " + argName)

        let choIndex = convert "choseong" compatChoseongToIndex choseongToIndex choseong
        let jungIndex = convert "jungseong" compatJungseongToIndex jungseongToIndex jungseong
        let jongIndex = convert "jongseong" compatJongseongToIndex jongseongToIndex jongseong

        composeFromIndexes choIndex jungIndex jongIndex

    let decomposeWith table syllable =
        if not (syllable |> isSyllable) then
            invalidArg "syllable" <| sprintf "%c is not a Hangul syllable" syllable

        let choIndex, jungIndex, jongIndex = syllable |> decomposeIntoIndexes
        table.Choseong.[choIndex], table.Jungseong.[jungIndex], table.Jongseong.[jongIndex]

    let decompose = decomposeWith jamos
    let decomposeCompat = decomposeWith compatJamos

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

    static member Decompose syllable =
        decomposeWith jamosAsChar syllable

    static member DecomposeCompat syllable =
        decomposeWith compatJamosAsChar syllable
