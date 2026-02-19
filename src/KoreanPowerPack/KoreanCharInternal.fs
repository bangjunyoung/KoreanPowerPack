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

open FSharpCoreMissingParts

module internal KoreanCharInternal =
    let [<Literal>] HangulSyllableFirst = '가'
    let [<Literal>] HangulSyllableLast = '힣'

    let syllableToIndex syllable =
        int (syllable - HangulSyllableFirst)

    let [<Literal>] ChoseongCount = 19
    let [<Literal>] JungseongCount = 21
    let [<Literal>] JongseongCount = 28

    let getChoseongIndex syllable =
        let sylIndex = syllableToIndex syllable
        sylIndex / (JungseongCount * JongseongCount)
    let getJungseongIndex syllable =
        let sylIndex = syllableToIndex syllable
        sylIndex % (JungseongCount * JongseongCount) / JongseongCount
    let getJongseongIndex syllable =
        let sylIndex = syllableToIndex syllable
        sylIndex % JongseongCount

    type JamoCollection<'a> = { Choseong: 'a[]; Jungseong: 'a[]; Jongseong: 'a[] }

    let JoinedJamos = {
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
            '\u0000'
            'ᆨ'; 'ᆩ'; 'ᆪ'; 'ᆫ'; 'ᆬ'; 'ᆭ'; 'ᆮ'; 'ᆯ'; 'ᆰ'; 'ᆱ'
            'ᆲ'; 'ᆳ'; 'ᆴ'; 'ᆵ'; 'ᆶ'; 'ᆷ'; 'ᆸ'; 'ᆹ'; 'ᆺ'; 'ᆻ'
            'ᆼ'; 'ᆽ'; 'ᆾ'; 'ᆿ'; 'ᇀ'; 'ᇁ'; 'ᇂ'
        |]
    }

    let SplitJamos = {
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
            ""
            "ᆨ"; "ᆨᆨ"; "ᆨᆺ"; "ᆫ"; "ᆫᆽ"; "ᆫᇂ"; "ᆮ"; "ᆯ"; "ᆯᆨ"; "ᆯᆷ"
            "ᆯᆸ"; "ᆯᆺ"; "ᆯᇀ"; "ᆯᇁ"; "ᆯᇂ"; "ᆷ"; "ᆸ"; "ᆸᆺ"; "ᆺ"; "ᆺᆺ"
            "ᆼ"; "ᆽ"; "ᆾ"; "ᆿ"; "ᇀ"; "ᇁ"; "ᇂ"
        |]
    }

    let JoinedCompatJamos = {
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

    let SplitCompatJamos = {
        Choseong = [|
            "ㄱ"; "ㄱㄱ"; "ㄴ"; "ㄷ"; "ㄷㄷ"; "ㄹ"; "ㅁ"; "ㅂ"; "ㅂㅂ"; "ㅅ"
            "ㅅㅅ"; "ㅇ"; "ㅈ"; "ㅉ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"
        |]
        Jungseong = [|
            "ㅏ"; "ㅐ"; "ㅑ"; "ㅒ"; "ㅓ"; "ㅔ"; "ㅕ"; "ㅖ"; "ㅗ"; "ㅗㅏ"
            "ㅗㅐ"; "ㅗㅣ"; "ㅛ"; "ㅜ"; "ㅜㅓ"; "ㅜㅔ"; "ㅜㅣ"; "ㅠ"; "ㅡ"; "ㅡㅣ"
            "ㅣ"
        |]
        Jongseong = [|
            ""
            "ㄱ"; "ㄱㄱ"; "ㄱㅅ"; "ㄴ"; "ㄴㅈ"; "ㄴㅎ"; "ㄷ"; "ㄹ"; "ㄹㄱ"; "ㄹㅁ"
            "ㄹㅂ"; "ㄹㅅ"; "ㄹㅌ"; "ㄹㅍ"; "ㄹㅎ"; "ㅁ"; "ㅂ"; "ㅂㅅ"; "ㅅ"; "ㅅㅅ"
            "ㅇ"; "ㅈ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"
        |]
    }

    let DubeolsikJamos = {
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
            ""
            "ㄱ"; "ㄲ"; "ㄱㅅ"; "ㄴ"; "ㄴㅈ"; "ㄴㅎ"; "ㄷ"; "ㄹ"; "ㄹㄱ"; "ㄹㅁ"
            "ㄹㅂ"; "ㄹㅅ"; "ㄹㅌ"; "ㄹㅍ"; "ㄹㅎ"; "ㅁ"; "ㅂ"; "ㅂㅅ"; "ㅅ"; "ㅆ"
            "ㅇ"; "ㅈ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"
        |]
    }

    let choseongToIndex choseong =
        let index = int (choseong - '\u1100')
        if 0 <= index && index < ChoseongCount then Some index
        else None
    let jungseongToIndex jungseong =
        let index = int (jungseong - '\u1161')
        if 0 <= index && index < JungseongCount then Some index
        else None
    let jongseongToIndex jongseong =
        if jongseong = '\u0000' then Some 0
        else
            let index = int (jongseong - '\u11A8') + 1
            if 0 <= index && index < JongseongCount then Some index
            else None

    let compatChoseongToIndex choseong =
        JoinedCompatJamos.Choseong |> Array.tryBinarySearch choseong

    let compatJungseongToIndex jungseong =
        let index = int (jungseong - '\u314F')
        if 0 <= index && index < JungseongCount then Some index
        else None

    let compatJongseongToIndex jongseong =
        JoinedCompatJamos.Jongseong |> Array.tryBinarySearch jongseong

    let JoinedUnifiedJamos = [|
        // Hangul Jamo
        'ᄀ'; 'ᄁ'; 'ᄂ'; 'ᄃ'; 'ᄄ'; 'ᄅ'; 'ᄆ'; 'ᄇ'; 'ᄈ'; 'ᄉ'
        'ᄊ'; 'ᄋ'; 'ᄌ'; 'ᄍ'; 'ᄎ'; 'ᄏ'; 'ᄐ'; 'ᄑ'; 'ᄒ'

        'ᅡ'; 'ᅢ'; 'ᅣ'; 'ᅤ'; 'ᅥ'; 'ᅦ'; 'ᅧ'; 'ᅨ'; 'ᅩ'; 'ᅪ'
        'ᅫ'; 'ᅬ'; 'ᅭ'; 'ᅮ'; 'ᅯ'; 'ᅰ'; 'ᅱ'; 'ᅲ'; 'ᅳ'; 'ᅴ'
        'ᅵ'

        'ᆨ'; 'ᆩ'; 'ᆪ'; 'ᆫ'; 'ᆬ'; 'ᆭ'; 'ᆮ'; 'ᆯ'; 'ᆰ'; 'ᆱ'
        'ᆲ'; 'ᆳ'; 'ᆴ'; 'ᆵ'; 'ᆶ'; 'ᆷ'; 'ᆸ'; 'ᆹ'; 'ᆺ'; 'ᆻ'
        'ᆼ'; 'ᆽ'; 'ᆾ'; 'ᆿ'; 'ᇀ'; 'ᇁ'; 'ᇂ'

        // Hangul Compatibility Jamo
        'ㄱ'; 'ㄲ'; 'ㄳ'; 'ㄴ'; 'ㄵ'; 'ㄶ'; 'ㄷ'; 'ㄸ'; 'ㄹ'; 'ㄺ'
        'ㄻ'; 'ㄼ'; 'ㄽ'; 'ㄾ'; 'ㄿ'; 'ㅀ'; 'ㅁ'; 'ㅂ'; 'ㅃ'; 'ㅄ'
        'ㅅ'; 'ㅆ'; 'ㅇ'; 'ㅈ'; 'ㅉ'; 'ㅊ'; 'ㅋ'; 'ㅌ'; 'ㅍ'; 'ㅎ'

        'ㅏ'; 'ㅐ'; 'ㅑ'; 'ㅒ'; 'ㅓ'; 'ㅔ'; 'ㅕ'; 'ㅖ'; 'ㅗ'; 'ㅘ'
        'ㅙ'; 'ㅚ'; 'ㅛ'; 'ㅜ'; 'ㅝ'; 'ㅞ'; 'ㅟ'; 'ㅠ'; 'ㅡ'; 'ㅢ'
        'ㅣ'
    |]

    let SplitUnifiedJamos = [|
        // Hangul Jamo
        "ᄀ"; "ᄀᄀ"; "ᄂ"; "ᄃ"; "ᄃᄃ"; "ᄅ"; "ᄆ"; "ᄇ"; "ᄇᄇ"; "ᄉ"
        "ᄉᄉ"; "ᄋ"; "ᄌ"; "ᄌᄌ"; "ᄎ"; "ᄏ"; "ᄐ"; "ᄑ"; "ᄒ"

        "ᅡ"; "ᅢ"; "ᅣ"; "ᅤ"; "ᅥ"; "ᅦ"; "ᅧ"; "ᅨ"; "ᅩ"; "ᅩᅡ"
        "ᅩᅢ"; "ᅩᅵ"; "ᅭ"; "ᅮ"; "ᅮᅥ"; "ᅮᅦ"; "ᅮᅵ"; "ᅲ"; "ᅳ"; "ᅳᅵ"
        "ᅵ"

        "ᆨ"; "ᆨᆨ"; "ᆨᆺ"; "ᆫ"; "ᆫᆽ"; "ᆫᇂ"; "ᆮ"; "ᆯ"; "ᆯᆨ"; "ᆯᆷ"
        "ᆯᆸ"; "ᆯᆺ"; "ᆯᇀ"; "ᆯᇁ"; "ᆯᇂ"; "ᆷ"; "ᆸ"; "ᆸᆺ"; "ᆺ"; "ᆺᆺ"
        "ᆼ"; "ᆽ"; "ᆾ"; "ᆿ"; "ᇀ"; "ᇁ"; "ᇂ"

        // Hangul Compatibility Jamo
        "ㄱ"; "ㄱㄱ"; "ㄱㅅ"; "ㄴ"; "ㄴㅈ"; "ㄴㅎ"; "ㄷ"; "ㄷㄷ"; "ㄹ"; "ㄹㄱ"
        "ㄹㅁ"; "ㄹㅂ"; "ㄹㅅ"; "ㄹㅌ"; "ㄹㅍ"; "ㄹㅎ"; "ㅁ"; "ㅂ"; "ㅂㅂ"; "ㅂㅅ"
        "ㅅ"; "ㅅㅅ"; "ㅇ"; "ㅈ"; "ㅈㅈ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"

        "ㅏ"; "ㅐ"; "ㅑ"; "ㅒ"; "ㅓ"; "ㅔ"; "ㅕ"; "ㅖ"; "ㅗ"; "ㅗㅏ"
        "ㅗㅐ"; "ㅗㅣ"; "ㅛ"; "ㅜ"; "ㅜㅓ"; "ㅜㅔ"; "ㅜㅣ"; "ㅠ"; "ㅡ"; "ㅡㅣ"
        "ㅣ"
    |]

    assert (SplitUnifiedJamos.Length = JoinedUnifiedJamos.Length)
