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

module KoreanPowerPack.SebeolsikParsers

open FParsec

type Jamo = string

type Syllable =
    | Choseong of Jamo
    | Jungseong of Jamo
    | Jongseong of Jamo
    | ChoJungseong of Jamo * Jamo
    | ChoJungJongseong of Jamo * Jamo * Jamo

    static member op_Explicit(syllable): char =
        match syllable with
        | Choseong cho ->
            KoreanChar.joinJamo cho
        | Jungseong jung ->
            KoreanChar.joinJamo jung
        | Jongseong jong ->
            KoreanChar.joinJamo jong
        | ChoJungseong (cho, jung) ->
            KoreanChar.composeWithStrings cho jung ""
        | ChoJungJongseong (cho, jung, jong) ->
            KoreanChar.composeWithStrings cho jung jong

let pchoseong = anyStringOf [
    "ᄀᄀ"; "ᄃᄃ"; "ᄇᄇ"; "ᄉᄉ"; "ᄌᄌ"
    "ᄀ"; "ᄁ"; "ᄂ"; "ᄃ"; "ᄄ"; "ᄅ"; "ᄆ"; "ᄇ"; "ᄈ"; "ᄉ"
    "ᄊ"; "ᄋ"; "ᄌ"; "ᄍ"; "ᄎ"; "ᄏ"; "ᄐ"; "ᄑ"; "ᄒ"
]
let pjungseong = anyStringOf [
    "ᅩᅡ"; "ᅩᅢ"; "ᅩᅵ"; "ᅮᅥ"; "ᅮᅦ"; "ᅮᅵ"; "ᅳᅵ"
    "ᅡ"; "ᅢ"; "ᅣ"; "ᅤ"; "ᅥ"; "ᅦ"; "ᅧ"; "ᅨ"; "ᅩ"; "ᅪ"
    "ᅫ"; "ᅬ"; "ᅭ"; "ᅮ"; "ᅯ"; "ᅰ"; "ᅱ"; "ᅲ"; "ᅳ"; "ᅴ"
    "ᅵ"
]
let pjongseong = anyStringOf [
    "ᆨᆨ"; "ᆨᆺ"; "ᆫᆽ"; "ᆫᇂ"; "ᆯᆨ"; "ᆯᆷ"; "ᆯᆸ"
    "ᆯᆺ"; "ᆯᇀ"; "ᆯᇁ"; "ᆯᇂ"; "ᆸᆺ"; "ᆺᆺ"
    "ᆨ"; "ᆩ"; "ᆪ"; "ᆫ"; "ᆬ"; "ᆭ"; "ᆮ"; "ᆯ"; "ᆰ"; "ᆱ"
    "ᆲ"; "ᆳ"; "ᆴ"; "ᆵ"; "ᆶ"; "ᆷ"; "ᆸ"; "ᆹ"; "ᆺ"; "ᆻ"
    "ᆼ"; "ᆽ"; "ᆾ"; "ᆿ"; "ᇀ"; "ᇁ"; "ᇂ"
]

let psyllable: Parser<Syllable, unit> = choice [
    tuple3BT pchoseong pjungseong pjongseong |>> ChoJungJongseong
    tuple2BT pchoseong pjungseong |>> ChoJungseong
    pchoseong |>> Choseong
    pjungseong |>> Jungseong
    pjongseong |>> Jongseong
]

let psyllableChar = psyllable |>> char
