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

    static member op_Explicit(syllable) =
        match syllable with
        | Choseong cho ->
            KoreanChar.combineJamo cho
        | Jungseong jung ->
            KoreanChar.combineJamo jung
        | Jongseong jong ->
            KoreanChar.combineJamo jong
        | ChoJungseong (cho, jung) ->
            KoreanChar.composeStrings cho jung ""
        | ChoJungJongseong (cho, jung, jong) ->
            KoreanChar.composeStrings cho jung jong

let choseong = anyStringOf [
    "ᄀᄀ"; "ᄃᄃ"; "ᄇᄇ"; "ᄉᄉ"; "ᄌᄌ"
    "ᄀ"; "ᄁ"; "ᄂ"; "ᄃ"; "ᄄ"; "ᄅ"; "ᄆ"; "ᄇ"; "ᄈ"; "ᄉ"
    "ᄊ"; "ᄋ"; "ᄌ"; "ᄍ"; "ᄎ"; "ᄏ"; "ᄐ"; "ᄑ"; "ᄒ"
]
let jungseong = anyStringOf [
    "ᅩᅡ"; "ᅩᅢ"; "ᅩᅵ"; "ᅮᅥ"; "ᅮᅦ"; "ᅮᅵ"; "ᅳᅵ"
    "ᅡ"; "ᅢ"; "ᅣ"; "ᅤ"; "ᅥ"; "ᅦ"; "ᅧ"; "ᅨ"; "ᅩ"; "ᅪ"
    "ᅫ"; "ᅬ"; "ᅭ"; "ᅮ"; "ᅯ"; "ᅰ"; "ᅱ"; "ᅲ"; "ᅳ"; "ᅴ"
    "ᅵ"
]
let jongseong = anyStringOf [
    "ᆨᆨ"; "ᆨᆺ"; "ᆫᆽ"; "ᆫᇂ"; "ᆯᆨ"; "ᆯᆷ"; "ᆯᆸ"
    "ᆯᆺ"; "ᆯᇀ"; "ᆯᇁ"; "ᆯᇂ"; "ᆸᆺ"; "ᆺᆺ"
    "ᆨ"; "ᆩ"; "ᆪ"; "ᆫ"; "ᆬ"; "ᆭ"; "ᆮ"; "ᆯ"; "ᆰ"; "ᆱ"
    "ᆲ"; "ᆳ"; "ᆴ"; "ᆵ"; "ᆶ"; "ᆷ"; "ᆸ"; "ᆹ"; "ᆺ"; "ᆻ"
    "ᆼ"; "ᆽ"; "ᆾ"; "ᆿ"; "ᇀ"; "ᇁ"; "ᇂ"
]

let syllable : Parser<Syllable, unit> =
    choice [
        tuple3BT choseong jungseong jongseong |>> ChoJungJongseong
        tuple2BT choseong jungseong |>> ChoJungseong
        choseong |>> Choseong
        jungseong |>> Jungseong
        jongseong |>> Jongseong
    ]

let syllableChar = syllable |>> char
