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

module Rembris.Globalization.DubeolsikParsers

open FParsec
open Rembris.FParsec
open KoreanChar

type Jamo = string

type Syllable =
    | Choseong of Jamo
    | Jungseong of Jamo
    | ChoJungseong of Jamo * Jamo
    | ChoJungJongseong of Jamo * Jamo * Jamo

    static member op_Explicit(syllable) =
        match syllable with
        | Choseong cho -> compatChoseongToChar cho
        | Jungseong jung -> compatJungseongToChar jung
        | ChoJungseong(cho, jung) -> composeCompat cho jung ""
        | ChoJungJongseong(cho, jung, jong) -> composeCompat cho jung jong

let choseong = anyStringOf [
    "ㄱ"; "ㄲ"; "ㄴ"; "ㄷ"; "ㄸ"; "ㄹ"; "ㅁ"; "ㅂ"; "ㅃ"; "ㅅ"
    "ㅆ"; "ㅇ"; "ㅈ"; "ㅉ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"
]
let jungseong = anyStringOf [
    "ㅗㅏ"; "ㅗㅐ"; "ㅗㅣ"; "ㅜㅓ"; "ㅜㅔ"; "ㅜㅣ"; "ㅡㅣ"
    "ㅏ"; "ㅐ"; "ㅑ"; "ㅒ"; "ㅓ"; "ㅔ"; "ㅕ"
    "ㅖ"; "ㅗ"; "ㅛ"; "ㅜ"; "ㅠ"; "ㅡ"; "ㅣ"
]
let bokjongseong = anyStringOf [
    "ㄱㅅ"; "ㄴㅈ"; "ㄴㅎ"; "ㄹㄱ"; "ㄹㅁ"; "ㄹㅂ"
    "ㄹㅅ"; "ㄹㅌ"; "ㄹㅍ"; "ㄹㅎ"; "ㅂㅅ"
]
let danjongseong = anyStringOf [
    "ㄱ"; "ㄲ"; "ㄴ"; "ㄷ"; "ㄹ"; "ㅁ"; "ㅂ"; "ㅅ"
    "ㅆ"; "ㅇ"; "ㅈ"; "ㅊ"; "ㅋ"; "ㅌ"; "ㅍ"; "ㅎ"
]

let syllable : Parser<Syllable, unit> = choice [
    tuple3BT choseong jungseong ((bokjongseong .>>? notFollowedBy jungseong) <|>
                                 (danjongseong .>>? notFollowedBy jungseong))
        |>> ChoJungJongseong
    tuple2BT choseong jungseong |>> ChoJungseong
    choseong |>> Choseong
    jungseong |>> Jungseong
]

let syllableChar = syllable |>> char
