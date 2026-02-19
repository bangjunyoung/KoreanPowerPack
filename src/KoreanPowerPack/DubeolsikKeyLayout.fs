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

module KoreanPowerPack.DubeolsikKeyLayout

open HangulKeyLayout

let qwertyToDubeolsik, dubeolsikToQwerty =
    let qwerty =
        "qwertyuiop" +
        "asdfghjkl" +
        "zxcvbnm" +
        "QWERTOP"
    let dubeolsik =
        "ㅂㅈㄷㄱㅅㅛㅕㅑㅐㅔ" +
        "ㅁㄴㅇㄹㅎㅗㅓㅏㅣ" +
        "ㅋㅌㅊㅍㅠㅜㅡ" +
        "ㅃㅉㄸㄲㅆㅒㅖ"
    assert (qwerty.Length = dubeolsik.Length)
    mapping qwerty dubeolsik, mapping dubeolsik qwerty

[<CompiledName("Parse")>]
let parse str =
    if isNull str then nullArg (nameof str)

    str
    |> String.map qwertyToDubeolsik
    |> HangulKeyLayout.parse DubeolsikParsers.psyllableChar

[<CompiledName("Unparse")>]
let unparse str =
    if isNull str then nullArg (nameof str)

    str
    |> HangulKeyLayout.unparse (fun c ->
        if c |> KoreanChar.isCompatJungseong then
            KoreanChar.splitJamo c
        elif c |> KoreanChar.isSyllable then
            KoreanChar.decomposeToDubeolsik c |> String.concat ""
        else
            string c)
    |> String.map dubeolsikToQwerty
