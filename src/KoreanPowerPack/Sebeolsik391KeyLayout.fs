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

module KoreanPowerPack.Sebeolsik391KeyLayout

open KoreanPowerPack.FSharp
open HangulKeyLayout

let qwertyToSebeolsik391, sebeolsik391ToQwerty =
    let qwerty =
        "`1234567890-=\\" +
        "qwertyuiop[]" +
        "asdfghjkl;'" +
        "zxcvbnm,./" +
        "~!@#$%^&*()_+|" +
        "QWERTYUIOP{}" + 
        "ASDFGHJKL:\"" +
        "ZXCVBNM<>?"
    let sebeolsik391 =
        "*ᇂᆻᆸᅭᅲᅣᅨᅴᅮᄏ)>:" +
        "ᆺᆯᅧᅢᅥᄅᄃᄆᄎᄑ(<" +
        "ᆼᆫᅵᅡᅳᄂᄋᄀᄌᄇᄐ" +
        "ᆷᆨᅦᅩᅮᄉᄒ,.ᅩ" +
        "※ᆩᆰᆽᆵᆴ=“”'~;>\\" +
        "ᇁᇀᆬᆶᆳ56789%/" +
        "ᆮᆭᆲᆱᅤ01234·" +
        "ᆾᆹᆿᆪ?-\",.!"
    assert (qwerty.Length = sebeolsik391.Length)
    mapping qwerty sebeolsik391, mapping sebeolsik391 qwerty

[<CompiledName("Parse")>]
let parse str =
    raiseIfNull "str" str

    str
    |> String.map qwertyToSebeolsik391
    |> HangulKeyLayout.parse SebeolsikParsers.syllableChar

[<CompiledName("Unparse")>]
let unparse str =
    raiseIfNull "str" str

    str
    |> HangulKeyLayout.unparse (fun c ->
        if c |> KoreanChar.isJungseong then
            KoreanChar.decomposeJungseong c
        elif c |> KoreanChar.isSyllable then
            let (cho, jung, jong) = KoreanChar.decomposeIntoStrings c
            cho + jung + jong
        else
            string c)
    |> String.map sebeolsik391ToQwerty
