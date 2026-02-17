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

module KoreanPowerPack.Sebeolsik391KeyLayoutTests

open NUnit.Framework
open KoreanPowerPack.FSharp

let parseTestParameters =
    [
        ("", "")
        ("khuyi;njlo0'pm",
         "ᄀᄂᄃᄅᄆᄇᄉᄋᄌᄎᄏᄐᄑᄒ")
        ("kkuu;;nnll",
         "ᄁᄄᄈᄊᄍ")
        ("fr6Gtce7v4b5g d",
         "ᅡᅢᅣᅤᅥᅦᅧᅨᅩᅭᅮᅲᅳ ᅵ")
        ("vfvrvdbtbcbdgd",
         "ᅪᅫᅬᅯᅰᅱᅴ")
        ("j50vrmr2uts uufqlbd 'vkkdpbw llvZkd ;f;;gz",
         "유쾌했던 땃쥐 토끼풀 쫓기 바쁨")
        ("hf!nd nfxq ;fAjgye jfs#ufkf nrants mfwWjfitxuts nfwxjgw ;fw3jgs",
         "낚시 삯 받으려 앉다가 생선 핥아먹던 삵을 밟은")
        ("jvdkvwqjgs nfwzjd jrufw1kv ifzjd pesofs1jf ndygw jgwQjt2uf",
         "외곬은 삶이 애닳고 맘이 편찮아 시를 읊었다")
    ]
    |> List.map (fun (actual, expected) ->
        TestCaseData(actual).Returns(expected)
            .SetName($"{nameof Sebeolsik391KeyLayout.parse}(\"{actual}\")"))

[<TestCaseSource(nameof parseTestParameters)>]
let parseTest actual =
    Sebeolsik391KeyLayout.parse actual

let unparseTestParameters =
    [
        ("", "")
        ("ᄀᄂᄃᄅᄆᄇᄉᄋᄌᄎᄏᄐᄑᄒ",
         "khuyi;njlo0'pm")
        ("ᄁᄄᄈᄊᄍ",
         "kkuu;;nnll")
        ("ᅡᅢᅣᅤᅥᅦᅧᅨᅩᅭᅮᅲᅳ ᅵ",
         "fr6Gtce7/4b5g d")
        ("ᅪᅫᅬᅯᅰᅱᅴ",
         "/f/r/dbtbcbdgd")
        ("ᆨᆫᆮᆯᆷᆸᆺᆼᆽᆾᆿᇀᇁᇂ",
         "xsAwz3qa#ZCWQ1")
        ("ᆩᆪᆬᆭᆰᆱᆲᆳᆴᆵᆶᆹᆻ",
         "xxxqs#s1wxwzw3wqwWwQw13qqq")
        ("유쾌했던 땃쥐 토끼풀 쫓기 바쁨",
         "j50/rmrqquts uufqlbd '/kkdpbw ll/Zkd ;f;;gz")
        ("낚시 삯 받으려 앉다가 생선 핥아먹던 삵을 밟은",
         "hfxxnd nfxq ;fAjgye jfs#ufkf nrants mfwWjfitxuts nfwxjgw ;fw3jgs")
        ("외곬은 삶이 애닳고 맘이 편찮아 시를 읊었다",
         "j/dk/wqjgs nfwzjd jrufw1k/ ifzjd pesofs1jf ndygw jgwQjtqquf")
    ]
    |> List.map (fun (actual, expected) ->
        TestCaseData(actual).Returns(expected)
            .SetName($"{nameof Sebeolsik391KeyLayout.unparse}(\"{actual}\")"))

[<TestCaseSource(nameof unparseTestParameters)>]
let unparseTest actual =
    Sebeolsik391KeyLayout.unparse actual
