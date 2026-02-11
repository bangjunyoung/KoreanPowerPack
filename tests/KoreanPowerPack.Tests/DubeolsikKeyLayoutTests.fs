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

module KoreanPowerPack.DubeolsikKeyLayoutTests

open NUnit.Framework
open KoreanPowerPack.FSharp

let parseTestParameters =
    [
        ("", "")
        ("rsefaqtdwczxvg",
         "ㄱㄴㄷㄹㅁㅂㅅㅇㅈㅊㅋㅌㅍㅎ")
        ("rreeqqttww",
         "ㄱㄱㄷㄷㅂㅂㅅㅅㅈㅈ")
        ("REQTW",
         "ㄲㄸㅃㅆㅉ")
        ("kijuhynbm l",
         "ㅏㅑㅓㅕㅗㅛㅜㅠㅡ ㅣ")
        ("hkhohlnjnpnlml",
         "ㅘㅙㅚㅝㅞㅟㅢ")
        ("dbzhogoTejs Ektwnl xhRlvnf Whcrl qkQma",
         "유쾌했던 땃쥐 토끼풀 쫓기 바쁨")
        ("skRtl tkrt qkedmfu dkswekrk todtjs gkfxdkajrejs tkfrdmf qkfqdms",
         "낚시 삯 받으려 앉다가 생선 핥아먹던 삵을 밟은")
        ("dhlrhftdms tkfadl doekfgrh akadl vuscksgdk tlfmf dmfvdjTek",
         "외곬은 삶이 애닳고 맘이 편찮아 시를 읊었다")
    ]
    |> List.map (fun (actual, expected) ->
        TestCaseData(actual).Returns(expected).SetName($"parse(\"{actual}\")"))


[<TestCaseSource(nameof parseTestParameters)>]
let parseTest actual =
    DubeolsikKeyLayout.parse actual

let unparseTestParameters =
    [
        ("", "")
        ("ㄱㄱㄷㄷㅂㅂㅅㅅㅈㅈ",
         "rreeqqttww")
        ("ㄲㄸㅃㅆㅉ",
         "REQTW")
        ("ㅘㅙㅚㅝㅞㅟㅢ",
         "hkhohlnjnpnlml")
        ("유쾌했던 땃쥐 토끼풀 쫓기 바쁨",
         "dbzhogoTejs Ektwnl xhRlvnf Whcrl qkQma")
        ("낚시 삯 받으려 앉다가 생선 핥아먹던 삵을 밟은",
         "skRtl tkrt qkedmfu dkswekrk todtjs gkfxdkajrejs tkfrdmf qkfqdms")
        ("외곬은 삶이 애닳고 맘이 편찮아 시를 읊었다",
         "dhlrhftdms tkfadl doekfgrh akadl vuscksgdk tlfmf dmfvdjTek")
    ]
    |> List.map (fun (actual, expected) ->
        TestCaseData(actual).Returns(expected).SetName($"unparse(\"{actual}\")"))

[<TestCaseSource(nameof unparseTestParameters)>]
let unparseTest actual =
    DubeolsikKeyLayout.unparse actual
