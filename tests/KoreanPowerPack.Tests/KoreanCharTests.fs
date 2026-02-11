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

module KoreanPowerPack.KoreanCharTests

open System
open NUnit.Framework

let formatChar c =
    if Char.IsControl(c) || c = '\uFFFF' then
        $"\\u{(int c):X4}"
    else
        string c

let isSyllableTestParameters =
    [
        '\u0041' (* A *), false
        '\uAC00' (* 가 *), true
        '\uD7A3' (* 힣 *), true
        '\uFFE6' (* ￦ *), false
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"isSyllable('{c}')"))

[<TestCaseSource(nameof isSyllableTestParameters)>]
let isSyllableTest c expected =
    Assert.That(KoreanChar.isSyllable c, Is.EqualTo expected)

let isChoseongTestParameters =
    [
        '\u0041' (* A *), false
        '\u1100' (* ᄀ *), true
        '\u1112' (* ᄒ *), true
        '\u3131' (* ㄱ *), false
        '\u314E' (* ㅎ *), false
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"isChoseong('{c}')"))

[<TestCaseSource(nameof isChoseongTestParameters)>]
let isChoseongTest c expected =
    Assert.That(KoreanChar.isChoseong c, Is.EqualTo expected)

let isJungseongTestParameters =
    [
        '\u0041' (* A *), false
        '\u1161' (* ᅡ *), true
        '\u1175' (* ᅵ *), true
        '\u314F' (* ㅏ *), false
        '\u3163' (* ㅣ *), false
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"isJungseong('{c}')"))

[<TestCaseSource(nameof isJungseongTestParameters)>]
let isJungseongTest c expected =
    Assert.That(KoreanChar.isJungseong c, Is.EqualTo expected)

let isJongseongTestParameters =
    [
        '\u0041' (* A *), false
        '\u11A8' (* ᆨ *), true
        '\u11C2' (* ᇂ *), true
        '\u3131' (* ㄱ *), false
        '\u314E' (* ㅎ *), false
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"isJongseong('{c}')"))

[<TestCaseSource(nameof isJongseongTestParameters)>]
let isJongseongTest c expected =
    Assert.That(KoreanChar.isJongseong c, Is.EqualTo expected)

let isCompatChoseongTestParameters =
    [
        '\u1100' (* ᄀ *), false
        '\u1112' (* ᄒ *), false
        '\u3131' (* ㄱ *), true
        '\u314E' (* ㅎ *), true
        '\uFFE6' (* ￦ *), false
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"isCompatChoseong('{c}')"))

[<TestCaseSource(nameof isCompatChoseongTestParameters)>]
let isCompatChoseongTest c expected =
    Assert.That(KoreanChar.isCompatChoseong c, Is.EqualTo expected)

let isCompatJungseongTestParameters =
    [
        '\u1161' (* ᅡ *), false
        '\u1175' (* ᅵ *), false
        '\u314F' (* ㅏ *), true
        '\u3163' (* ㅣ *), true
        '￦' (* \uFFE6 *), false
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"isCompatJungseong('{c}')"))

[<TestCaseSource(nameof isCompatJungseongTestParameters)>]
let isCompatJungseongTest c expected =
    Assert.That(KoreanChar.isCompatJungseong c, Is.EqualTo expected)

let isCompatJongseongTestParameters =
    [
        '\u11A8' (* ᆨ *), false
        '\u11C2' (* ᇂ *), false
        '\u3131' (* ㄱ *), true
        '\u314E' (* ㅎ *), true
        '\uFFE6' (* ￦ *), false
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"isCompatJongseong('{c}')"))

[<TestCaseSource(nameof isCompatJongseongTestParameters)>]
let isCompatJongseongTest c expected =
    Assert.That(KoreanChar.isCompatJongseong c, Is.EqualTo expected)

let getChoseongTestParameters =
    [
         '가' (* \uAC00 *), '\u1100' (* ᄀ *)
         '힣' (* \uD7A3 *), '\u1112' (* ᄒ *)
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"getChoseong('{c}')"))

[<TestCaseSource(nameof getChoseongTestParameters)>]
let getChoseongTest c expected =
    Assert.That(KoreanChar.getChoseong c, Is.EqualTo expected)

let getJungseongTestParameters =
    [
        '가' (* \uAC00 *), '\u1161' (* ᅡ *)
        '힣' (* \uD7A3 *), '\u1175' (* ᅵ *)
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"getJungseong('{c}')"))

[<TestCaseSource(nameof getJungseongTestParameters)>]
let getJungseongTest c expected =
    Assert.That(KoreanChar.getJungseong c, Is.EqualTo expected)

let getJongseongTestParameters =
    [
        '가' (* \uAC00 *), '\u0000'
        '각' (* \uAC01 *), '\u11A8' (* ᆨ *)
        '힣' (* \uD7A3 *), '\u11C2' (* ᇂ *)
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"getJongseong('{c}')"))

[<TestCaseSource(nameof getJongseongTestParameters)>]
let getJongseongTest c expected =
    Assert.That(KoreanChar.getJongseong c, Is.EqualTo expected)

let getCompatChoseongTestParameters =
    [
        '가' (* \uAC00 *), '\u3131' (* ㄱ *)
        '힣' (* \uD7A3 *), '\u314E' (* ㅎ *)
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"getCompatChoseong('{c}')"))

[<TestCaseSource(nameof getCompatChoseongTestParameters)>]
let getCompatChoseongTest c expected =
    Assert.That(KoreanChar.getCompatChoseong c, Is.EqualTo expected)

let getCompatJungseongTestParameters =
    [
        '가' (* \uAC00 *), '\u314F' (* ㅏ *)
        '힣' (* \uD7A3 *), '\u3163' (* ㅣ *)
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"getCompatJungseong('{c}')"))

[<TestCaseSource(nameof getCompatJungseongTestParameters)>]
let getCompatJungseongTest c expected =
    Assert.That(KoreanChar.getCompatJungseong c, Is.EqualTo expected)

let getCompatJongseongTestParameters =
    [
        '가' (* \uAC00 *), '\u0000'
        '각' (* \uAC01 *), '\u3131' (* ㄱ *)
        '힣' (* \uD7A3 *), '\u314E' (* ㅎ *)
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"getCompatJongseong('{c}')"))

[<TestCaseSource(nameof getCompatJongseongTestParameters)>]
let getCompatJongseongTest c expected =
    Assert.That(KoreanChar.getCompatJongseong c, Is.EqualTo expected)

let choseongToCompatChoseongTestParameters =
    [
        '\u1100' (* ᄀ *), '\u3131' (* ㄱ *)
        '\u1112' (* ᄒ *), '\u314E' (* ㅎ *)
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"choseongToCompatChoseong('{c}')"))

[<TestCaseSource(nameof choseongToCompatChoseongTestParameters)>]
let choseongToCompatChoseongTest c expected =
    Assert.That(KoreanChar.choseongToCompatChoseong c, Is.EqualTo expected)

let choseongToCompatChoseongExceptionTestParameters =
    [
        '\u0041' (* A *)
        '\uAC00' (* 가 *)
        '\u3131' (* ㄱ *)
        '\u314E' (* ㅎ *)
    ]
    |> List.map (fun c ->
        TestCaseData(c).SetName($"choseongToCompatChoseong('{c}') throws ArgumentException"))

[<TestCaseSource(nameof choseongToCompatChoseongExceptionTestParameters)>]
let choseongToCompatChoseongExceptionTest c =
     Assert.That(Func<_>(fun () -> KoreanChar.choseongToCompatChoseong c),
         Throws.ArgumentException)

let compatChoseongToChoseongTestParameters =
    [
        '\u3131' (* ㄱ *), '\u1100' (* ᄀ *)
        '\u314E' (* ㅎ *), '\u1112' (* ᄒ *)
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c, expected).SetName($"compatChoseongToChoseong('{c}')"))

[<TestCaseSource(nameof compatChoseongToChoseongTestParameters)>]
let compatChoseongToChoseongTest c expected =
    Assert.That(KoreanChar.compatChoseongToChoseong c, Is.EqualTo expected)

let compatChoseongToChoseongExceptionTestParameters =
    [
        '\u0041' (* A *)
        '\uAC00' (* 가 *)
        '\u1100' (* ᄀ *)
        '\u1112' (* ᄒ *)
    ]
    |> List.map (fun c ->
        TestCaseData(c).SetName($"compatChoseongToChoseong('{c}') throws ArgumentException"))

[<TestCaseSource(nameof compatChoseongToChoseongExceptionTestParameters)>]
let compatChoseongToChoseongExceptionTest c =
     Assert.That(Func<_>(fun () -> KoreanChar.compatChoseongToChoseong c), Throws.ArgumentException)

let joinJamoTestParameters =
    [
        "", '\u0000'
        "\u1101" (* ᄁ *), 'ᄁ'
        "\u1161" (* ᅡ *), 'ᅡ'
        "\u11C2" (* ᇂ *), 'ᇂ'
        "\u3132" (* ㄲ *), 'ㄲ'
        "\u314F" (* ㅏ *), 'ㅏ'
        "\u314E" (* ㅎ *), 'ㅎ'
        "\u1100\u1100" (* ᄀᄀ *), '\u1101' (* ᄁ *)
        "\u1169\u1161" (* ᅩᅡ *), '\u116A' (* ᅪ *)
        "\u3131\u3131" (* ㄱㄱ *), '\u3132' (* ㄲ *)
        "\u3157\u314F" (* ㅗㅏ *), '\u3158' (* ㅘ *)
    ]
    |> List.map (fun (jamo, expected) ->
        TestCaseData(jamo).Returns(expected).SetName($"joinJamo('{jamo}')"))

[<TestCaseSource(nameof joinJamoTestParameters)>]
let joinJamoTest jamo =
    KoreanChar.joinJamo jamo

let joinJamoExceptionTestParameters =
    [
        "\u0041" (* A *)
        "\uAC00" (* 가 *)
        "\u1100\u1161" (* 가 *)
        "\u3131\u314F" (* ㄱㅏ *)
    ]
    |> List.map (fun jamo ->
        TestCaseData(jamo).SetName($"joinJamo('{jamo}') throws ArgumentException"))

[<TestCaseSource(nameof joinJamoExceptionTestParameters)>]
let joinJamoExceptionTest jamo =
     Assert.That(Func<_>(fun () -> KoreanChar.joinJamo jamo), Throws.ArgumentException)

let splitJamoTestParameters =
    [
        '\u0000', ""
        '\u1100' (* ᄀ *), "\u1100" (* ᄀ *)
        '\u1161' (* ᅡ *), "\u1161" (* ᅡ *)
        '\u11A8' (* ᆨ *), "\u11A8" (* ᆨ *)
        '\u3131' (* ㄱ *), "\u3131" (* ㄱ *)
        '\u314F' (* ㅏ *), "\u314F" (* ㅏ *)
        '\u1101' (* ᄁ *), "\u1100\u1100" (* ᄀᄀ *)
        '\u116A' (* ᅪ *), "\u1169\u1161" (* ᅩᅡ *)
        '\u3132' (* ㄲ *), "\u3131\u3131" (* ㄱㄱ *)
        '\u3158' (* ㅘ *), "\u3157\u314F" (* ㅗㅏ *)
    ]
    |> List.map (fun (c, expected) ->
        TestCaseData(c).Returns(expected).SetName($"splitJamo('{formatChar c}')"))

[<TestCaseSource(nameof splitJamoTestParameters)>]
let splitJamoTest jamo =
     KoreanChar.splitJamo jamo

let splitJamoExceptionTestParameters =
    [
        '\u0041' (* A *)
        '\uAC00' (* 가 *)
    ]
    |> List.map (fun jamo ->
        TestCaseData(jamo).SetName($"splitJamo('{jamo}') throws ArgumentException"))

[<TestCaseSource(nameof splitJamoExceptionTestParameters)>]
let splitJamoExceptionTest jamo =
     Assert.That(Func<_>(fun () -> KoreanChar.splitJamo jamo), Throws.ArgumentException)

let composeTestParameters =
    [
        '\u1101' (* ᄁ *), '\u116A' (* ᅪ *), '\u0000'         , '꽈'
        '\u1101' (* ᄁ *), '\u116A' (* ᅪ *), '\u11A9' (* ᆩ *), '꽊'
        '\u3132' (* ㄲ *), '\u116A' (* ᅪ *), '\u11A9' (* ᆩ *), '꽊'
        '\u3132' (* ㄲ *), '\u3158' (* ㅘ *), '\u11A9' (* ᆩ *), '꽊'
        '\u3132' (* ㄲ *), '\u3158' (* ㅘ *), '\u3132' (* ㄲ *), '꽊'
    ]
    |> List.map (fun (cho, jung, jong, expected) ->
        TestCaseData(cho, jung, jong).Returns(expected).SetName($"compose('{cho}', '{jung}', '{formatChar jong}')"))

[<TestCaseSource(nameof composeTestParameters)>]
let composeTest cho jung jong =
    KoreanChar.compose cho jung jong

let composeExceptionTestParameters =
    [
        '\u10FF' (* invalid choseong *), '\u116A' (* ᅪ *), '\u11A9' (* ᆩ *)
        '\u3132' (* ㄲ *), '\u1160' (* invalid jungseong *), '\u11A9' (* ᆩ *)
        '\u3132' (* ㄲ *), '\u3158' (* ㅘ *), '\u11C3' (* invalid jongseong *)
    ]
    |> List.map (fun (cho, jung, jong) ->
        TestCaseData(cho, jung, jong).SetName($"compose('{cho}', '{jung}', '{jong}') throws ArgumentException"))

[<TestCaseSource(nameof composeExceptionTestParameters)>]
let composeExceptionTest cho jung jong =
    Assert.That(Func<_>(fun () -> KoreanChar.compose cho jung jong), Throws.ArgumentException)

let composeWithStringsTestParameters =
    [
        "\u1101" (* ᄁ *), "\u116A" (* ᅪ *), "", '꽈'
        "\u1101" (* ᄁ *), "\u116A" (* ᅪ *), "\u11A9" (* ᆩ *), '꽊'
        "\u1100\u1100" (* ᄀᄀ *), "\u1169\u1161" (* ᅩᅡ *), "\u11A8\u11A8" (* ᆨᆨ *), '꽊'
    ]
    |> List.map (fun (cho, jung, jong, expected) ->
        TestCaseData(cho, jung, jong).Returns(expected).SetName($"composeWithStrings('{cho}', '{jung}', '{jong}')"))

[<TestCaseSource(nameof composeWithStringsTestParameters)>]
let composeWithStringsTest cho jung jong =
    KoreanChar.composeWithStrings cho jung jong

let composeWithStringsExceptionTestParameters =
    [
        "\u10FF" (* invalid choseong *), "\u116A" (* ᅪ *), "\u11A9" (* ᆩ *)
        "\u3132" (* ㄲ *), "\u1160" (* invalid jungseong *), "\u11A9" (* ᆩ *)
        "\u3132" (* ㄲ *), "\u3158" (* ㅘ *), "\u11C3" (* invalid jongseong *)
    ]
    |> List.map (fun (cho, jung, jong) ->
        TestCaseData(cho, jung, jong)
            .SetName($"composeWithStrings('{cho}', '{jung}', '{jong}') throws ArgumentException"))

[<TestCaseSource(nameof composeWithStringsExceptionTestParameters)>]
let composeWithStringExceptionTest cho jung jong =
    Assert.That(Func<_>(fun () -> KoreanChar.composeWithStrings cho jung jong), Throws.ArgumentException)

let decomposeTestParameters =
    [
        '가' (* \uAC00 *), [|"ᄀ"; "ᅡ"|]
        '힣' (* \uD7A3 *), [|"ᄒ"; "ᅵ"; "ᇂ"|]
        '쐈' (* \uC370 *), [|"ᄉᄉ"; "ᅩᅡ"; "ᆺᆺ"|]
        '뛇' (* \uB5C7 *), [|"ᄃᄃ"; "ᅮᅥ"; "ᆯᇂ"|]
    ]
    |> List.map (fun (syllable, expected) ->
        TestCaseData(syllable).Returns(expected).SetName($"decompose('{syllable}')"))

[<TestCaseSource(nameof decomposeTestParameters)>]
let decomposeTest syllable =
    KoreanChar.decompose syllable

let decomposeExceptionTestParameters =
    [
        '\u0041' (* A *)
        '\u1100' (* ᄀ *)
        '\u3131' (* ㄱ *)
    ]
    |> List.map (fun syllable ->
        TestCaseData(syllable).SetName($"decompose('{syllable}') throws ArgumentException"))

[<TestCaseSource(nameof decomposeExceptionTestParameters)>]
let decomposeExceptionTest syllable =
    Assert.That(Func<_>(fun () -> KoreanChar.decompose syllable), Throws.ArgumentException)

let decomposeCompatTestParametersTestParameters =
    [
        '가' (* \uAC00 *), [|"ㄱ"; "ㅏ"|]
        '뛇' (* \uB5C7 *), [|"ㄷㄷ"; "ㅜㅓ"; "ㄹㅎ"|]
        '쐈' (* \uC370 *), [|"ㅅㅅ"; "ㅗㅏ"; "ㅅㅅ"|]
        '힣' (* \uD7A3 *), [|"ㅎ"; "ㅣ"; "ㅎ"|]
    ]
    |>List.map (fun (syllable, expected) ->
        TestCaseData(syllable).Returns(expected).SetName($"decomposeCompat('{syllable}')"))

[<TestCaseSource(nameof decomposeCompatTestParametersTestParameters)>]
let decomposeCompatTest syllable =
    KoreanChar.decomposeCompat syllable

let decomposeCompatExceptionTestParameters =
    [
        '\u0041' (* A *)
        '\u1100' (* ᄀ *)
        '\u3131' (* ㄱ *)
    ]
    |> List.map (fun syllable ->
        TestCaseData(syllable).SetName($"decomposeCompat('{syllable}') throws ArgumentException"))

[<TestCaseSource(nameof decomposeCompatExceptionTestParameters)>]
let decomposeCompatExceptionTest syllable =
    Assert.That(Func<_>(fun () -> KoreanChar.decomposeCompat syllable), Throws.ArgumentException)

let decomposeDubeolsikTestParameters =
    [
        '가' (* \uAC00 *), [|"ㄱ"; "ㅏ"|]
        '뛇' (* \uB5C7 *), [|"ㄸ"; "ㅜㅓ"; "ㄹㅎ"|]
        '쐈' (* \uC370 *), [|"ㅆ"; "ㅗㅏ"; "ㅆ"|]
        '힣' (* \uD7A3 *), [|"ㅎ"; "ㅣ"; "ㅎ"|]
    ]
    |>List.map (fun (syllable, expected) ->
        TestCaseData(syllable).Returns(expected).SetName($"decomposeDubeolsik('{syllable}')"))

[<TestCaseSource(nameof decomposeDubeolsikTestParameters)>]
let decomposeDubeolsikTest syllable =
    KoreanChar.decomposeDubeolsik syllable

let decomposeDubeolsikExceptionTestParameters =
    [
        '\u0041' (* A *)
        '\u1100' (* ᄀ *)
        '\u3131' (* ㄱ *)
    ]
    |> List.map (fun syllable ->
        TestCaseData(syllable).SetName($"decomposeDubeolsik('{syllable}') throws ArgumentException"))

[<TestCaseSource(nameof decomposeDubeolsikExceptionTestParameters)>]
let decomposeDubeolsikExceptionTest syllable =
    Assert.That(Func<_>(fun () -> KoreanChar.decomposeDubeolsik syllable), Throws.ArgumentException)
