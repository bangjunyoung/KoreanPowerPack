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

module KoreanPowerPack.KoreanCharTest

open NUnit.Framework
open KoreanChar

[<TestCase('\uAC00')>]
[<TestCase('\uD7A3')>]
let ``isSyllable with valid arguments`` c =
    Assert.That(isSyllable c)

[<TestCase('\uABFF')>]
[<TestCase('\uD7A4')>]
let ``isSyllable with invalid arguments`` c =
    Assert.That(isSyllable c, Is.False)

[<TestCase('\u1100')>]
[<TestCase('\u1112')>]
let ``isChoseong with valid arguments`` c =
    Assert.That(isChoseong c)

[<TestCase('\u1009')>]
[<TestCase('\u1113')>]
[<TestCase('\u3131')>]
let ``isChoseong with invalid arguments`` c =
    Assert.That(isChoseong c, Is.False)

[<TestCase('\u1161')>]
[<TestCase('\u1175')>]
let ``isJungseong with valid arguments`` c =
    Assert.That(isJungseong c)

[<TestCase('\u1160')>]
[<TestCase('\u1176')>]
[<TestCase('\u314F')>]
let ``isJungseong with invalid arguments`` c =
    Assert.That(isJungseong c, Is.False)

[<TestCase('\u11A8')>]
[<TestCase('\u11C2')>]
let ``isJongseong with valid arguments`` c =
    Assert.That(isJongseong c)

[<TestCase('\u11A7')>]
[<TestCase('\u11C3')>]
[<TestCase('\u3131')>]
let ``isJongseong with invalid arguments`` c =
    Assert.That(isJongseong c, Is.False)

[<TestCase('\u3131')>]
[<TestCase('\u314E')>]
let ``isCompatChoseong with valid arguments`` c =
    Assert.That(isCompatChoseong c)

[<TestCase('\u3130')>]
[<TestCase('\u314F')>]
[<TestCase('\u3133')>]
let ``isCompatChoseong with invalid arguments`` c =
    Assert.That(isCompatChoseong c, Is.False)

[<TestCase('\u314F')>]
[<TestCase('\u3163')>]
let ``isCompatJungseong with valid arguments`` c =
    Assert.That(isCompatJungseong c)

[<TestCase('\u314E')>]
[<TestCase('\u3164')>]
let ``isCompatJungseong with invalid arguments`` c =
    Assert.That(isCompatJungseong c, Is.False)

[<TestCase('\u3131')>]
[<TestCase('\u314E')>]
let ``isCompatJongseong with valid arguments`` c =
    Assert.That(isCompatJongseong c)

[<TestCase('\u3138')>]
[<TestCase('\u3143')>]
[<TestCase('\u3149')>]
let ``isCompatJongseong with invalid arguments`` c =
    Assert.That(isCompatJongseong c, Is.False)

[<TestCase("", ExpectedResult = '\u0000')>]
[<TestCase("\u1101", ExpectedResult = 'ᄁ')>]
[<TestCase("\u1161", ExpectedResult = 'ᅡ')>]
[<TestCase("\u11C2", ExpectedResult = 'ᇂ')>]
[<TestCase("\u3132", ExpectedResult = 'ㄲ')>]
[<TestCase("\u314F", ExpectedResult = 'ㅏ')>]
[<TestCase("\u314E", ExpectedResult = 'ㅎ')>]
[<TestCase("\u1100\u1100", ExpectedResult = 'ᄁ')>]
[<TestCase("\u1169\u1161", ExpectedResult = 'ᅪ')>]
[<TestCase("\u3131\u3131", ExpectedResult = 'ㄲ')>]
[<TestCase("\u3157\u314F", ExpectedResult = 'ㅘ')>]
let ``joinJamo with valid arguments`` jamo =
    joinJamo jamo

[<TestCase('\u0000', ExpectedResult = "")>]
[<TestCase('\u1100', ExpectedResult = "ᄀ")>]
[<TestCase('\u1161', ExpectedResult = "ᅡ")>]
[<TestCase('\u11C2', ExpectedResult = "ᇂ")>]
[<TestCase('\u3131', ExpectedResult = "ㄱ")>]
[<TestCase('\u314F', ExpectedResult = "ㅏ")>]
[<TestCase('ᄁ', ExpectedResult = "\u1100\u1100")>]
[<TestCase('ᅪ', ExpectedResult = "\u1169\u1161")>]
[<TestCase('ㄳ', ExpectedResult = "\u3131\u3145")>]
[<TestCase('ㅘ', ExpectedResult = "\u3157\u314F")>]
let ``splitJamo with valid arguments`` jamo =
     splitJamo jamo

[<TestCase('A')>]
[<TestCase('가')>]
let ``splitJamo with invalid arguments throws ArgumentException`` jamo =
     Assert.That(System.Func<_>(fun () -> splitJamo jamo),
        Throws.ArgumentException)

[<TestCase('\u1101', '\u116A', '\u0000', ExpectedResult = '꽈')>]
[<TestCase('\u1101', '\u116A', '\u11A9', ExpectedResult = '꽊')>]
[<TestCase('\u3132', '\u116A', '\u11A9', ExpectedResult = '꽊')>]
[<TestCase('\u3132', '\u3158', '\u11A9', ExpectedResult = '꽊')>]
[<TestCase('\u3132', '\u3158', '\u3132', ExpectedResult = '꽊')>]
let ``compose with valid arguments`` cho jung jong =
    compose cho jung jong

[<TestCase('\u10FF', '\u116A', '\u11A9')>]
[<TestCase('\u3132', '\u1160', '\u11A9')>]
[<TestCase('\u3132', '\u3158', '\u11C3')>]
let ``compose with invalid arguments throws ArgumentException`` cho jung jong =
    Assert.That(System.Func<_>(fun () -> compose cho jung jong),
        Throws.ArgumentException)

[<TestCase("\u1101", "\u116A", "", ExpectedResult = '꽈')>]
[<TestCase("\u1101", "\u116A", "\u11A9", ExpectedResult = '꽊')>]
[<TestCase("\u1100\u1100", "\u1169\u1161", "\u11A8\u11A8", ExpectedResult = '꽊')>]
let ``composeWithStrings with valid arguments`` cho jung jong =
    composeWithStrings cho jung jong

[<TestCase("\u10FF", "\u116A", "\u11A9")>]
[<TestCase("\u3132", "\u1160", "\u11A9")>]
[<TestCase("\u3132", "\u3158", "\u11C3")>]
let ``composeWithString with invalid arguments throws ArgumentException`` cho jung jong =
    Assert.That(System.Func<_>(fun () -> composeWithStrings cho jung jong),
        Throws.ArgumentException)

[<TestCase('가', ExpectedResult = [|"ᄀ"; "ᅡ"|])>]
[<TestCase('힣', ExpectedResult = [|"ᄒ"; "ᅵ"; "ᇂ"|])>]
[<TestCase('쐈', ExpectedResult = [|"ᄉᄉ"; "ᅩᅡ"; "ᆺᆺ"|])>]
[<TestCase('뛇', ExpectedResult = [|"ᄃᄃ"; "ᅮᅥ"; "ᆯᇂ"|])>]
let ``decompose with valid arguments`` syllable =
    decompose syllable

[<TestCase('가', ExpectedResult = [|"ㄱ"; "ㅏ"|])>]
[<TestCase('힣', ExpectedResult = [|"ㅎ"; "ㅣ"; "ㅎ"|])>]
[<TestCase('쐈', ExpectedResult = [|"ㅆ"; "ㅗㅏ"; "ㅆ"|])>]
[<TestCase('뛇', ExpectedResult = [|"ㄸ"; "ㅜㅓ"; "ㄹㅎ"|])>]
let ``decomposeCompat with valid arguments`` syllable =
    decomposeCompat syllable
