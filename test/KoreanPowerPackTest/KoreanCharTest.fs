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

[<TestCase('가')>]
[<TestCase('힣')>]
let ``isSyllable returns true for syllable`` c =
    Assert.That(isSyllable c)

[<TestCase('\uABFF')>]
[<TestCase('\uD7A4')>]
let ``isSyllable returns false for non-syllable`` c =
    Assert.That(isSyllable c, Is.False)

[<TestCase('\u1100')>]
[<TestCase('\u1112')>]
let ``isChoseong returns true for choseong`` c =
    Assert.That(isChoseong c)

[<TestCase('\u1009')>]
[<TestCase('\u1113')>]
[<TestCase('\u3131')>]
let ``isChoseong returns false for non-choseong`` c =
    Assert.That(isChoseong c, Is.False)

[<TestCase('\u1161')>]
[<TestCase('\u1175')>]
let ``isJungseong returns true for jungseong`` c =
    Assert.That(isJungseong c)

[<TestCase('\u1160')>]
[<TestCase('\u1176')>]
[<TestCase('\u314F')>]
let ``isJungseong returns false for non-jungseong`` c =
    Assert.That(isJungseong c, Is.False)

[<TestCase('\u11A8')>]
[<TestCase('\u11C2')>]
let ``isJongseong returns true for jongseong`` c =
    Assert.That(isJongseong c)

[<TestCase('\u11A7')>]
[<TestCase('\u11C3')>]
[<TestCase('\u3131')>]
let ``isJongseong returns false for non-jongseong`` c =
    Assert.That(isJongseong c, Is.False)

[<TestCase('\u3131')>]
[<TestCase('\u314E')>]
let ``isCompatChoseong returns true for Compat choseong`` c =
    Assert.That(isCompatChoseong c)

[<TestCase('\u3130')>]
[<TestCase('\u314F')>]
[<TestCase('\u3133')>]
let ``isCompatChoseong returns false for non-Compat choseong`` c =
    Assert.That(isCompatChoseong c, Is.False)

[<TestCase('\u314F')>]
[<TestCase('\u3163')>]
let ``isCompatJungseong returns true for Compat jungseong`` c =
    Assert.That(isCompatJungseong c)

[<TestCase('\u314E')>]
[<TestCase('\u3164')>]
let ``isCompatJungseong returns false for non-Compat jungseong`` c =
    Assert.That(isCompatJungseong c, Is.False)

[<TestCase('\u3131')>]
[<TestCase('\u314E')>]
let ``isCompatJongseong returns true for Compat jongseong`` c =
    Assert.That(isCompatJongseong c)

[<TestCase('\u3138')>]
[<TestCase('\u3143')>]
[<TestCase('\u3149')>]
let ``isCompatJongseong returns false for non-Compat jongseong`` c =
    Assert.That(isCompatJongseong c, Is.False)

[<TestCase('\u1100', ExpectedResult = '\u3131')>]
[<TestCase('\u1112', ExpectedResult = '\u314E')>]
let ``choseongToCompatChoseong returns Compat choseong for choseong`` c =
    choseongToCompatChoseong c

[<TestCase('A')>]
let ``choseongToCompatChoseong throws ArgumentException for non-choseong`` c =
     Assert.That(System.Func<_>(fun () -> choseongToCompatChoseong c),
         Throws.ArgumentException)

[<TestCase('\u3131', ExpectedResult = '\u1100')>]
[<TestCase('\u314E', ExpectedResult = '\u1112')>]
let ``compatChoseongToChoseong returns choseong for Compat choseong`` c =
    compatChoseongToChoseong c

[<TestCase('ㄳ')>]
let ``compatChoseongToChoseong throws ArgumentException for non-Compat choseong`` c =
     Assert.That(System.Func<_>(fun () -> compatChoseongToChoseong c),
         Throws.ArgumentException)

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
let ``joinJamo with valid argument`` jamo =
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
let ``splitJamo with valid argument`` jamo =
     splitJamo jamo

[<TestCase('A')>]
[<TestCase('가')>]
let ``splitJamo throws ArgumentException for invalid argument`` jamo =
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
let ``compose throws ArgumentException for invalid arguments`` cho jung jong =
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
let ``composeWithString throws ArgumentException for invalid arguments`` cho jung jong =
    Assert.That(System.Func<_>(fun () -> composeWithStrings cho jung jong),
        Throws.ArgumentException)

[<TestCase('가', ExpectedResult = [|"ᄀ"; "ᅡ"|])>]
[<TestCase('힣', ExpectedResult = [|"ᄒ"; "ᅵ"; "ᇂ"|])>]
[<TestCase('쐈', ExpectedResult = [|"ᄉᄉ"; "ᅩᅡ"; "ᆺᆺ"|])>]
[<TestCase('뛇', ExpectedResult = [|"ᄃᄃ"; "ᅮᅥ"; "ᆯᇂ"|])>]
let ``decompose with valid argument`` syllable =
    decompose syllable

[<TestCase('A')>]
let ``decompose throws ArgumentException for invalid argument`` syllable =
    Assert.That(System.Func<_>(fun () -> decompose syllable),
        Throws.ArgumentException)

[<TestCase('가', ExpectedResult = [|"ㄱ"; "ㅏ"|])>]
[<TestCase('힣', ExpectedResult = [|"ㅎ"; "ㅣ"; "ㅎ"|])>]
[<TestCase('쐈', ExpectedResult = [|"ㅅㅅ"; "ㅗㅏ"; "ㅅㅅ"|])>]
[<TestCase('뛇', ExpectedResult = [|"ㄷㄷ"; "ㅜㅓ"; "ㄹㅎ"|])>]
let ``decomposeCompat with valid argument`` syllable =
    decomposeCompat syllable

[<TestCase('A')>]
let ``decomposeCompat throws ArgumentException for invalid argument`` syllable =
    Assert.That(System.Func<_>(fun () -> decomposeCompat syllable),
        Throws.ArgumentException)

[<TestCase('가', ExpectedResult = [|"ㄱ"; "ㅏ"|])>]
[<TestCase('힣', ExpectedResult = [|"ㅎ"; "ㅣ"; "ㅎ"|])>]
[<TestCase('쐈', ExpectedResult = [|"ㅆ"; "ㅗㅏ"; "ㅆ"|])>]
[<TestCase('뛇', ExpectedResult = [|"ㄸ"; "ㅜㅓ"; "ㄹㅎ"|])>]
let ``decomposeDubeolsik with valid argument`` syllable =
    decomposeDubeolsik syllable

[<TestCase('A')>]
let ``decomposeDubeolsik throws ArgumentException for invalid argument`` syllable =
    Assert.That(System.Func<_>(fun () -> decomposeDubeolsik syllable),
        Throws.ArgumentException)
