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

[<TestCase('\uAC00', ExpectedResult = true)>]
[<TestCase('\uD7A3', ExpectedResult = true)>]
[<TestCase('\uABFF', ExpectedResult = false)>]
[<TestCase('\uD7A4', ExpectedResult = false)>]
let ``IsSyllable returns expected result`` c =
    KoreanChar.IsSyllable c

[<TestCase('\u1100', ExpectedResult = true)>]
[<TestCase('\u1112', ExpectedResult = true)>]
[<TestCase('\u1009', ExpectedResult = false)>]
[<TestCase('\u1113', ExpectedResult = false)>]
[<TestCase('\u3131', ExpectedResult = false)>]
let ``IsChoseong returns expected result`` c =
    KoreanChar.IsChoseong c

[<TestCase('\u1161', ExpectedResult = true)>]
[<TestCase('\u1175', ExpectedResult = true)>]
[<TestCase('\u1160', ExpectedResult = false)>]
[<TestCase('\u1176', ExpectedResult = false)>]
[<TestCase('\u314F', ExpectedResult = false)>]
let ``IsJungseong returns expected result`` c =
    KoreanChar.IsJungseong c

[<TestCase('\u11A8', ExpectedResult = true)>]
[<TestCase('\u11C2', ExpectedResult = true)>]
[<TestCase('\u11A7', ExpectedResult = false)>]
[<TestCase('\u11C3', ExpectedResult = false)>]
[<TestCase('\u3131', ExpectedResult = false)>]
let ``IsJongseong returns expected result`` c =
    KoreanChar.IsJongseong c

[<TestCase('\u3131', ExpectedResult = true)>]
[<TestCase('\u314E', ExpectedResult = true)>]
[<TestCase('\u3130', ExpectedResult = false)>]
[<TestCase('\u314F', ExpectedResult = false)>]
[<TestCase('\u3133', ExpectedResult = false)>]
let ``IsCompatChoseong returns expected result`` c =
    KoreanChar.IsCompatChoseong c

[<TestCase('\u314F', ExpectedResult = true)>]
[<TestCase('\u3163', ExpectedResult = true)>]
[<TestCase('\u314E', ExpectedResult = false)>]
[<TestCase('\u3164', ExpectedResult = false)>]
let ``IsCompatJungseong returns expected result`` c =
    KoreanChar.IsCompatJungseong c

[<TestCase('\u3131', ExpectedResult = true)>]
[<TestCase('\u314E', ExpectedResult = true)>]
[<TestCase('\u3138', ExpectedResult = false)>]
[<TestCase('\u3143', ExpectedResult = false)>]
[<TestCase('\u3149', ExpectedResult = false)>]
let ``IsCompatJongseong returns expected result`` c =
    KoreanChar.IsCompatJongseong c

[<TestCase('\u1101', '\u116A', '\u11A9', ExpectedResult = '꽊')>]
let ``Compose with chars returns expected result`` (cho: char) jung jong =
    KoreanChar.Compose(cho, jung, jong)

[<TestCase("\u1101", "\u116A", "\u11A9", ExpectedResult = '꽊')>]
[<TestCase("\u1100\u1100", "\u1169\u1161", "\u11A8\u11A8", ExpectedResult = '꽊')>]
let ``Compose with strings returns expected result`` (cho: string) jung jong =
    KoreanChar.Compose(cho, jung, jong)
