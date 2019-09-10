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
let ``isSyllable with valid arguments`` c =
    KoreanChar.isSyllable c

[<TestCase('\u1100', ExpectedResult = true)>]
[<TestCase('\u1112', ExpectedResult = true)>]
[<TestCase('\u1009', ExpectedResult = false)>]
[<TestCase('\u1113', ExpectedResult = false)>]
[<TestCase('\u3131', ExpectedResult = false)>]
let ``isChoseong with valid arguments`` c =
    KoreanChar.isChoseong c

[<TestCase('\u1161', ExpectedResult = true)>]
[<TestCase('\u1175', ExpectedResult = true)>]
[<TestCase('\u1160', ExpectedResult = false)>]
[<TestCase('\u1176', ExpectedResult = false)>]
[<TestCase('\u314F', ExpectedResult = false)>]
let ``isJungseong with valid arguments`` c =
    KoreanChar.isJungseong c

[<TestCase('\u11A8', ExpectedResult = true)>]
[<TestCase('\u11C2', ExpectedResult = true)>]
[<TestCase('\u11A7', ExpectedResult = false)>]
[<TestCase('\u11C3', ExpectedResult = false)>]
[<TestCase('\u3131', ExpectedResult = false)>]
let ``isJongseong with valid arguments`` c =
    KoreanChar.isJongseong c

[<TestCase('\u3131', ExpectedResult = true)>]
[<TestCase('\u314E', ExpectedResult = true)>]
[<TestCase('\u3130', ExpectedResult = false)>]
[<TestCase('\u314F', ExpectedResult = false)>]
[<TestCase('\u3133', ExpectedResult = false)>]
let ``isCompatChoseong with valid arguments`` c =
    KoreanChar.isCompatChoseong c

[<TestCase('\u314F', ExpectedResult = true)>]
[<TestCase('\u3163', ExpectedResult = true)>]
[<TestCase('\u314E', ExpectedResult = false)>]
[<TestCase('\u3164', ExpectedResult = false)>]
let ``isCompatJungseong with valid arguments`` c =
    KoreanChar.isCompatJungseong c

[<TestCase('\u3131', ExpectedResult = true)>]
[<TestCase('\u314E', ExpectedResult = true)>]
[<TestCase('\u3138', ExpectedResult = false)>]
[<TestCase('\u3143', ExpectedResult = false)>]
[<TestCase('\u3149', ExpectedResult = false)>]
let ``isCompatJongseong with valid arguments`` c =
    KoreanChar.isCompatJongseong c

[<TestCase('\u1101', '\u116A', '\u11A9', ExpectedResult = '꽊')>]
let ``compose with valid arguments`` (cho: char) jung jong =
    KoreanChar.compose cho jung jong

[<TestCase("\u1101", "\u116A", "\u11A9", ExpectedResult = '꽊')>]
[<TestCase("\u1100\u1100", "\u1169\u1161", "\u11A8\u11A8", ExpectedResult = '꽊')>]
let ``composeWithStrings with valid arguments`` (cho: string) jung jong =
    KoreanChar.composeWithStrings cho jung jong
