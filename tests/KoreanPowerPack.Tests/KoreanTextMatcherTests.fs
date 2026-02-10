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

module KoreanPowerPack.KoreanTextMatcherTests

open NUnit.Framework

let IsMatchTestParameters =
    [
        "", "", true
        "", "^$", true
        "하늘", "", true
        "하늘", "^", true
        "하늘", "$", true
        "하늘", "하늘", true
        " 하늘", "하늘", true
        "하늘 ", "하늘", true
        " 하늘 ", "하늘", true
        "하늘", "^하늘", true
        "하늘 ", "^하늘", true
        "하늘", "하늘$", true
        " 하늘", "하늘$", true
        "하늘", "^하늘$", true
        "하늘", "하ㄴ", true
        "하늘", "^하ㄴ", true
        "하늘", "하ㄴ$", true
        "하늘", "^하ㄴ$", true
        "하늘", "ㅎ늘", true
        "하늘", "^ㅎ늘", true
        "하늘", "ㅎ늘$", true
        "하늘", "^ㅎ늘$", true
        "하늘", "ㅎㄴ", true
        "하늘 ", "ㅎㄴ", true
        " 하늘", "ㅎㄴ", true
        " 하늘 ", "ㅎㄴ", true
        "하늘", "^ㅎㄴ", true
        "하늘", "ㅎㄴ$", true
        "하늘", "^ㅎㄴ$", true
        "하늘", "ㅎ느", true
        "하늘", "^ㅎ느", true
        "하늘", "ㅎ느$", true
        "하늘", "^ㅎ느$", true
        "하늘", "^$", false
        "하늘", "ㅎㄴㅎㄴ", false
        "하늘", "한", false
        "하 늘", "하늘", false
        " 하 늘", "하늘", false
        "하 늘 ", "하늘", false
        " 하 늘 ", "하늘", false
        "하늘", "하를", false
        " 하늘", "^하늘", false
        " 하늘 ", "^하늘", false
        "하늘 ", "하늘$", false
        " 하늘 ", "하늘$", false
        " 하늘", "^하늘$", false
        "하늘 ", "^하늘$", false
    ]
    |> List.map (fun (text, pattern, expected) ->
        TestCaseData(text, pattern).Returns(expected))

[<TestCaseSource("IsMatchTestParameters")>]
let ``static IsMatch(text, pattern) with valid arguments`` text pattern =
    KoreanTextMatcher.IsMatch(text, pattern)

let MatchesTestParameters =
    [
        // Hangul Compatibility Jamo
        "하늘 ㅎ늘 하느 ㅎㄴ", "ㅎㄹ", 0
        "하늘 ㅎ늘 하느 ㅎㄴ", "하늘", 1
        "하늘 ㅎ늘 하느 ㅎㄴ", "ㅎ늘", 2
        "하늘 ㅎ늘 하느 ㅎㄴ", "ㅎ느", 3
        "하늘 ㅎ늘 하느 ㅎㄴ", "ㅎㄴ", 4
        // Hangul Jamo
        "하늘 ᄒ늘 하느 ᄒᄂ", "ᄒᄅ", 0
        "하늘 ᄒ늘 하느 ᄒᄂ", "하늘", 1
        "하늘 ᄒ늘 하느 ᄒᄂ", "ᄒ늘", 2
        "하늘 ᄒ늘 하느 ᄒᄂ", "ᄒ느", 3
        "하늘 ᄒ늘 하느 ᄒᄂ", "ᄒᄂ", 4
    ]
    |> List.map (fun (text, pattern, expected) ->
        TestCaseData(text, pattern, expected))

[<TestCaseSource("MatchesTestParameters")>]
let ``static Matches(text, pattern) with valid arguments`` text pattern (expected: int) =
    let matches = KoreanTextMatcher.Matches(text, pattern)
    for ``match`` in matches do
        Assert.That(text, Does.Contain(``match``.Value.ToString()))

    Assert.That(Seq.length matches, Is.EqualTo expected);

let MatchTestParameters =
    [
        "", "", 0, true, 0, 0
        "0", "", 0, true, 0, 0
        "012", "01", 0, true, 0, 2
        "012", "12", 0, true, 1, 2
        "012", "12", 1, true, 1, 2
        "012", "01", 1, false, 0, 0
        "012", "012", 1, false, 0, 0
        "012", "0123", 0, false, 0, 0
    ]
    |> List.map (fun (text, pattern, startIndex, success, index, length) ->
        TestCaseData(text, pattern, startIndex, success, index, length))

[<TestCaseSource("MatchTestParameters")>]
let ``Match(pattern, startIndex) with valid arguments`` text pattern startIndex success index length =
    let ``match`` = KoreanTextMatcher(pattern).Match(text, startIndex)
    Assert.That(``match``.Success, Is.EqualTo success)
    if success then
        Assert.That(``match``.Index, Is.EqualTo index)
        Assert.That(``match``.Length, Is.EqualTo length)
