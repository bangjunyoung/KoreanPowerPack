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

let staticIsMatchTestParameters =
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
        TestCaseData(text, pattern).Returns(expected)
            .SetName($"static {nameof KoreanTextMatcher.IsMatch}(\"{text}\", \"{pattern}\")"))

[<TestCaseSource(nameof staticIsMatchTestParameters)>]
let staticIsMatchTest text pattern =
    KoreanTextMatcher.IsMatch(text, pattern)

let staticMatchesTestParameters =
    [
        "가나다", "", 4
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
        TestCaseData(text, pattern, expected)
            .SetName($"static {nameof KoreanTextMatcher.Matches}(\"{text}\", \"{pattern}\")"))

[<TestCaseSource(nameof staticMatchesTestParameters)>]
let staticMatchesTest text pattern (expected: int) =
    let matches = KoreanTextMatcher.Matches(text, pattern)
    for ``match`` in matches do
        Assert.That(text, Does.Contain(``match``.Value.ToString()))

    Assert.That(Seq.length matches, Is.EqualTo expected);

let staticMatchTestParameters =
    [
        "하늘", "", true, 0, 0
        "하늘", "^", true, 0, 0
        "하늘", "$", true, 2, 0
        "하늘", "^$", false, 0, 0
        "하늘", "ㅎ", true, 0, 1
        "하늘", "^ㅎ", true, 0, 1
        "하늘", "ㅎ$", false, 0, 0
        "하늘", "ㄴ$", true, 1, 1
        "하늘", "^ㅎㄴ$", true, 0, 2
        "하늘", "^ㅎ니$", false, 0, 0
    ]
    |> List.map (fun (text, pattern, expectedSuccess, expectedIndex, expectedLength) ->
        TestCaseData(text, pattern, expectedSuccess, expectedIndex, expectedLength)
            .SetName($"static {nameof KoreanTextMatcher.Match}(\"{text}\", \"{pattern}\")"))

[<TestCaseSource(nameof staticMatchTestParameters)>]
let staticMatchTest text pattern expectedSuccess expectedIndex expectedLength =
    let ``match`` = KoreanTextMatcher.Match(text, pattern)
    Assert.That(``match``.Success, Is.EqualTo expectedSuccess)
    if expectedSuccess then
        Assert.That(``match``.Index, Is.EqualTo expectedIndex)
        Assert.That(``match``.Length, Is.EqualTo expectedLength)
