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

module KoreanPowerPack.KoreanTextMatcherTest

open NUnit.Framework

let IsMatch_TestParameters =
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

[<TestCaseSource("IsMatch_TestParameters")>]
let ``static IsMatch(text, pattern) with valid arguments`` text pattern =
    KoreanTextMatcher.IsMatch(text, pattern)
