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

module KoreanPowerPack.KoreanCharApproxMatcherTests

open NUnit.Framework

let isMatchTestParameters =
    [
        'h', 'h', true
        'H', 'h', false
        '\u1100' (* ᄀ *), '\u3131' (* ㄱ *), true
        '\u1112' (* ᄒ *), '\u314E' (* ㅎ *), true
        '\u3131' (* ㄱ *), '\u1100' (* ᄀ *), true
        '\u314E' (* ㅎ *), '\u1112' (* ᄒ *), true
        '꽜', '\u1100' (* ᄀ *), true
        '꽜', '\u3131' (* ㄱ *), true
        '꽜', 'ㄲ', true
        '꽜', '꼬', true
        '꽜', '꽈', true
        '꽜', '꽛', true
        '꽜', '꽜', true
        '하', '한', false
        '한', 'ㅏ', false
        '한', '핞', false
    ]
    |> List.map (fun (text, pattern, expected) ->
        TestCaseData(text, pattern).Returns(expected).SetName($"isMatch('{text}', '{pattern}')"))

[<TestCaseSource(nameof isMatchTestParameters)>]
let isMatchTest text pattern =
    KoreanCharApproxMatcher.isMatch text pattern
