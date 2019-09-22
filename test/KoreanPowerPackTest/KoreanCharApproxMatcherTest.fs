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

module KoreanPowerPack.KoreanCharApproxMatcherTest

open NUnit.Framework

[<TestCase('h', 'h')>]
[<TestCase('\u3131', '\u1100')>]
[<TestCase('\u314E', '\u1112')>]
[<TestCase('\u1100', '\u3131')>]
[<TestCase('\u1112', '\u314E')>]
[<TestCase('또', 'ㄷ')>]
[<TestCase('또', 'ㄸ')>]
[<TestCase('광', '고')>]
[<TestCase('광', '과')>]
[<TestCase('밝', '발')>]
[<TestCase('밝', '밝')>]
[<TestCase('꽜', 'ㄱ')>]
[<TestCase('꽜', 'ㄲ')>]
[<TestCase('꽜', '꼬')>]
[<TestCase('꽜', '꽈')>]
[<TestCase('꽜', '꽛')>]
[<TestCase('꽜', '꽜')>]
let ``isMatch with matched arguments`` t p =
    Assert.That(KoreanCharApproxMatcher.isMatch t p)

[<TestCase('H', 'h')>]
[<TestCase('하', '한')>]
[<TestCase('한', 'ㅏ')>]
[<TestCase('한', '핞')>]
let ``isMatch with unmatched arguments`` t p =
    Assert.That(KoreanCharApproxMatcher.isMatch t p, Is.False)
