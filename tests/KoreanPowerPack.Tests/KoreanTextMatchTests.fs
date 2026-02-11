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

module KoreanPowerPack.KoreanTextMatchTests

open NUnit.Framework

[<Test>]
let ``KoreanTextMatch works as expected`` () =
    let m = KoreanTextMatch(KoreanTextMatcher(""), "0123456789", 1, 5)
    Assert.That(m.Index, Is.EqualTo 1)
    Assert.That(m.Length, Is.EqualTo 5)
    Assert.That(m.Success)
    Assert.That(m.Value.ToString(), Is.EqualTo "12345")

[<Test>]
let ``KoreanTextMatch․Empty․Success always returns false`` () =
    Assert.That(KoreanTextMatch.Empty.Success, Is.False)

[<Test>]
let ``KoreanTextMatch․Empty․NextMatch() always returns KoreanTextMatch․Empty`` () =
    Assert.That(KoreanTextMatch.Empty.NextMatch(), Is.EqualTo KoreanTextMatch.Empty)
