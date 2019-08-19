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

module KoreanPowerPack.Sebeolsik391KeyLayoutTest

open NUnit.Framework

[<TestCase("0dngjgd k/j5 l/ktsjgs jd3nbwkkdyd ifshfj6 u/dk/ 'gx;ewmfs j4yeajgs pdwj4od jfSuf",
    ExpectedResult = "키스의 고유 조건은 입술끼리 만나야 되고 특별한 요령은 필요치 않다")>]
[<TestCase("0dngjgd k/j5 l/ktsjgs jd3nbwkkdyd ifshfj6 u/dk/ 'gx;ewmfs j4yeajgs pdwj4od jfs1uf",
    ExpectedResult = "키스의 고유 조건은 입술끼리 만나야 되고 특별한 요령은 필요치 않다")>]
[<TestCase("0/ak/ibwk/f jbj5kf ugwjtkfs ;danbhgs ofkc itxjtj6 'gx;ewmfs ifqjd lfw p4mesu/dsuf",
    ExpectedResult = "콩고물과 우유가 들어간 빙수는 차게 먹어야 특별한 맛이 잘 표현된다")>]
[<TestCase("j50/rmr2uts uufqlbd '/kkdpbw ll/Zkd ;f;;gz",
    ExpectedResult = "유쾌했던 땃쥐 토끼풀 쫓기 바쁨")>]
[<TestCase("j50/rmrqquts uufqlbd '/kkdpbw ll/Zkd ;f;;gz",
    ExpectedResult = "유쾌했던 땃쥐 토끼풀 쫓기 바쁨")>]
[<TestCase("vfvrvdbtbcbdgd", ExpectedResult = "ᅪᅫᅬᅯᅰᅱᅴ")>]
[<TestCase("/f/r/dbtbcbdgd", ExpectedResult = "ᅪᅫᅬᅯᅰᅱᅴ")>]
let ``parse returns expected result`` actual =
    Sebeolsik391KeyLayout.parse actual

[<TestCase("키스의 고유 조건은 입술끼리 만나야 되고 특별한 요령은 필요치 않다",
    ExpectedResult = "0dngjgd k/j5 l/ktsjgs jd3nbwkkdyd ifshfj6 u/dk/ 'gx;ewmfs j4yeajgs pdwj4od jfs1uf")>]
[<TestCase("콩고물과 우유가 들어간 빙수는 차게 먹어야 특별한 맛이 잘 표현된다",
    ExpectedResult = "0/ak/ibwk/f jbj5kf ugwjtkfs ;danbhgs ofkc itxjtj6 'gx;ewmfs ifqjd lfw p4mesu/dsuf")>]
[<TestCase("유쾌했던 땃쥐 토끼풀 쫓기 바쁨",
    ExpectedResult = "j50/rmrqquts uufqlbd '/kkdpbw ll/Zkd ;f;;gz")>]
[<TestCase("ᅪᅫᅬᅯᅰᅱᅴ", ExpectedResult = "/f/r/dbtbcbdgd")>]
let ``unparse returns expected result`` actual =
    Sebeolsik391KeyLayout.unparse actual
