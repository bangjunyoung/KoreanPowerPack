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

module KoreanPowerPack.DubeolsikKeyLayoutTest

open NUnit.Framework

[<TestCase("zltmdml rhdb whrjsdms dlqtnfRlfl aksskdi ehlrh xmrqufgks dyfuddms vlfdycl dksgek",
    ExpectedResult = "키스의 고유 조건은 입술끼리 만나야 되고 특별한 요령은 필요치 않다")>]
[<TestCase("zhdrhanfrhk dndbrk emfdjrks qldtnsms ckrp ajrdjdi xmrqufgks aktdl wkf vygusehlsek",
    ExpectedResult = "콩고물과 우유가 들어간 빙수는 차게 먹어야 특별한 맛이 잘 표현된다")>]
[<TestCase("dbzhogoTejs Ektwnl xhRlvnf Whcrl qkQma",
    ExpectedResult = "유쾌했던 땃쥐 토끼풀 쫓기 바쁨")>]
let ``parse should work as expected`` actual =
    DubeolsikKeyLayout.parse actual

[<TestCase("키스의 고유 조건은 입술끼리 만나야 되고 특별한 요령은 필요치 않다",
    ExpectedResult = "zltmdml rhdb whrjsdms dlqtnfRlfl aksskdi ehlrh xmrqufgks dyfuddms vlfdycl dksgek")>]
[<TestCase("콩고물과 우유가 들어간 빙수는 차게 먹어야 특별한 맛이 잘 표현된다",
    ExpectedResult = "zhdrhanfrhk dndbrk emfdjrks qldtnsms ckrp ajrdjdi xmrqufgks aktdl wkf vygusehlsek")>]
[<TestCase("유쾌했던 땃쥐 토끼풀 쫓기 바쁨",
    ExpectedResult = "dbzhogoTejs Ektwnl xhRlvnf Whcrl qkQma")>]
let ``unparse should work as expected``actual =
    DubeolsikKeyLayout.unparse actual
