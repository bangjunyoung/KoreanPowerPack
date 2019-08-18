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

module KoreanPowerPack.KoreanJosaFormatterTest

open NUnit.Framework

let validJosas = [
    "을", "를", "을(를)"
    "으로", "로", "(으)로"
    "은", "는", "은(는)"
    "이", "가", "이(가)"
    "과", "와", "과(와)"
    "아", "야", "아(야)"
    "이든", "든", "(이)든"
    "이나", "나", "(이)나"
    "이고", "고", "(이)고"
    "이며", "며", "(이)며"
    "이면", "면", "(이)면"
    "이라", "라", "(이)라"
    "이란", "란", "(이)란"
    "이랑", "랑", "(이)랑"
    "이야말로", "야말로", "(이)야말로"
    "이여", "여", "(이)여"
    "이시여", "시여", "(이)시여"
]

let generateTestCaseData testStrings expected =
    validJosas
    |> List.collect (fun (formC, formV, formA) ->
        testStrings
        |> List.collect (fun cheeon ->
            if expected |> List.exists ((=) (cheeon + formC)) then
                [cheeon, formC, cheeon + formC
                 cheeon, formV, cheeon + formC]
            elif expected |> List.exists ((=) (cheeon + formV)) then
                [cheeon, formC, cheeon + formV
                 cheeon, formV, cheeon + formV]
            else
                [cheeon, formC, cheeon + formA
                 cheeon, formV, cheeon + formA]))
    |> List.map (fun (cheeon, josa, combined) ->
        TestCaseData(cheeon, josa).Returns(combined))

let testHangul =
    let testStrings = ["비"; "눈"; "물"]
    let expected = [
        "비를"; "비로"; "비는"; "비가"; "비와"; "비야"
        "비든"; "비나"; "비고"; "비며"; "비면"; "비라"
        "비란"; "비랑"; "비야말로"; "비여"; "비시여"
        "눈을"; "눈으로"; "눈은"; "눈이"; "눈과"; "눈아"
        "눈이든"; "눈이나"; "눈이고"; "눈이며"; "눈이면"; "눈이라"
        "눈이란"; "눈이랑"; "눈이야말로"; "눈이여"; "눈이시여"
        "물을"; "물로"; "물은"; "물이"; "물과"; "물아"
        "물이든"; "물이나"; "물이고"; "물이며"; "물이면"; "물이라"
        "물이란"; "물이랑"; "물이야말로"; "물이여"; "물이시여"
    ]

    generateTestCaseData testStrings expected

let testNumbers =
    let testStrings = ["0"; "1"; "2"; "3000000000000"]
    let expected = [
        "0을"; "0으로"; "0은"; "0이"; "0과"; "0아"
        "0이든"; "0이나"; "0이고"; "0이며"; "0이면"; "0이라"
        "0이란"; "0이랑"; "0이야말로"; "0이여"; "0이시여"
        "1을"; "1로"; "1은"; "1이"; "1과"; "1아"
        "1이든"; "1이나"; "1이고"; "1이며"; "1이면"; "1이라"
        "1이란"; "1이랑"; "1이야말로"; "1이여"; "1이시여"
        "2를"; "2로"; "2는"; "2가"; "2와"; "2야"
        "2든"; "2나"; "2고"; "2며"; "2면"; "2라"
        "2란"; "2랑"; "2야말로"; "2여"; "2시여"
        "3000000000000를"; "3000000000000로"; "3000000000000는"
        "3000000000000가"; "3000000000000와"; "3000000000000야"
        "3000000000000든"; "3000000000000나"; "3000000000000고"
        "3000000000000며"; "3000000000000면"; "3000000000000라"
        "3000000000000란"; "3000000000000랑"; "3000000000000야말로"
        "3000000000000여"; "3000000000000시여"
    ]

    generateTestCaseData testStrings expected

[<TestCaseSource("testHangul")>]
let ``should handle Hangul strings``cheeon josa =
    KoreanJosaFormatter().Format(josa, cheeon)

[<TestCaseSource("testNumbers")>]
let ``should handle numbers`` cheeon josa =
    KoreanJosaFormatter().Format(josa, cheeon)
