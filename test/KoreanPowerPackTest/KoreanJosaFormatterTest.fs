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

module List =
    let unzip4 source =
        (source, ([], [], [], []))
        ||> List.foldBack (fun (f1, f2, f3, f4) (acc1, acc2, acc3, acc4)  ->
            f1 :: acc1, f2 :: acc2, f3 :: acc3, f4 :: acc4) 

let validJosas = [
    "로", "으로", "로", "(으)로"
#if false
    "는", "은", "은", "은(는)"
    "를", "을", "을", "을(를)"
    "가", "이", "이", "이(가)"
    "와", "과", "과", "과(와)"
    "나", "이나", "이나", "(이)나"
    "라", "이라", "이라", "(이)라"
    "라고", "이라고", "이라고", "(이)라고"
    "란", "이란", "이란", "(이)란"
    "랑", "이랑", "이랑", "(이)랑"
    "로서", "으로서", "로서", "(으)로서"
    "로써", "으로써", "로써", "(으)로써"
    "나마", "이나마", "이나마", "(이)나마"
    "야", "아", "아", "아(야)"
    "야말로", "이야말로", "이야말로", "(이)야말로"
    "여", "이여", "이여", "(이)여"
    "시여", "이시여", "이시여", "(이)시여"
#endif
]

let josasFormV, josasFormC, josasFormL, josasFormA =
    validJosas |> List.unzip4

let inline join cheeons josas =
    (cheeons, josas)
    ||> List.allPairs
    |> List.map (fun (cheeon, josa) -> cheeon + josa)

let generateTestCaseData testStrings expected =
    validJosas
    |> List.collect (fun (formV, formC, _, formA) ->
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

let testHangulLetters =
    let beforeFormV = ["피"]
    let beforeFormC = ["땀"]
    let beforeFormL = ["눈물"]
    let testStrings = beforeFormV @ beforeFormC @ beforeFormL

    let expected =
        join beforeFormV josasFormV @
        join beforeFormC josasFormC @
        join beforeFormL josasFormL

    generateTestCaseData testStrings expected

let testLatinLetters =
    let beforeFormV = [
        "Asia"; "elf"; "phi"; "Troj"; "halo"; "Mars"
        "you"; "luv"; "cow"; "six"; "by"; "jazz"
        "calc"; "fold"; "milk"; "help"; "silq"; "bolt"
        "ramc"; "namd"; "timk"; "pump"; "memq"; "dreamt"
        "sync"; "mind"; "link"; "ninp"; "ranq"; "hunt"
        "marc"; "lord"; "park"; "warp"; "kirq"; "dart"
        "bear"; "user"; "pair"; "door"; "tour"
        "orb";  "more"; "centre"
    ]
    let beforeFormL = ["ball"]
    let beforeFormC = ["mom"; "bun"; "ring"]
    let beforeFormA = ["mob"; "food"; "good"; "sec"; "bag"]
    let testStrings = beforeFormV @ beforeFormC @ beforeFormL @ beforeFormA

    let expected =
        join beforeFormV josasFormV @
        join beforeFormC josasFormC @
        join beforeFormL josasFormL @
        join beforeFormA josasFormA

    generateTestCaseData testStrings expected

let testNumbers =
    let beforeFormV = ["2"; "3000000000000"]
    let beforeFormC = ["0"]
    let beforeFormL = ["1"]
    let testStrings = beforeFormV @ beforeFormC @ beforeFormL

    let expected =
        join beforeFormV josasFormV @
        join beforeFormC josasFormC @
        join beforeFormL josasFormL

    generateTestCaseData testStrings expected

let testPunctuations =
    let beforeFormV = ["%"]
    let beforeFormC = ["#"]
    let testStrings = beforeFormV @ beforeFormC

    let expected =
        join beforeFormV josasFormV @
        join beforeFormC josasFormC

    generateTestCaseData testStrings expected

[<TestCaseSource("testHangulLetters")>]
let ``handle Hangul``cheeon josa =
    KoreanJosaFormatter().Format(josa, cheeon)

[<TestCaseSource("testLatinLetters")>]
let ``handle Latin letters``cheeon josa =
    KoreanJosaFormatter().Format(josa, cheeon)

[<TestCaseSource("testNumbers")>]
let ``handle numbers`` cheeon josa =
    KoreanJosaFormatter().Format(josa, cheeon)

[<TestCaseSource("testPunctuations")>]
let ``handle punctuations`` cheeon josa =
    KoreanJosaFormatter().Format(josa, cheeon)
