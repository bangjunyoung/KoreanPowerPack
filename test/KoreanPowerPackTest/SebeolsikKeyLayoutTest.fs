namespace KoreanPowerPack.Tests

open NUnit.Framework
open KoreanPowerPack

type Sebeolsik391KeyLayoutTest () =
    [<TestCase("0dngjgd k/j5 l/ktsjgs jd3nbwkkdyd ifshfj6 u/dk/ 'gx;ewmfs j4yeajgs pdwj4od jfSuf",
        ExpectedResult = "키스의 고유 조건은 입술끼리 만나야 되고 특별한 요령은 필요치 않다")>]
    [<TestCase("0/ak/ibwk/f jbj5kf ugwjtkfs ;danbhgs ofkc itxjtj6 'gx;ewmfs ifqjd lfw p4mesu/dsuf",
        ExpectedResult = "콩고물과 우유가 들어간 빙수는 차게 먹어야 특별한 맛이 잘 표현된다")>]
    [<TestCase("j50/rmr2uts uufqlbd '/kkdpbw ll/Zkd ;f;;gz",
        ExpectedResult = "유쾌했던 땃쥐 토끼풀 쫓기 바쁨")>]
    member __.``parse should work as expected``(actual) =
        Sebeolsik391KeyLayout.parse actual

    [<TestCase("키스의 고유 조건은 입술끼리 만나야 되고 특별한 요령은 필요치 않다",
        ExpectedResult = "0dngjgd k/j5 l/ktsjgs jd3nbwkkdyd ifshfj6 u/dk/ 'gx;ewmfs j4yeajgs pdwj4od jfSuf")>]
    [<TestCase("콩고물과 우유가 들어간 빙수는 차게 먹어야 특별한 맛이 잘 표현된다",
        ExpectedResult = "0/ak/ibwk/f jbj5kf ugwjtkfs ;danbhgs ofkc itxjtj6 'gx;ewmfs ifqjd lfw p4mesu/dsuf")>]
    [<TestCase("유쾌했던 땃쥐 토끼풀 쫓기 바쁨",
        ExpectedResult = "j50/rmr2uts uufqlbd '/kkdpbw ll/Zkd ;f;;gz")>]
    member __.``unparse should work as expected``(actual) =
        Sebeolsik391KeyLayout.unparse actual
