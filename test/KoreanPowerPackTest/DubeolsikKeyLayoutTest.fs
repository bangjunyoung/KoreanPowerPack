namespace KoreanPowerPack.Tests

open NUnit.Framework
open KoreanPowerPack

type DubeolsikKeyLayoutTest () =
    [<TestCase("zltmdml rhdb whrjsdms dlqtnfRlfl aksskdi ehlrh xmrqufgks dyfuddms vlfdycl dksgek",
        ExpectedResult = "키스의 고유 조건은 입술끼리 만나야 되고 특별한 요령은 필요치 않다")>]
    [<TestCase("zhdrhanfrhk dndbrk emfdjrks qldtnsms ckrp ajrdjdi xmrqufgks aktdl wkf vygusehlsek",
        ExpectedResult = "콩고물과 우유가 들어간 빙수는 차게 먹어야 특별한 맛이 잘 표현된다")>]
    [<TestCase("dbzhogoTejs Ektwnl xhRlvnf Whcrl qkQma",
        ExpectedResult = "유쾌했던 땃쥐 토끼풀 쫓기 바쁨")>]
    member __.``parse should work as expected``(actual) =
        DubeolsikKeyLayout.parse actual

    [<TestCase("키스의 고유 조건은 입술끼리 만나야 되고 특별한 요령은 필요치 않다",
        ExpectedResult = "zltmdml rhdb whrjsdms dlqtnfRlfl aksskdi ehlrh xmrqufgks dyfuddms vlfdycl dksgek")>]
    [<TestCase("콩고물과 우유가 들어간 빙수는 차게 먹어야 특별한 맛이 잘 표현된다",
        ExpectedResult = "zhdrhanfrhk dndbrk emfdjrks qldtnsms ckrp ajrdjdi xmrqufgks aktdl wkf vygusehlsek")>]
    [<TestCase("유쾌했던 땃쥐 토끼풀 쫓기 바쁨",
        ExpectedResult = "dbzhogoTejs Ektwnl xhRlvnf Whcrl qkQma")>]
    member __.``unparse should work as expected``(actual) =
        DubeolsikKeyLayout.unparse actual
