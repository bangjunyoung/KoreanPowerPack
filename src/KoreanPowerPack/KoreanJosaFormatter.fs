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

namespace KoreanPowerPack

open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions

[<AutoOpen>]
module internal KoreanJosaFormatter =
    let validJosas = [|
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
    |]

    let trimChars = [|' '; '\''; '\"'; '>'; ')'; '}'; ']'|]

    let combine josa cheeon =
        match validJosas
              |> Array.tryFindIndex
                  (fun (form0, form1, _) -> form0 = josa || form1 = josa) with
        | None -> None
        | Some index ->
            let (|NullOrEmpty|_|) str =
                if String.IsNullOrEmpty str then Some NullOrEmpty
                else None

            let (|Number|_|) str =
                // 가장 작은 자리가 "조"로 끝나는 수는 뒤에 "가/와/는/를" 등이 와야 하기 때문에
                // 정규식을 써서 전체수를 추출한 다음 종성이 필요한지 검사한다.
                let number = Regex.Match(str, @"\d+$").ToString()
                if number <> "" then
                    let converted = KoreanNumericFormatter().Format("한자", number)
                    Some converted.[converted.Length - 1]
                else None

            let (|Hangul|_|) (str: string) =
                let lastChar = str.[str.Length - 1]
                if lastChar |> KoreanChar.isSyllable then Some lastChar
                else None

            let (|LatinSingleChar|_|) (str: string) =
                let lastChar = str.[str.Length - 1]
                if str.Length = 1 && Char.IsLetter(lastChar) then
                    Some <| Char.ToLower(lastChar)
                else None

            let (|Latin|_|) (str: string) =
                let lastChar = str.[str.Length - 1]
                if str.Length >= 2 && Char.IsLetter(lastChar) then
                    let secondLastChar = str.[str.Length - 2]
                    Some <| (Char.ToLower(secondLastChar), Char.ToLower(lastChar))
                else None

            let (|Punctuation|_|) (str: string) =
                let lastChar = str.[str.Length - 1]
                if Char.IsPunctuation(lastChar) then Some lastChar
                else None

            let (=~) (value: char) (str: string) =
                str.IndexOf value >= 0

            let josa =
                let (form0, form1, form2) = validJosas.[index]
                match cheeon with
                | NullOrEmpty -> form2
                | Number lastChar
                | Hangul lastChar ->
                    let _, _, jongseong = KoreanChar.decomposeCompat lastChar
                    match josa with
                    | "로" | "으로" -> 
                        if jongseong = "" || jongseong = "ㄹ" then form1 else form0
                    | _ -> if jongseong = "" then form1 else form0
                | LatinSingleChar lastChar ->
                    match josa with
                    | "로" | "으로" -> if lastChar = 'l' then form1 else form0
                    | _ -> if lastChar =~ "lmnr"  then form0 else form1
                | Latin (secondLastChar, lastChar) ->
                    match josa with
                    | "로" | "으로" -> if lastChar = 'l' then form1 else form0
                    | _ -> 
                        if lastChar =~ "afijosuvwxyz" ||
                           secondLastChar =~ "lmn" && lastChar =~ "cdkpqt" ||
                           secondLastChar =~ "aeiou" && lastChar = 'r' ||
                           (secondLastChar, lastChar) = ('r', 'e') then form1
                        elif lastChar =~ "lmn" ||
                             (secondLastChar, lastChar) = ('n', 'g') then form0
                        else form2
                | Punctuation lastChar ->
                    match lastChar with
                    | '#' -> form0
                    | '%' -> form1
                    | _ -> form2
                | _ -> form2

            Some <| cheeon + josa

[<Sealed>]
[<AllowNullLiteral>]
type KoreanJosaFormatter() =
    member this.Format(format, arg: obj,
                       [<Optional; DefaultParameterValue(null: IFormatProvider)>]
                       formatProvider) =
        (this :> ICustomFormatter).Format(format, arg, formatProvider)

    member this.GetFormat(formatType) = 
        (this :> IFormatProvider).GetFormat(formatType)

    interface ICustomFormatter with
        member __.Format(format, arg,
                         [<Optional; DefaultParameterValue(null)>]
                         formatProvider) =
            if arg = null then
                ""
            else
                let argString = string arg
                match combine format (argString.TrimEnd trimChars) with
                | Some combined -> combined
                | None -> argString + format

    interface IFormatProvider with
        member this.GetFormat(formatType) = 
            if formatType = typeof<ICustomFormatter> then this :> obj else null
