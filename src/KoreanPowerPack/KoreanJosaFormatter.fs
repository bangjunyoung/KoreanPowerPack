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
    let josas = [|
        [| "을";     "으로";   "은";     "이";     "과";     "이나";   "이고";
           "이며";   "이면";   "이라";   "이란";   "아";     "이여";   "이시여" |]
        [| "를";     "로";     "는";     "가";     "와";     "나";     "고";
           "며";     "면";     "라";     "란" ;    "야";     "여"  ;   "시여" |]
        [| "을(를)"; "(으)로"; "은(는)"; "이(가)"; "과(와)"; "(이)나"; "(이)고";
           "(이)며"; "(이)면"; "(이)라"; "(이)란"; "아(야)"; "(이)여"; "(이)시여" |]
    |]

    let trimChars = [| ' '; '\''; '\"'; '>'; ')'; '}'; ']' |]

    let combine josa cheeon =
        let minorIndexOf josa =
            match josas.[0] |> Array.tryFindIndex ((=) josa) with
            | Some index -> Some index
            | None -> josas.[1] |> Array.tryFindIndex ((=) josa)

        match minorIndexOf josa with
        | None -> None
        | Some minorIndex ->
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

            let (==) (value: char) (str: string) =
                str.IndexOf value >= 0

            let majorIndex =
                match cheeon with
                | NullOrEmpty -> 2
                | Number lastChar
                | Hangul lastChar ->
                    let _, _, jongseong = KoreanChar.decomposeCompat lastChar
                    match josa with
                    | "로" | "으로" -> 
                        if jongseong = "" || jongseong = "ㄹ" then 1 else 0
                    | _ -> if jongseong = "" then 1 else 0
                | LatinSingleChar lastChar ->
                    match josa with
                    | "로" | "으로" -> if lastChar = 'l' then 1 else 0
                    | _ -> if lastChar == "lmnr"  then 0 else 1
                | Latin (secondLastChar, lastChar) ->
                    match josa with
                    | "로" | "으로" -> if lastChar = 'l' then 1 else 0
                    | _ -> 
                        if lastChar == "afijosuvwxyz" ||
                           secondLastChar == "lmn" && lastChar == "cdkpqt" ||
                           secondLastChar == "aeiou" && lastChar = 'r' ||
                           secondLastChar = 'r' && lastChar = 'e' then 1
                        elif lastChar == "lmn" ||
                             secondLastChar = 'n' && lastChar = 'g' then 0
                        else 2
                | Punctuation lastChar ->
                    match lastChar with
                    | '#' -> 0
                    | '%' -> 1
                    | _ -> 2
                | _ -> 2

            Some <| cheeon + josas.[majorIndex].[minorIndex]

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
