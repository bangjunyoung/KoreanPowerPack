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

module Rembris.Globalization.KoreanNumericConverter

open System
open Rembris.FSharp

type FormatKind = Hanja | HanjaDigit | Native | NativePrenoun

let hanjaDigits = [|"영"; "일"; "이"; "삼"; "사"; "오"; "육"; "칠"; "팔"; "구"|]
let nativeDigits = [|""; "하나"; "둘"; "셋"; "넷"; "다섯"; "여섯"; "일곱"; "여덟"; "아홉"|]
let nativePrenounDigits = [|""; "한"; "두"; "세"; "네"; "다섯"; "여섯"; "일곱"; "여덟"; "아홉"|]
let nativeMultiplesOf10 = [|""; "열"; "스물"; "서른"; "마흔"; "쉰"; "예순"; "일흔"; "여든"; "아흔"|]
let lowerUnits = [|""; "십"; "백"; "천"|]
let higherUnits = [|""; "만"; "억"; "조"; "경"|]

/// Split the specified number into chunks of the specified length.
// 4321 |> chunkOf 2 = [21; 43]
// 1234567L |> chunkOf 4 = [4567; 123]
// 42I |> chunkOf 1 = [2; 4]
let inline chunkOf length number =
    let power = pown (10 ^> number) length
    let rec loop number =
        if number < power then [int number]
        else int (number % power) :: loop (number / power)
    loop number

/// The digit can be one of 1 to 9.
/// The power is an nth power of 10 and can be one of 0 to 3.
// 1 |> toHanjaWord 0 = "일"
// 1 |> toHanjaWord 1 = "일십"
// 1 |> toHanjaWord 2 = "일백"
let toHanjaWord power digit =
    hanjaDigits.[digit] + lowerUnits.[power]

let toHanjaDigitWord _ digit =
    hanjaDigits.[digit]

/// The digit can be one of 1 to 9.
/// The power is an nth power of 10 and can be one of 0 to 3.
// 1 |> toNativeWord 0 = "하나"
// 1 |> toNativeWord 1 = "열"
// 1 |> toNativeWord 2 = "백"
let toNativeWord power digit =
    match power, digit with
    | 0, _ -> nativeDigits.[digit]
    | 1, _ -> nativeMultiplesOf10.[digit]
    | _, 1 -> (* "일" 생략 *) lowerUnits.[power]
    | _ -> digit |> toHanjaWord power

/// The digit can be one of 1 to 9.
/// The power is an nth power of 10 and can be one of 0 to 3.
// 1 |> toNativePrenounWord 0 = "한"
// 1 |> toNativePrenounWord 1 = "열"
// 1 |> toNativePrenounWord 2 = "백"
let toNativePrenounWord power digit =
    match power, digit with
    | 0, _ -> nativePrenounDigits.[digit]
    | 1, _ -> nativeMultiplesOf10.[digit]
    | _, 1 -> (* "일" 생략 *) lowerUnits.[power]
    | _ -> digit |> toHanjaWord power

let toWord = function
    | Hanja -> toHanjaWord
    | HanjaDigit -> toHanjaDigitWord
    | Native -> toNativeWord
    | NativePrenoun -> toNativePrenounWord

/// Convert a number less than 10000 to Korean.
let smallNaturalToKorean format =
    chunkOf 1 >>
    Seq.mapi (fun power digit -> (power, digit)) >>
    Seq.filter (fun (_, digit) -> digit <> 0) >>
    Seq.map (fun (power, digit) -> digit |> (toWord format) power) >>
    Seq.rev >>
    String.concat ""

let naturalToKorean format =
    chunkOf 4 >>
    Seq.mapi (fun index number ->
        let format' =
            match format with
            | Native | NativePrenoun when index <> 0 -> Hanja
            | _ -> format
        smallNaturalToKorean format' number) >>
    Seq.zip higherUnits >>
    Seq.filter (fun (_, number) -> number <> "") >>
    Seq.map (fun (unit, number) -> number + unit) >>
    Seq.rev >>
    String.concat " "

let numberSeqToKorean =
    Seq.map (fun digit -> hanjaDigits.[int digit - int '0']) >>
    String.concat ""

let integerToKorean format number =
    match format with
    | HanjaDigit -> numberSeqToKorean number
    | _ ->
        let number' = Int64.Parse number
        if number' > 0L then naturalToKorean format number'
        elif number' < 0L then "음수 " + naturalToKorean format (abs number')
        else hanjaDigits.[0]

[<CompiledName("ToKorean")>]
let toKorean format (number: string) =
    match number.Split '.' with
    | [|intPart; fracPart|] ->
         integerToKorean format intPart + " 점 " + numberSeqToKorean fracPart
    | _ -> integerToKorean format number
