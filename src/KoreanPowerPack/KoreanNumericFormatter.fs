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
open KoreanNumericConverter

[<AutoOpen>]
module internal KoreanNumericFormatter =
    let validFormats =
        ["한자", Hanja
         "한자개별", HanjaDigit
         "한자혼합", HanjaMixed
         "고유", Native
         "고유관형", NativePrenoun]
        |> Map.ofList

    let handleInvalidFormat format (arg: obj) =
        match arg with
        | :? IFormattable as arg' -> arg'.ToString(format, null)
        | _ -> arg.ToString()

[<Sealed>]
[<AllowNullLiteral>]
type KoreanNumericFormatter() =
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
                match validFormats |> Map.tryFind format with
                | Some format' -> string arg |> toKorean format'
                | None -> handleInvalidFormat format arg

    interface IFormatProvider with
        member this.GetFormat(formatType) = 
            if formatType = typeof<ICustomFormatter> then this :> obj else null
