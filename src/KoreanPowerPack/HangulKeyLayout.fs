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

module KoreanPowerPack.HangulKeyLayout

open FParsec
open KoreanPowerPack.FSharp

let parse syllableParser str =
    assert (str <> null)

    let parser = many (syllableParser <|> anyChar) |>> String.ofSeq
    match run parser str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> invalidArg "str" errorMsg

let unparse syllableDecomposer (str: string) =
    assert (str <> null)

    str
    |> Seq.map (fun c ->
        try c |> syllableDecomposer
        with _ -> c |> string)
    |> String.concat ""

let mapping from to' =
    let map = Map.ofSeq <| Seq.zip from to'
    fun c ->
        match map |> Map.tryFind c with
        | Some x -> x
        | None -> c
