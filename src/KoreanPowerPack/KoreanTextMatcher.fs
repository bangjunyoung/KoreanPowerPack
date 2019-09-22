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

type KoreanTextMatch
    private (matcher: KoreanTextMatcher, text: string, startIndex: int,
             length: int, success: bool) =
    do
        if isNull text then nullArg "text"
        if startIndex < 0 ||
           text.Length = 0 && startIndex > 0 ||
           text.Length > 0 && startIndex >= text.Length then
            raise <| ArgumentOutOfRangeException("startIndex",
                         sprintf "startIndex: %d is out of range 0 .. %d"
                             startIndex (text.Length - 1))
        if length < 0 || length > text.Length then
            raise <| ArgumentOutOfRangeException("length",
                         sprintf "length: %d is out of range 0 .. %d"
                             length (text.Length - 1))
        if startIndex + length > text.Length then
            raise <| ArgumentOutOfRangeException("length",
                         sprintf "startIndex + length: %d is out of range 0 .. %d"
                             (startIndex + length) (text.Length - 1))

    static let empty = KoreanTextMatch(KoreanTextMatcher(""), "", 0, 0, false)

    static member Empty = empty

    new(matcher, text, startIndex, length) =
        KoreanTextMatch(matcher, text, startIndex, length, true)

    member __.Value = text.AsMemory().Slice(startIndex, length)

    member __.Index = startIndex

    member __.Length = length

    member __.Success = success

    member __.NextMatch() =
        if not success then KoreanTextMatch.Empty
        else matcher.Match(text, startIndex + length)

and KoreanTextMatcher(pattern: string) =
    do
        if isNull pattern then nullArg "pattern"

    let pattern, startAnchorFound, endAnchorFound =
        if pattern.Length = 0 then pattern, false, false
        else
            match pattern.[0] = '^', pattern.[pattern.Length - 1] = '$' with
            | true,  true  -> pattern.Substring(1, pattern.Length - 2), true, true
            | true,  false -> pattern.Substring(1, pattern.Length - 1), true, false
            | false, true  -> pattern.Substring(0, pattern.Length - 1), false, true
            | false, false -> pattern, false, false

    member this.Match(text, startIndex, length) =
        if pattern.Length = 0 then KoreanTextMatch(this, text, 0, 0)
        elif length < pattern.Length then KoreanTextMatch.Empty
        else
            let isMatch xs ys =
                (xs, ys)
                ||> Seq.zip
                |> Seq.forall (fun (x, y) -> KoreanCharApproxMatcher.isMatch x y)

            text
            |> Seq.skip startIndex
            |> Seq.take length
            |> Seq.windowed pattern.Length
            |> Seq.tryFindIndex (fun subtext -> isMatch subtext pattern)
            |> function
               | Some index -> KoreanTextMatch(this, text, index, pattern.Length)
               | None -> KoreanTextMatch.Empty

    member this.Match(text: string,
                      [<Optional; DefaultParameterValue(0)>] startIndex: int) =
        if isNull text then nullArg "text"
        if startIndex < 0 ||
           text.Length = 0 && startIndex > 0 ||
           text.Length > 0 && startIndex >= text.Length then
            raise <| ArgumentOutOfRangeException("startIndex",
                         sprintf "startIndex: %d is out of range 0 .. %d"
                             startIndex (text.Length - 1))

        let textSpan =
            let length = text.Length - startIndex
            if length < pattern.Length then None
            else
                match startAnchorFound, endAnchorFound with
                | true,  true  -> if text.Length <> pattern.Length then None
                                  else Some(startIndex, length)
                | true,  false -> if startIndex <> 0 then None
                                  else Some(startIndex, pattern.Length)
                | false, true  -> Some(text.Length - pattern.Length, pattern.Length)
                | false, false -> Some(startIndex, length)

        match textSpan with
        | None -> KoreanTextMatch.Empty
        | Some(startIndex, length) ->
            if length = 0 then KoreanTextMatch(this, text, 0, 0)
            else this.Match(text, startIndex, length)

    member this.Matches(text,
                        [<Optional; DefaultParameterValue(0)>] startIndex) =
        let rec loop (m: KoreanTextMatch) = seq {
            if m.Success then
                yield m
                yield! loop <| m.NextMatch()
        }
        loop <| this.Match(text, startIndex)

    static member Matches(text, pattern) =
        KoreanTextMatcher(pattern).Matches(text)

    static member Match(text, pattern) =
        KoreanTextMatcher(pattern).Match(text)

    static member IsMatch(text, pattern) =
        KoreanTextMatcher.Match(text, pattern).Success
