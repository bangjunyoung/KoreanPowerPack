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

namespace KoreanPowerPack.FSharp

module String =
    let ofSeq (source: seq<char>) =
        (System.Text.StringBuilder(Seq.length source), source)
        ||> Seq.fold (fun builder c -> builder.Append(c))
        |> string

[<AutoOpen>]
module Exception =
    let invalidArgNull argName (message: string) =
        raise <| System.ArgumentNullException(argName, message)

    let raiseIfNull argName arg =
        if arg = null then
            invalidArgNull argName (argName + " can't be null")

module Array =
    let tryBinarySearch (value: 'a) (source: 'a[]) =
        let rec loop lo hi =
            if lo <= hi then
                let mid = lo + (hi - lo) / 2
                match sign <| compare value source.[mid] with
                | 0 -> Some mid
                | 1 -> loop (mid + 1) hi
                | _ -> loop lo (mid - 1)
            else None

        loop 0 (source.Length - 1)
