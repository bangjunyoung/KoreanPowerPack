//
// Copyright 2019, 2026 Bang Jun-young
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
open Microsoft.FSharp.NativeInterop
open KoreanChar

module Span =
    #nowarn "9"
    let inline stackalloc<'T when 'T : unmanaged> length =
        Span<'T>(NativePtr.toVoidPtr (NativePtr.stackalloc<'T> length), length)

    #nowarn "3391"
    let inline toReadOnlySpan<'T> (span: Span<'T>) : ReadOnlySpan<'T> = span

module KoreanCharApproxMatcher =
    let private decompose destination c =
        if isSyllable c then
            let length = decomposeToCompatInto destination c
            Span.toReadOnlySpan<char>(destination.Slice(0, length))
        elif isCompatChoseong c then
            (splitJamo c).AsSpan()
        elif isChoseong c then
            (splitJamo (convertChoseongToCompat c)).AsSpan()
        else
            destination[0] <- c
            Span.toReadOnlySpan<char>(destination.Slice(0, 1))

    [<CompiledName("IsMatch")>]
    let isMatch t p =
        if t = p then true
        else
            let tBuffer = Span.stackalloc<char> 6
            let pBuffer = Span.stackalloc<char> 6

            (decompose tBuffer t).StartsWith(decompose pBuffer p)
