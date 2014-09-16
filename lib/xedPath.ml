(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

type split =
| From_end of int

type t =
| Cdata of int
| Attr  of Xmlm.name
| Child of Xmlm.name * int * t option

let rec rev pl = function
  | None           -> pl
  | Some (Cdata k) -> (Cdata k)::pl
  | Some (Attr n)  -> (Attr n)::pl
  | Some (Child (n, k, c)) -> rev (Child (n,k,c) :: pl) c

let with_child c = function
  | (Cdata _ | Attr _) as v -> v
  | Child (n, k, _) -> Child (n, k, c)

let rec top p = function
  | [] -> p
  | r::rs -> top (with_child (Some p) r) rs

let this = function
  | Cdata _       -> Cdata 0
  | Attr attr     -> Attr attr
  | Child (n,_,c) -> Child (n,0,c)

let rec split_from_rev k tail = function
  | [] -> None, tail
  | p::ps when k > 0 -> split_from_rev (k - 1) (Some p) ps
  | p::ps -> Some (top (with_child None p) ps), Some (this p)

let split path = function
  | From_end k -> split_from_rev k None (rev [] (Some path))
