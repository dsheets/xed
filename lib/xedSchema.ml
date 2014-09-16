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

type whitespace =
| Trim
| Collapse
| Verbatim

type prettify_context = {
  pretty_input : XedInput.t;
  indentation  : int;
  white_stack  : whitespace list;
}

module TagMap = Map.Make(struct
  type t = Xmlm.name
  let compare (ns,el) (ns',el') =
    match String.compare ns ns' with
    | 0 -> String.compare el el'
    | n -> n
end)

module type S = sig
  val whitespace : whitespace TagMap.t
end

let make_indent k = "\n" ^ (String.make k ' ')

let increment = 2

let prettify_context_of_input pretty_input = {
  pretty_input; indentation = 0; white_stack = [];
}

let input_of_prettify_context ({ pretty_input }) = pretty_input

let rec is_rest_char c i s =
  if String.length s > i
  then s.[i] = c && (is_rest_char c (i + 1) s)
  else true

let is_rest_space = is_rest_char ' '

let fixup_indent indentation s =
  if String.length s > 0
  then if s.[0] = '\n'
    then if is_rest_space 1 s
      then [`Data (make_indent indentation)]
      else [`Data s]
    else [`Data s]
  else [`Data s; `Data (make_indent indentation)]

let prettify (module Schema : S) last { pretty_input; indentation; white_stack } =
  let indent s = match white_stack, last, s with
    | [], _, _
    | _, _, `Dtd _
    | Verbatim::_, _, _
    | Collapse::_, _, `Data _
    | Collapse::_, `Data _, (`El_start _ | `El_end)
    | Trim::_, `Data _, `Data _
    | (Trim | Collapse)::_, `El_start _, `El_end ->
      [s]
    | Collapse::_, (`Dtd _ | `El_end), `El_end
    | Trim::_, _, `El_end ->
      [s; `Data (make_indent (indentation - increment))]
    | Collapse::_, (`Dtd _ | `El_start _ | `El_end), `El_start _
    | Trim::_, _, `Data _
    | Trim::_, _, `El_start _ ->
      [s; `Data (make_indent indentation)]
  in
  XedInput.(match peek pretty_input with
  | Some (`El_start (tag, attrs) as s) -> {
    pretty_input = drop pretty_input;
    indentation = indentation + increment;
    white_stack =
      (try TagMap.find tag Schema.whitespace with Not_found -> Verbatim)
    :: white_stack;
  }, indent s
  | Some  `El_end -> {
    pretty_input = drop pretty_input;
    indentation = indentation - increment;
    white_stack = List.tl white_stack;
  }, indent `El_end
  | Some (`Dtd _ as s) -> {
    pretty_input = drop pretty_input; indentation; white_stack;
  }, [s]
  | Some (`Data _ as s) -> {
    pretty_input = drop pretty_input; indentation; white_stack;
  }, indent s
  | None -> { pretty_input = pretty_input; indentation; white_stack }, []
  )
