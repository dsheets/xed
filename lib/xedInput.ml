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

type xmls = Xmlm.signal list

type t =
| Xmlm of Xmlm.input
| Xmls of (xmls * xmls) (* pre * post *)

let xmlm input = Xmlm input
let xmls xmls  = Xmls ([], xmls)

let rec xmls_of_xmlm xmls input =
  if (try Xmlm.eoi input with Xmlm.Error (_,`Unexpected_eoi) -> true)
  then List.rev xmls
  else
    let next = Xmlm.input input in
    xmls_of_xmlm (next::xmls) input

let to_xmls = function
  | Xmls (pre, c) -> List.rev_append pre c
  | Xmlm input    -> xmls_of_xmlm [] input

let buffer = function
  | Xmls (pre, c) -> Xmls (pre, c)
  | Xmlm input    -> let xmls = xmls_of_xmlm [] input in Xmls ([], xmls)

let try_with_eoi fn input ret =
  if Xmlm.eoi input
  then ret
  else fn input

let some fn arg = Some (fn arg)

let peek = function
  | Xmlm xmlm -> try_with_eoi (some Xmlm.peek) xmlm None
  | Xmls (_, []) -> None
  | Xmls (_, c::_) -> Some c

let drop input = match input with
  | Xmlm xmlm -> try_with_eoi (fun i -> ignore (Xmlm.input i)) xmlm (); input
  | Xmls (pre, []) -> input
  | Xmls (pre, c::r) -> Xmls (c::pre, r)

let rec skip_subtree k input = match peek input with
  | Some (`Data _ | `Dtd _) when k > 0 -> skip_subtree k (drop input)
  | Some `El_end when k > 1            -> skip_subtree (k - 1) (drop input)
  | Some (`Data _ | `Dtd _ | `El_end)  -> drop input
  | Some (`El_start _)                 -> skip_subtree (k + 1) (drop input)
  | None                               -> input

let rec string_of_path = XedPath.(function
  | Cdata k -> "cdata "^(string_of_int k)
  | Attr (_,name) -> "attr "^name
  | Child ((_,name), k, Some more) ->"child "^name^" "^(string_of_int k)^" -> "^(string_of_path more)
  | Child ((_,name), k, None) -> "child "^name^" "^(string_of_int k)
)

let string_of_peek_input = function
  | Some (`Data s) -> "data "^s
  | Some (`Dtd _) -> "dtd"
  | Some  `El_end -> "end"
  | Some (`El_start ((ns,name),attrs)) -> "tag "^name
  | None -> "none"

let seek_exn input path =
  let rec seek input path = XedPath.(match path, peek input with
    | _, (Some `El_end | None)             -> raise Not_found

    | Cdata 0, Some (`Data _)              -> input
    | Cdata k, Some (`Data _)              -> seek (drop input) (Cdata (k - 1))
    | Cdata _, Some (`El_start _ | `Dtd _) -> seek (skip_subtree 0 input) path

    | Attr name, Some (`El_start (_, attrs))
      when List.mem_assoc name attrs       -> input
    | Attr name, Some (`El_start _ | `Data _ | `Dtd _) -> raise Not_found
    | Child (name, 0, Some (Attr attr)),
      Some (`El_start (tag, _)) when name = tag -> seek input (Attr attr)

    | Child (name, 0, None),
      Some (`El_start (tag, _)) when name = tag -> input
    | Child (name, 0, Some sub),
      Some (`El_start (tag, _)) when name = tag -> seek (drop input) sub
    | Child (name, k, c_opt),
      Some (`El_start (tag, _)) when name = tag ->
      seek (skip_subtree 0 input) (Child (name, k - 1, c_opt))

    | Child (_,_,_), Some (`El_start _ | `Dtd _ | `Data _) ->
      seek (skip_subtree 0 input) path
  ) in
  seek input path

let subtree input =
  let rec collect acc depth input = match peek input with
    | Some (`El_start _ as s)     ->
      collect (s::acc) (depth + 1) (drop input)
    | Some `El_end when depth > 1 ->
      collect (`El_end::acc) (depth - 1) (drop input)
    | Some `El_end                -> List.rev (`El_end::acc)
    | Some (`Dtd _ | `Data _ as s)-> collect (s::acc) depth (drop input)
    | None -> List.rev acc
  in
  let sub = collect [] 0 input in
  Xmls ([], sub)

let seek input path =
  try Some (seek_exn input path) with Not_found -> None

let zipto input path =
  let (pre, post) = match seek_exn (buffer input) path with
    | Xmlm _ -> failwith "XedInput.zipto: seek xmlm unzippable (impossible)"
    | Xmls z -> z
  in
  let (focus, post) = match skip_subtree 0 (Xmls ([],post)) with
    | Xmlm _ ->
      failwith "XedInput.zipto: skip_subtree xmlm unzippable (impossible)"
    | Xmls z -> z
  in
  List.rev pre, List.rev focus, post

let concat xmlss = Xmls ([], List.concat xmlss)

(* TODO: FIXME *)
let ns_prefix = function
  | "patch" -> Some "patch"
  | _       -> None

let to_string input =
  let b = Buffer.create 1024 in
  let output = Xmlm.make_output ~decl:false ~ns_prefix (`Buffer b) in
  let rec accu input = match peek input with
    | Some s -> Xmlm.output output s; accu (drop input)
    | None -> ()
  in
  match peek input with
  | Some (`Dtd _) -> accu input; Buffer.contents b
  | Some (`El_start _ | `Data _ | `El_end) ->
    Xmlm.output output (`Dtd None);
    accu input;
    Buffer.contents b
  | None -> ""
