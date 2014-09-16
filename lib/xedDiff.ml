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

type patch =
| Patch_insert  of XedInput.xmls
| Patch_replace of XedInput.xmls
| Patch_delete

type t = (XedPath.t * patch) list

let string_of_patch = Xmldiff.string_of_patch

let xmlns = "patch" (* TODO: FIXME *)

module Schema : XedSchema.S = struct
  open XedSchema

  let whitespace = List.fold_left
    (fun map (tag,t) -> TagMap.add tag t map)
    TagMap.empty
    [
      (xmlns,"patch"),    Collapse;
      (xmlns,"diff"),     Collapse;
      (xmlns,"insert"),   Verbatim;
      (xmlns,"delete"),   Verbatim;
      (xmlns,"replace"),  Collapse;
      (xmlns,"original"), Verbatim;
      (xmlns,"new"),      Verbatim;
    ]
end

module Xmldiff = struct
  open Xmldiff

  let diff = diff

  let attrs_of_attrs = List.fold_left (fun map (name,value) ->
    Nmap.add name value map
  ) Nmap.empty

  let rec tree_of_input input = match XedInput.peek input with
    | Some (`Dtd _)  -> tree_of_input (XedInput.drop input)
    | Some (`Data s) -> `D s
    | Some  `El_end  -> failwith "XedDiff.Xmldiff.tree_of_input: bad end signal"
    | Some (`El_start ((ns, tag) as name, attrs)) ->
      snd (open_tag (XedInput.drop input) name attrs [])
    | None -> failwith "XedDiff.Xmldiff.tree_of_input: no root"
  and open_tag input name attrs children = match XedInput.peek input with
    | Some (`Dtd _)  -> failwith "XedDiff.Xmldiff.open_tag: bad dtd signal"
    | Some (`Data s) ->
      open_tag (XedInput.drop input) name attrs ((`D s)::children)
    | Some  `El_end  ->
      XedInput.drop input, `E (name, attrs_of_attrs attrs, List.rev children)
    | Some (`El_start (tag_name, tag_attrs)) ->
      let input, el = open_tag (XedInput.drop input) tag_name tag_attrs [] in
      open_tag input name attrs (el::children)
    | None -> failwith "XedDiff.Xmldiff.open_tag: premature eoi"

  let attrs_to_attrs = Nmap.bindings

  let xml_s_of_tree tree =
    let rec dft list = function
      | `E (name, attrs, children) ->
        let attrs = attrs_to_attrs attrs in
        let new_list = `El_start (name, attrs) :: list in
        `El_end :: (List.fold_left dft new_list children)
      | `D s -> (`Data s) :: list
    in
    List.rev (dft [] tree)

  let xml_s_of_name_attrs name attrs = [
    `El_start (name, attrs_to_attrs attrs);
    `El_end;
  ]

  let rec path_to_xedpath = XedPath.(function
    | Path_node (name, k, None)   -> Child (name, k, None)
    | Path_node (name, k, Some c) -> Child (name, k, Some (path_to_xedpath c))
    | Path_cdata k -> Cdata k
  )

  let op_to_op = function
    | PInsertTree tree          -> Patch_insert (xml_s_of_tree tree)
    | PDeleteTree               -> Patch_delete
    | PUpdateCData s            -> Patch_replace ([`Data s])
    | PReplace tree             -> Patch_replace (xml_s_of_tree tree)
    | PUpdateNode (name, attrs) ->
      Patch_replace (xml_s_of_name_attrs name attrs)

  let patch_to_patch patch = List.(rev (rev_map (fun (path, op) ->
    path_to_xedpath path, op_to_op op
  ) patch))
end

let diff base branch =
  let open Xmldiff in
  let base_tree   = tree_of_input base in
  let branch_tree = tree_of_input branch in
  let _, patch    = diff base_tree branch_tree in
  (*print_endline (string_of_patch patch);*)
  patch_to_patch patch

let markup_patch subtree path patch =
  let patch_markup focus = match patch with
    | Patch_insert  xmls -> focus@[
      `El_start (("patch", "insert"),[]);
    ]@xmls@[`El_end]
    | Patch_replace xmls -> [
      `El_start (("patch", "replace"),[]);
      `El_start (("patch", "original"),[]);
    ]@focus@[`El_end]@[
      `El_start (("patch", "new"), []);
    ]@xmls@[
      `El_end;
      `El_end;
    ]
    | Patch_delete -> [
      `El_start (("patch", "delete"),[]);
    ]@focus@[
      `El_end;
    ]
  in
  let pre, cursor, post = XedInput.zipto subtree path in
  XedInput.concat [pre; patch_markup cursor; post]

let context base (path, patch) =
  let parent, child = XedPath.(split path (From_end 1)) in
  let parent = match parent with Some p -> p | None -> path in
  let child  = match child  with Some p -> p | None -> path in
  let pos = XedInput.seek_exn base parent in
  let subtree = XedInput.subtree pos in
  markup_patch subtree child patch

let doc_of_contexts inputs =
  let patches = List.map (fun input ->
    (`El_start (("patch", "patch"),[]))::(XedInput.to_xmls input)@[`El_end]
  ) inputs in
  XedInput.concat (
    [`El_start (("patch", "diff"),[])]
    ::(patches@[[`El_end]]))
