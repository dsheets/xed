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

open Printf

(* TODO: FIXME *)
let ns = function
  | "patch" -> Some "patch"
  | _       -> None

let xml_of_oc oc =
  XedInput.xmlm (Xmlm.make_input ~ns (`Channel oc))

let buffer_file file =
  let oc = open_in file in
  let input = XedInput.buffer (xml_of_oc oc) in
  close_in oc;
  input

let ( / ) = Filename.concat

(* TODO: find a better location *)
let prettify input =
  let schema = (module XedDiff.Schema : XedSchema.S) in
  let ctxt = XedSchema.prettify_context_of_input input in
  let rec loop acc last c =
    let c, ss = XedSchema.prettify schema last c in
    match XedInput.peek (XedSchema.input_of_prettify_context c) with
    | None -> List.rev (ss@acc)
    | Some _ -> loop (ss@acc) (List.hd ss) c
  in
  XedInput.xmls (loop [] (`Dtd None) ctxt)

module Diff = struct

  (* base * branch * diff *)
  let tests = [
    "cdata.xml",         "other_cdata.xml",   "other_cdata_diff.xml";
    "other_cdata.xml",   "cdata.xml",         "cdata_diff.xml";
    "root.xml",          "child.xml",         "root_child_diff.xml";
    "child.xml",         "root.xml",          "child_root_diff.xml";
    "child.xml",         "children.xml",      "child_children_diff.xml";
    "children.xml",      "child.xml",         "children_child_diff.xml";
    "children.xml",      "multichildren.xml", "children_multichildren_diff.xml";
    "multichildren.xml", "children.xml",      "multichildren_children_diff.xml";
    "multichildren.xml", "multimulti.xml",    "multichildren_multimulti_diff.xml";
    "multimulti.xml",    "multichildren.xml", "multimulti_multichildren_diff.xml";
    "multimulti.xml",    "multideep.xml",     "multimulti_multideep_diff.xml";
    "multideep.xml",     "multimulti.xml",    "multideep_multimulti_diff.xml";
    "multideep.xml",     "multideepattr.xml", "multideep_multideepattr_diff.xml";
    "multideepattr.xml", "multideep.xml",     "multideepattr_multideep_diff.xml";
  ]

  let run_test dir (base, branch, diff) =
    print_endline ("testing diff("^base^","^branch^")="^diff);
    let base_in   = buffer_file (dir / base) in
    let branch_in = buffer_file (dir / branch) in
    let diff_in   = buffer_file (dir / diff) in
    let real_diff = XedDiff.diff base_in branch_in in
    let contexts  = List.map (XedDiff.context base_in) real_diff in
    let diff_doc  = prettify (XedDiff.doc_of_contexts contexts) in
    let diff_diff = XedDiff.diff diff_in diff_doc in
    match diff_diff with
    | [] -> None
    | _  ->
      Some XedDiff.(doc_of_contexts (List.map (context diff_in) diff_diff))

  let run_all dir =
    List.(rev (fold_left (fun fails ((_, _, diff) as case) ->
      match run_test dir case with
      | None          -> fails
      | Some diff_doc -> (diff, diff_doc)::fails
    ) [] tests))
end

let run_all dir =
  let total = List.length Diff.tests in
  let diff_fails = Diff.run_all dir in
  let fails = List.length diff_fails in
  List.iter (fun (file, diff_doc) ->
    printf "test %s fails:\n%s\n\n" file (XedInput.to_string (prettify diff_doc))
  ) diff_fails;
  if fails = 0
  then (printf "ALL TESTS PASS\n"; exit 0)
  else (printf "FAILING: %d/%d test cases failed.\n" fails total; exit 1)

;;

run_all "test"
