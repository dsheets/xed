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

open Cmdliner

open XedCli

let xml_of_oc oc =
  XedInput.xmlm (Xmlm.make_input (`Channel oc))

let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "AUTHORS";
  `P "David Sheets <sheets@alum.mit.edu>"; `Noblank;
  `S "BUGS";
  `P "Browse and report new issues at"; `Noblank;
  `P "<https://github.com/dsheets/xed/issues>.";
]

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

let diff_cmd =
  let doc = "Compare a base XML file to another XML file" in
  let info = Term.info ~doc "diff" in
  (Term.(
    pure (fun a b ->
      let base_oc = open_in a in
      let base = XedInput.buffer (xml_of_oc base_oc) in
      let branch_oc = open_in b in
      let branch = xml_of_oc branch_oc in
      let diff = XedDiff.diff base branch in
      close_in base_oc;
      close_in branch_oc;
      let contexts = List.map (XedDiff.context base) diff in
      let diff_doc = XedDiff.doc_of_contexts contexts in
      print_endline (XedInput.to_string (prettify diff_doc))
    ) $ base $ branch), info)

let xed_cmd = 
  let doc = "structurally edit XML documents" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,xed) consists of a number of useful functions on XML documents.";
  ] @ help_sections
  in
  let exec_name = Filename.basename Sys.argv.(0) in
  let no_cmd_err _ = `Error (true, "No command specified.") in
  Term.(ret (pure no_cmd_err $ common),
        info exec_name ~version:"0.1.0" ~sdocs:global_option_section
          ~doc ~man)

;;

match Term.eval_choice xed_cmd [
  diff_cmd;
] with
| `Ok () | `Version | `Help -> exit 0
| `Error _ -> exit 1
