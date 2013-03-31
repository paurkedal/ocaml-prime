(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Odoc_info

let opt_copy_css_style = ref None

let copy_file src dst =
  let ich = open_in src in
  ( try
      let och = open_out dst in
      ( try
	  let buflen = 4096 in
	  let buf = String.create buflen in
	  let rec loop () =
	    let n = input ich buf 0 buflen in
	    if n > 0 then (output och buf 0 n; loop ()) in
	  loop ()
	with xc -> close_out och; close_in ich; raise xc );
      close_out och
    with xc -> close_in ich; raise xc );
  close_in ich

module Generator (G : Odoc_html.Html_generator) = struct
  class html = object (self)
    inherit G.html as base

    method init_style =
      begin match !opt_copy_css_style with
      | None -> ()
      | Some (src, symbolic) ->
	let dst = Filename.concat !Global.target_dir style_file in
	if not (Sys.file_exists dst) then
	  (if symbolic then Unix.symlink src dst else copy_file src dst)
      end;
      base#init_style

    method private html_of_notice t =
      let b = Buffer.create 128 in
      Buffer.add_string b "<p><span class=\"notice\">Notice.</span> ";
      self#html_of_text b t;
      Buffer.add_string b "</p>";
      Buffer.contents b

    initializer
      tag_functions <- ("notice", self#html_of_notice) :: tag_functions
  end
end
