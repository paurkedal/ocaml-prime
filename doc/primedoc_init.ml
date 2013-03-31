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

open Primedoc_html

let set_format_html () =
  Odoc_args.extend_html_generator (module Generator : Odoc_gen.Html_functor)

let () =
  Odoc_args.add_option
    ( "-copy-css-style",
      Arg.String (fun s -> opt_copy_css_style := Some (s, false)),
      "FILE Use a copy of FILE as the CSS stylesheet." );
  Odoc_args.add_option
    ( "-link-css-style",
      Arg.String (fun s -> opt_copy_css_style := Some (s, true)),
      "LINK Use a symbolic link to LINK as the CSS stylesheet." );
  Odoc_args.add_option
    ( "-html", Arg.Unit set_format_html,
      " Generate HTML documentation using Odocgen_prime customizations." );
