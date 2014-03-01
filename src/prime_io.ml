(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

let with_file_in f fn =
  let ic = open_in fn in
  try
    let y = f ic in
    close_in ic; y
  with xc ->
    close_in ic; raise xc

let with_file_out f fn =
  let oc = open_out fn in
  try
    let y = f oc in
    close_out oc; y
  with xc ->
    close_out oc;
    (try Sys.remove fn with _ -> ());
    raise xc

let with1_file_in f fn x =
  let ic = open_in fn in
  try let y = f ic x in close_in ic; y
  with xc -> close_in ic; raise xc

let with1_file_out f fn x =
  let oc = open_out fn in
  try let y = f oc x in close_out oc; y
  with xc -> close_out oc; raise xc

let rec fold_input f ic acc =
  try fold_input f ic (f ic acc)
  with End_of_file -> acc

let rec iter_input f ic =
  try f ic; iter_input f ic
  with End_of_file -> ()
