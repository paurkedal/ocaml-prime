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

open Unprime_list

include Prime

let ( |?>. ) xo f = match xo with None -> None | Some x -> Some (f x)
let ( |?>.. ) xo f acc = match xo with None -> acc | Some x -> f x acc
let ( |?>! ) xo f = match xo with None -> () | Some x -> f x
let ( |?>= ) xo f = match xo with None -> None | Some x -> f x
let ( |@>. ) xs f = List.map f xs
let ( |@>.. ) xs f = List.fold f xs
let ( |@>! ) xs f = List.iter f xs
let ( |@>= ) xs f = List.rev (List.fold (fun x -> List.rev_append (f x)) xs [])
