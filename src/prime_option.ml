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

type 'a t = 'a option

let fold f = function None -> fun x -> x | Some x -> f x
let iter f = function None -> () | Some x -> f x
let for_all f = function None -> true | Some x -> f x
let exists f = function None -> false | Some x -> f x
let map f = function None -> None | Some x -> Some (f x)
let filter f = function Some x when f x -> Some x | _ -> None
