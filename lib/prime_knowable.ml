(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

type (+_, _) t =
  | Unknown : ('a, [> `Unknown]) t
  | Known : 'a -> ('a, [> `Known]) t

let get : (_, [`Known]) t -> _ = function Known x -> x

let get_opt (type k) : ('a, k) t -> 'a option = function
 | Unknown -> None
 | Known x -> Some x

let of_option : 'a option -> ('a, _) t = function
 | None -> Unknown
 | Some x -> Known x

let inquire (type k) : (_, k) t -> _ = function
 | Known _ as k -> Some k
 | Unknown -> None
