(* Copyright (C) 2013--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

let some x = Some x
let get = function None -> invalid_arg "Prime_option.get" | Some a -> a
let get_or d = function None -> d | Some a -> a
let get_else h = function None -> h () | Some a -> a
let found f = try Some (f ()) with Not_found -> None
let flatten = function Some (Some x) -> Some x | _ -> None

let return x = Some x
let (>>=) m f = match m with Some x -> f x | None -> None

let fold f = function None -> fun x -> x | Some x -> f x
let iter f = function None -> () | Some x -> f x
let for_all f = function None -> true | Some x -> f x
let exists f = function None -> false | Some x -> f x
let search f = function None -> None | Some x -> f x
let map f = function None -> None | Some x -> Some (f x)
let fmap f = function None -> None | Some x -> f x
let filter f = function Some x when f x -> Some x | _ -> None

let inter f xo yo =
  match xo, yo with
  | None, _ | _, None -> None
  | Some x, Some y -> Some (f x y)

let union f xo yo =
  match xo, yo with
  | None, zo | zo, None -> zo
  | Some x, Some y -> Some (f x y)

let compl f xo yo =
  match xo, yo with
  | None, _ -> yo
  | _, None -> None
  | Some x, Some y -> Some (f x y)

let finter f xo yo =
  match xo, yo with
  | None, _ | _, None -> None
  | Some x, Some y -> f x y

let funion f xo yo =
  match xo, yo with
  | None, zo | zo, None -> zo
  | Some x, Some y -> f x y

let fcompl f xo yo =
  match xo, yo with
  | None, _ -> yo
  | _, None -> None
  | Some x, Some y -> f x y
