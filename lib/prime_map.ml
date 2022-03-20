(* Copyright (C) 2013--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Prime

module type OrderedType = Map.OrderedType

module type S = sig
  include Map.S
  val app : 'a t -> key -> 'a option
  val pop : key -> 'a t -> ('a * 'a t) option
  val find_map : (key -> 'a -> 'b option) -> 'a t -> 'b option
  val search : (key -> 'a -> 'b option) -> 'a t -> 'b option [@@deprecated]
  val fold2t : (key -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  val map2t : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val mapi2t : (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val left_union : 'a t -> 'a t -> 'a t
  val split_union : (key -> 'a -> 'b -> 'c) ->
                    'a t -> 'b t -> 'a t * 'b t * 'c t
  val left_inter : 'a t -> 'b t -> 'a t
  val compl : 'a t -> 'a t -> 'a t
end

module Make (K : OrderedType) = struct
  include Map.Make (K)

  let app m k = try Some (find k m) with Not_found -> None

  let pop k m = try Some (find k m, remove k m) with Not_found -> None

  let find_map f m =
    let res = ref None in
    let capture k v =
      match f k v with
      | None -> false
      | Some r -> res := Some r; true in
    ignore (exists capture m);
    !res

  let search = find_map

  let fold2t f m0 m1 =
    fold (fun k v0 -> try f k v0 (find k m1) with Not_found -> ident) m0

  let map2t f =
    merge (fun _ xopt yopt ->
           match xopt, yopt with
           | Some x, Some y -> Some (f x y)
           | _, _ -> None)

  let mapi2t f =
    merge (fun k xopt yopt ->
           match xopt, yopt with
           | Some x, Some y -> Some (f k x y)
           | _, _ -> None)

  let left_union m0 m1 =
    merge (fun _ v0o v1o -> match v0o with None -> v1o | _ -> v0o) m0 m1

  let split_union f mA mB =
    let aux k a (mA, mB, mC) =
      try let b = find k mB in
        (mA, remove k mB, add k (f k a b) mC)
      with Not_found ->
        (add k a mA, mB, mC) in
    fold aux mA (empty, mB, empty)

  let left_inter m0 m1 = filter (fun k _ -> mem k m1) m0

  let compl mN mP = filter (fun k _ -> not (mem k mN)) mP
end
