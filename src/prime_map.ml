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

open Prime

module type OrderedType = Map.OrderedType

module type S = sig
  include Map.S
  val fold2t : (key -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  val left_union : 'a t -> 'a t -> 'a t
  val left_inter : 'a t -> 'a t -> 'a t
  val compl : 'a t -> 'a t -> 'a t
end

module Make (K : OrderedType) = struct
  include Map.Make (K)

  let fold2t f m0 m1 =
    fold (fun k v0 -> try f k v0 (find k m1) with Not_found -> ident) m0

  let left_union m0 m1 =
    merge (fun _ v0o v1o -> match v0o with None -> v1o | _ -> v0o) m0 m1

  let left_inter m0 m1 = filter (fun k _ -> mem k m1) m0

  let compl mN mP = filter (fun k _ -> not (mem k mN)) mP
end
