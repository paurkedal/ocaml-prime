(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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

module Deck = struct

  type 'a t =
    | Empty
    | Front of 'a * 'a t
    | Back  of 'a * 'a t

  let rec reduce f = function
    | Empty -> invalid_arg "Deck.reduce"
    | Front (x, Empty) | Back (x, Empty) -> x
    | Front (x, xs) -> f x (reduce f xs)
    | Back  (x, xs) -> f (reduce f xs) x

  let rec iter f = function
    | Empty -> ()
    | Front (x, xs) -> f x; iter f xs
    | Back  (x, xs) -> iter f xs; f x
end

module type S = sig
  type elt
  type t

  val empty : t

  val is_empty : t -> bool

  val add : elt -> t -> t

  val remove : elt -> t -> t

  val find_min : t -> elt

  val remove_min : t -> t

  val gc : t -> t
end

module Make (Elt : Set.OrderedType) = struct
  type elt = Elt.t

  type t =
    | O
    | P of Elt.t * t Deck.t
    | N of Elt.t * t Deck.t

  let empty = O

  let is_empty = function O -> true | _ -> false

  let rec merge hA hB =
    match hA, hB with
    | O, N (e, hs) | N (e, hs), O -> reduce_merge hs
    | O, h | h, O -> h
    | P (eA, hsA), P (eB, hsB) ->
      let c = Elt.compare eA eB in
      if c < 0 then P (eA, Deck.Back  (hB, hsA))
	       else P (eB, Deck.Front (hA, hsB))
    | P (eA, hsA), N (eB, hsB) ->
      let c = Elt.compare eA eB in
      if c < 0 then P (eA, Deck.Back (hB, hsA)) else
      if c > 0 then merge hA (reduce_merge hsB) else
      P (eA, Deck.Back (reduce_merge hsB, hsA))
    | N (eA, hsA), P (eB, hsB) ->
      let c = Elt.compare eA eB in
      if c < 0 then merge (reduce_merge hsA) hB else
      if c > 0 then P (eB, Deck.Front (hA, hsB)) else
      merge (reduce_merge hsA) (reduce_merge hsB)
    | N (eA, hsA), N (eB, hsB) ->
      merge (reduce_merge hsA) (reduce_merge hsB)

  and reduce_merge = function
    | Deck.Empty -> O
    | Deck.Front (N (e, hs), Deck.Empty)
    | Deck.Back  (N (e, hs), Deck.Empty) -> reduce_merge hs
    | hs -> Deck.reduce merge hs

  let add e = merge (P (e, Deck.Empty))
  let remove e = merge (N (e, Deck.Empty))

  let find_min = function
    | O -> raise Not_found
    | P (e, _) -> e
    | N _ -> assert false

  let rec remove_min = function
    | O -> raise Not_found
    | P (_, Deck.Empty) -> O
    | P (_, hs) -> reduce_merge hs
    | N _ -> assert false

  let gc h =
    let rec loop h h' =
      if is_empty h then h' else
      loop (remove_min h) (add (find_min h) h') in
    loop h O
end
