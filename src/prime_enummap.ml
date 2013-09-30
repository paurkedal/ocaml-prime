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

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type 'a t

  val empty : 'a t
  val singleton : key -> 'a -> 'a t
  val contains : key -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val locate : key -> 'a t -> int option
  val get_binding : int -> 'a t -> key * 'a
  val min_binding : 'a t -> key * 'a
  val max_binding : 'a t -> key * 'a
  val add : key -> 'a -> 'a t -> 'a t
  val pop_min : 'a t -> key * 'a * 'a t
  val pop_max : 'a t -> key * 'a * 'a t
  val remove : key -> 'a t -> 'a t
  val card : 'a t -> int
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

exception Keep

module Make (K : OrderedType) = struct
  type key = K.t
  type 'a t = O | Y of int * key * 'a * 'a t * 'a t

  let empty = O
  let singleton k e = Y (1, k, e, O, O)

  let rec contains k = function
    | O -> false
    | Y (_, kC, _, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then contains k mL else
      if o > 0 then contains k mR else
      true

  let rec find k = function
    | O -> raise Not_found
    | Y (_, kC, eC, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then find k mL else
      if o > 0 then find k mR else
      eC

  let card = function O -> 0 | Y (n, _, _, _, _) -> n

  let rec locate' i k = function
    | O -> None
    | Y (n, kC, _, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then locate' i k mL else
      if o > 0 then locate' (i + card mL + 1) k mR else
      Some (i + card mL)
  let locate k = locate' 0 k

  let rec get_binding i = function
    | O -> invalid_arg "Prime_enummap.get_binding: Index out of bounds."
    | Y (n, kC, eC, mL, mR) ->
      let nL = card mL in
      if i < nL then get_binding i mL else
      if i > nL then get_binding (i - nL - 1) mR else
      (kC, eC)

  let rec min_binding = function
    | O -> raise Not_found
    | Y (_, kC, eC, O, _) -> kC, eC
    | Y (_, _, _, mL, _) -> min_binding mL

  let rec max_binding = function
    | O -> raise Not_found
    | Y (_, kC, eC, _, O) -> kC, eC
    | Y (_, _, _, _, mR) -> max_binding mR

  let bal_y n kC eC mL mR =
    match mL, mR with
    | _, Y (nR, kCR, eCR, mLR, mRR) when card mL < 4 * nR ->
      Y (n, kCR, eCR, Y (card mL + card mLR + 1, kC, eC, mL, mLR), mRR)
    | Y (nL, kCL, eCL, mLL, mRL), _ when card mR < 4 * nL ->
      Y (n, kCL, eCL, mLL, Y (card mRL + card mR + 1, kC, eC, mRL, mR))
    | _, _ ->
      Y (n, kC, eC, mL, mR)

  let rec add' k e = function
    | O -> Y (1, k, e, O, O)
    | Y (n, kC, eC, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then bal_y (n + 1) kC eC (add' k e mL) mR else
      if o > 0 then bal_y (n + 1) kC eC mL (add' k e mR) else
      raise Keep
  let add k e m = try add' k e m with Keep -> m

  let rec pop_min = function
    | O -> raise Not_found
    | Y (n, kC, eC, O, mR) -> kC, eC, mR
    | Y (n, kC, eC, mL, mR) ->
      let k', e', mL' = pop_min mL in k', e', bal_y (n - 1) kC eC mL' mR

  let rec pop_max = function
    | O -> raise Not_found
    | Y (n, kC, eC, mL, O) -> kC, eC, mL
    | Y (n, kC, eC, mL, mR) ->
      let k', e', mR' = pop_max mR in k', e', bal_y (n - 1) kC eC mL mR'

  let rec remove' k = function
    | O -> raise Keep
    | Y (n, kC, eC, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then bal_y (n - 1) kC eC (remove' k mL) mR else
      if o > 0 then bal_y (n - 1) kC eC mL (remove' k mR) else
      if n = 1 then O else
      if card mL > card mR
	then let kC', eC', mL' = pop_max mL in Y (n - 1, kC', eC', mL', mR)
	else let kC', eC', mR' = pop_min mR in Y (n - 1, kC', eC', mL, mR')
  let remove k m = try remove' k m with Keep -> m

  let rec iter f = function
    | O -> ()
    | Y (_, kC, eC, mL, mR) -> iter f mL; f kC eC; iter f mR

  let rec fold f = function
    | O -> ident
    | Y (_, kC, eC, mL, mR) -> fold f mR *< f kC eC *< fold f mL
end
