(* Copyright (C) 2013--2014  Petter Urkedal <paurkedal@gmail.com>
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
  val locate : key -> 'a t -> bool * int
  val get_o : int -> 'a t -> 'a option
  val get_e : int -> 'a t -> 'a
  val get_binding : int -> 'a t -> key * 'a
  val min_binding : 'a t -> key * 'a
  val max_binding : 'a t -> key * 'a
  val pred_binding_e : 'a t -> key -> key * 'a
  val succ_binding_e : 'a t -> key -> key * 'a
  val add : key -> 'a -> 'a t -> 'a t
  val pop_min : 'a t -> key * 'a * 'a t
  val pop_max : 'a t -> key * 'a * 'a t
  val remove : key -> 'a t -> 'a t
  val cardinal : 'a t -> int
  val search : (key -> 'a -> 'b option) -> 'a t -> 'b option
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val split_union : (key -> 'a -> 'b -> 'c) ->
		    'a t -> 'b t -> 'a t * 'b t * 'c t

  val card : 'a t -> int
end

exception Keep

module Make (K : OrderedType) = struct
  type key = K.t
  type 'a t = O | Y of int * key * 'a * 'a t * 'a t

  type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

  let rec cons_enum m q =
    match m with
    | O -> q
    | Y (n, k, e, mL, mR) -> cons_enum mL (More (k, e, mR, q))

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

  let cardinal = function O -> 0 | Y (n, _, _, _, _) -> n
  let card = cardinal

  let rec locate' i k = function
    | O -> false, i
    | Y (n, kC, _, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then locate' i k mL else
      if o > 0 then locate' (i + cardinal mL + 1) k mR else
      true, i + cardinal mL
  let locate k = locate' 0 k

  let rec get_o i = function
    | O -> None
    | Y (n, kC, eC, mL, mR) ->
      let nL = cardinal mL in
      if i < nL then get_o i mL else
      if i > nL then get_o (i - nL - 1) mR else
      Some eC

  let rec get_e i = function
    | O -> invalid_arg "Prime_enummap.get_e: Index out of bounds."
    | Y (n, kC, eC, mL, mR) ->
      let nL = cardinal mL in
      if i < nL then get_e i mL else
      if i > nL then get_e (i - nL - 1) mR else
      eC

  let rec get_binding i = function
    | O -> invalid_arg "Prime_enummap.get_binding: Index out of bounds."
    | Y (n, kC, eC, mL, mR) ->
      let nL = cardinal mL in
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

  let rec pred_binding_e = function
    | O -> raise Not_found
    | Y (_, kC, eC, mL, mR) -> fun k ->
      let o = K.compare k kC in
      if o < 0 then pred_binding_e mL k else
      if o > 0 then try pred_binding_e mR k with Not_found -> (kC, eC) else
      max_binding mL

  let rec succ_binding_e = function
    | O -> raise Not_found
    | Y (_, kC, eC, mL, mR) -> fun k ->
      let o = K.compare k kC in
      if o < 0 then try succ_binding_e mL k with Not_found -> (kC, eC) else
      if o > 0 then succ_binding_e mR k else
      min_binding mR

  let bal_y n kC eC mL mR =
    match mL, mR with
    | _, Y (nR, kCR, eCR, mLR, mRR) when cardinal mL < 4 * nR ->
      Y (n, kCR, eCR, Y (cardinal mL + cardinal mLR + 1, kC, eC, mL, mLR), mRR)
    | Y (nL, kCL, eCL, mLL, mRL), _ when cardinal mR < 4 * nL ->
      Y (n, kCL, eCL, mLL, Y (cardinal mRL + cardinal mR + 1, kC, eC, mRL, mR))
    | _, _ ->
      Y (n, kC, eC, mL, mR)

  let rec add' k e = function
    | O -> 1, Y (1, k, e, O, O)
    | Y (n, kC, eC, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then let dn, mL' = add' k e mL in
		    dn, bal_y (n + dn) kC eC mL' mR else
      if o > 0 then let dn, mR' = add' k e mR in
		    dn, bal_y (n + dn) kC eC mL mR' else
      0, Y (n, k, e, mL, mR)
  let add k e m = snd (add' k e m)

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
      if cardinal mL > cardinal mR
	then let kC', eC', mL' = pop_max mL in Y (n - 1, kC', eC', mL', mR)
	else let kC', eC', mR' = pop_min mR in Y (n - 1, kC', eC', mL, mR')
  let remove k m = try remove' k m with Keep -> m

  let rec search f = function
    | O -> None
    | Y (_, kC, eC, mL, mR) ->
      begin match search f mL with
      | Some _ as r -> r
      | None ->
	begin match f kC eC with
	| Some _ as r -> r
	| None -> search f mR
	end
      end

  let rec iter f = function
    | O -> ()
    | Y (_, kC, eC, mL, mR) -> iter f mL; f kC eC; iter f mR

  let rec fold f = function
    | O -> ident
    | Y (_, kC, eC, mL, mR) -> fold f mR *< f kC eC *< fold f mL

  let compare f mA mB =
    let rec aux = function
      | End, End -> 0
      | End, More _ -> -1
      | More _, End -> 1
      | More (kA, eA, mA, qA), More (kB, eB, mB, qB) ->
	let ck = K.compare kA kB in if ck <> 0 then ck else
	let cv = f eA eB in         if cv <> 0 then cv else
	aux (cons_enum mA qA, cons_enum mB qB) in
    aux (cons_enum mA End, cons_enum mB End)

  let equal f mA mB =
    let rec aux = function
      | End, End -> true
      | End, More _ | More _, End -> false
      | More (kA, eA, mA, qA), More (kB, eB, mB, qB) ->
	K.compare kA kB = 0 && f eA eB &&
	aux (cons_enum mA qA, cons_enum mB qB) in
    aux (cons_enum mA End, cons_enum mB End)

  let split_union f mA mB =
    let aux k a (mA, mB, mC) =
      try let b = find k mB in
	(mA, remove k mB, add k (f k a b) mC)
      with Not_found ->
	(add k a mA, mB, mC) in
    fold aux mA (empty, mB, empty)
end
