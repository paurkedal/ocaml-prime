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
  val is_empty : 'a t -> bool
  val cardinal : 'a t -> int
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
  val cut : key -> 'a t -> 'a option * 'a t * 'a t
  val search : (key -> 'a -> 'b option) -> 'a t -> 'b option
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fmapi : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val merge : (key -> 'a option -> 'b option -> 'c option) ->
	      'a t -> 'b t -> 'c t
  val finter : (key -> 'a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
  val funion : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val fcompl : (key -> 'a -> 'b -> 'b option) -> 'a t -> 'b t -> 'b t
  val fpatch : (key -> 'a -> 'b option -> 'b option) -> 'a t -> 'b t -> 'b t
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

  let is_empty = function O -> true | _ -> false

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

  let rec add_min k e = function
    | O -> Y (1, k, e, O, O)
    | Y (n, kC, eC, mL, mR) -> bal_y (n + 1) kC eC (add_min k e mL) mR

  let rec add_max k e = function
    | O -> Y (1, k, e, O, O)
    | Y (n, kC, eC, mL, mR) -> bal_y (n + 1) kC eC mL (add_max k e mR)

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

  let rec glue k e mL mR =
    match mL, mR with
    | O, _ -> add_min k e mR
    | _, O -> add_max k e mL
    | Y (nL, kL, eL, mLL, mRL), Y (nR, kR, eR, mLR, mRR) ->
      if nL < 4 * nR then bal_y (nL + nR + 1) kR eR (glue k e mL mLR) mRR else
      if nR < 4 * nL then bal_y (nL + nR + 1) kL eL mLL (glue k e mRL mR) else
      Y (nL + nR + 1, k, e, mL, mR)

  let cat mL mR =
    match mL, mR with
    | O, m | m, O -> m
    | _, _ -> let k, e, mL' = pop_max mL in glue k e mL' mR

  let glue_opt k e_opt mL mR =
    match e_opt with None -> cat mL mR | Some e -> glue k e mL mR

  let rec cut kC = function
    | O -> (None, O, O)
    | Y (n, k, e, mL, mR) ->
      let c = K.compare kC k in
      if c = 0 then (Some e, mL, mR) else
      if c < 0 then
	let eC, mLL, mRL = cut kC mL in
	(eC, mLL, glue k e mRL mR)
      else
	let eC, mLR, mRR = cut kC mR in
	(eC, glue k e mL mLR, mRR)

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

  let rec for_all f = function
    | O -> true
    | Y (_, k, e, mL, mR) -> f k e && for_all f mL && for_all f mR

  let rec exists f = function
    | O -> false
    | Y (_, k, e, mL, mR) -> f k e || exists f mL || exists f mR

  let rec map f = function
    | O -> O
    | Y (n, k, e, mL, mR) -> Y (n, k, f e, map f mL, map f mR)

  let rec mapi f = function
    | O -> O
    | Y (n, k, e, mL, mR) -> Y (n, k, f k e, mapi f mL, mapi f mR)

  let rec fmapi f = function
    | O -> O
    | Y (n, k, e, mL, mR) ->
      let mL' = fmapi f mL and mR' = fmapi f mR in
      match f k e with
      | None -> cat mL' mR'
      | Some e' -> glue k e' mL' mR'

  let rec filter f = function
    | O -> O
    | Y (n, k, e, mL, mR) ->
      let mL' = filter f mL in
      let mR' = filter f mR in
      if f k e then glue k e mL' mR' else cat mL' mR'

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

  let rec merge f mA mB =
    match mA, mB with
    | O, O -> O
    | Y (nA, kA, eA, mLA, mRA), _ when nA > cardinal mB ->
      let eB_opt, mLB, mRB = cut kA mB in
      glue_opt kA (f kA (Some eA) eB_opt) (merge f mLA mLB) (merge f mRA mRB)
    | _, Y (nB, kB, eB, mLB, mRB) ->
      let eA_opt, mLA, mRA = cut kB mA in
      glue_opt kB (f kB eA_opt (Some eB)) (merge f mLA mLB) (merge f mRA mRB)
    | _ -> assert false

  let rec finter f mA mB =
    match mA, mB with
    | O, _ | _, O -> O
    | Y (nA, kA, eA, mLA, mRA), _ ->
      let eB_opt, mLB, mRB = cut kA mB in
      let mL = finter f mLA mLB in
      let mR = finter f mRA mRB in
      match eB_opt with
      | None -> cat mL mR
      | Some eB -> glue_opt kA (f kA eA eB) mL mR

  let rec funion f mA mB =
    match mA, mB with
    | O, m | m, O -> m
    | Y (nA, kA, eA, mLA, mRA), _ ->
      let eB_opt, mLB, mRB = cut kA mB in
      let mL = funion f mLA mLB in
      let mR = funion f mRA mRB in
      match eB_opt with
      | None -> glue kA eA mL mR
      | Some eB -> glue_opt kA (f kA eA eB) mL mR

  let rec fcompl f mA mB =
    match mA, mB with
    | O, _ -> mB
    | _, O -> O
    | _, Y (nB, kB, eB, mLB, mRB) ->
      let eA_opt, mLA, mRA = cut kB mA in
      let mL = fcompl f mLA mLB in
      let mR = fcompl f mRA mRB in
      match eA_opt with
      | None -> glue kB eB mL mR
      | Some eA -> glue_opt kB (f kB eA eB) mL mR

  let rec fpatch f mA mB =
    match mA, mB with
    | O, _ -> mB
    | _, O -> fmapi (fun k eA -> f k eA None) mA
    | _, Y (nB, k, eB, mLB, mRB) ->
      let eA_opt, mLA, mRA = cut k mA in
      let mL = fpatch f mLA mLB in
      let mR = fpatch f mRA mRB in
      match eA_opt with
      | None -> glue k eB mL mR
      | Some eA -> glue_opt k (f k eA (Some eB)) mL mR

  let split_union f mA mB =
    let aux k a (mA, mB, mC) =
      try let b = find k mB in
	(mA, remove k mB, add k (f k a b) mC)
      with Not_found ->
	(add k a mA, mB, mC) in
    fold aux mA (empty, mB, empty)
end
