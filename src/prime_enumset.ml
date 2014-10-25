(* Copyright (C) 2013--2014  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or (at your
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

open Unprime

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val empty : t
  val singleton : elt -> t
  val contains : elt -> t -> bool
  val locate : elt -> t -> bool * int
  val get : int -> t -> elt
  val min_elt : t -> elt
  val max_elt : t -> elt
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val pop_min : t -> elt * t
  val pop_max : t -> elt * t
  val card : t -> int
  val search : (elt -> 'a option) -> t -> 'a option
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (elt -> unit) -> t -> unit
end

exception Keep

module Make (E : OrderedType) = struct
  type elt = E.t
  type t = O | Y of int * elt * t * t

  let empty = O
  let singleton e = Y (1, e, O, O)

  let rec contains e = function
    | O -> false
    | Y (_, eC, sL, sR) ->
      let o = E.compare e eC in
      if o < 0 then contains e sL else
      if o > 0 then contains e sR else
      true

  let card = function O -> 0 | Y (n, _, _, _) -> n

  let rec locate' i e = function
    | O -> false, i
    | Y (n, eC, sL, sR) ->
      let o = E.compare e eC in
      if o < 0 then locate' i e sL else
      if o > 0 then locate' (i + card sL + 1) e sR else
      true, i + card sL
  let locate = locate' 0

  let rec get i = function
    | O -> invalid_arg "Prime_enumset.get: Index out of bounds."
    | Y (n, eC, sL, sR) ->
      let nL = card sL in
      if i < nL then get i sL else
      if i > nL then get (i - nL - 1) sR else
      eC

  let rec min_elt = function
    | O -> raise Not_found
    | Y (_, eC, O, _) -> eC
    | Y (_, eC, sL, _) -> min_elt sL

  let rec max_elt = function
    | O -> raise Not_found
    | Y (_, eC, _, O) -> eC
    | Y (_, eC, _, sR) -> max_elt sR

  let bal_y n eC sL sR =
    match sL, sR with
    | _, Y (nR, eCR, sLR, sRR) when card sL < 4 * nR ->
      Y (n, eCR, Y (card sL + card sLR + 1, eC, sL, sLR), sRR)
    | Y (nL, eCL, sLL, sRL), _ when card sR < 4 * nL ->
      Y (n, eCL, sLL, Y (card sRL + card sR + 1, eC, sRL, sR))
    | _, _ ->
      Y (n, eC, sL, sR)

  let rec add' e = function
    | O -> Y (1, e, O, O)
    | Y (n, eC, sL, sR) ->
      let o = E.compare e eC in
      if o < 0 then bal_y (n + 1) eC (add' e sL) sR else
      if o > 0 then bal_y (n + 1) eC sL (add' e sR) else
      raise Keep
  let add e s = try add' e s with Keep -> s

  let rec pop_min = function
    | O -> raise Not_found
    | Y (n, eC, O, sR) -> eC, sR
    | Y (n, eC, sL, sR) ->
      let e', sL' = pop_min sL in e', bal_y (n - 1) eC sL' sR

  let rec pop_max = function
    | O -> raise Not_found
    | Y (n, eC, sL, O) -> eC, sL
    | Y (n, eC, sL, sR) ->
      let e', sR' = pop_max sR in e', bal_y (n - 1) eC sL sR'

  let rec remove' e = function
    | O -> raise Keep
    | Y (n, eC, sL, sR) ->
      let o = E.compare e eC in
      if o < 0 then bal_y (n - 1) eC (remove' e sL) sR else
      if o > 0 then bal_y (n - 1) eC sL (remove' e sR) else
      if n = 1 then O else
      if card sL > card sR
	then let eC', sL' = pop_max sL in Y (n - 1, eC', sL', sR)
	else let eC', sR' = pop_min sR in Y (n - 1, eC', sL, sR')
  let remove e s = try remove' e s with Keep -> s

  let rec search f = function
    | O -> None
    | Y (_, eC, sL, sR) ->
      begin match search f sL with
      | Some _ as r -> r
      | None ->
	begin match f eC with
	| Some _ as r -> r
	| None -> search f sR
	end
      end

  let rec iter f = function
    | O -> ()
    | Y (_, eC, sL, sR) -> iter f sL; f eC; iter f sR

  let rec fold f = function
    | O -> ident
    | Y (_, eC, sL, sR) -> fold f sR *< f eC *< fold f sL

end
