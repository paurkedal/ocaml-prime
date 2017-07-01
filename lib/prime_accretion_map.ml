(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

include Prime_accretion_map_intf

module type Monoid_ = sig
  type 'a t_
  type 'a generator_
  include MonoidG1
     with type 'a t := 'a t_
      and type 'a generator := 'a generator_
end

module Make_ (Key : Map.OrderedType) (Elt : Monoid_) = struct
  type key = Key.t
  type 'a elt_ = 'a Elt.generator_
  type 'a result_ = 'a Elt.t_
  type 'a t_ = O | Y of int * 'a result_ * key * 'a elt_ * 'a t_ * 'a t_

  let empty = O
  let singleton k e = Y (1, Elt.of_generator e, k, e, O, O)
  let is_empty = function O -> true | Y _ -> false
  let cardinal = function O -> 0 | Y (n, _, _, _, _, _) -> n
  let result = function O -> Elt.empty | Y (_, z, _, _, _, _) -> z
  let head = function O -> 0, Elt.empty | Y (n, z, _, _, _, _) -> n, z

  let aY kC eC mL mR =
    let nL, zL = head mL in
    let nR, zR = head mR in
    let zC = Elt.cat zL (Elt.cat (Elt.of_generator eC) zR) in
    Y (nL + 1 + nR, zC, kC, eC, mL, mR)

  let bY kC eC mL mR =
    match mL, mR with
    | _, Y (nR, _, kR, eR, mLR, mRR) when 4 * cardinal mL < nR ->
      aY kR eR (aY kC eC mL mLR) mRR
    | Y (nL, _, kL, eL, mLL, mRL), _ when 4 * cardinal mR < nL ->
      aY kL eL mLL (aY kC eC mRL mR)
    | _, _ ->
      aY kC eC mL mR

  let rec app m k =
    match m with
    | O -> None
    | Y (_, _, kC, eC, mL, mR) ->
      let o = Key.compare k kC in
      if o < 0 then app mL k else
      if o > 0 then app mR k else
      Some eC

  let rec find k = function
    | O -> raise Not_found
    | Y (_, _, kC, eC, mL, mR) ->
      let o = Key.compare k kC in
      if o < 0 then find k mL else
      if o > 0 then find k mR else
      eC

  let rec add k e = function
    | O -> Y (1, Elt.of_generator e, k, e, O, O)
    | Y (_, _, kC, eC, mL, mR) ->
      let o = Key.compare k kC in
      if o < 0 then bY kC eC (add k e mL) mR else
      if o > 0 then bY kC eC mL (add k e mR) else
      aY k e mL mR

  let rec pop_min = function
    | O -> invalid_arg "Prime_accretion_map.pop_min"
    | Y (_, _, kC, eC, O, mR) -> kC, eC, mR
    | Y (_, _, kC, eC, mL, mR) -> let k, e, mL' = pop_min mL in
                                  k, e, aY kC eC mL' mR

  let rec pop_max = function
    | O -> invalid_arg "Prime_accretion_map.pop_max"
    | Y (_, _, kC, eC, mL, O) -> kC, eC, mL
    | Y (_, _, kC, eC, mL, mR) -> let k, e, mR' = pop_max mR in
                                  k, e, aY kC eC mL mR'

  let rec remove' k = function
    | O -> raise Not_found
    | Y (n, _zC, kC, eC, mL, mR) ->
      let o = Key.compare k kC in
      if o < 0 then bY kC eC (remove' k mL) mR else
      if o > 0 then bY kC eC mL (remove' k mR) else
      if n = 1 then O else
      if cardinal mL > cardinal mR
      then let kC', eC', mL' = pop_max mL in aY kC' eC' mL' mR
      else let kC', eC', mR' = pop_min mR in aY kC' eC' mL mR'
  let remove k m = try remove' k m with Not_found -> m

  let bindings m =
    let rec push acc = function
      | O -> acc
      | Y (_, _, k, e, mL, mR) -> push ((k, e) :: push acc mR) mL in
    push [] m

  let rec fold f = function
    | O -> fun acc -> acc
    | Y (_, _, k, e, mL, mR) -> fun acc ->
      acc |> fold f mL |> f k e |> fold f mR

  let rec iter f = function
    | O -> ()
    | Y (_, _, k, e, mL, mR) -> iter f mL; f k e; iter f mR

  let rec search f = function
    | O -> None
    | Y (_, _, k, e, mL, mR) ->
      match search f mL with
      | Some _ as r -> r
      | None -> (match f k e with Some _ as r -> r | None -> search f mR)

  let rec for_all f = function
    | O -> true
    | Y (_, _, k, e, mL, mR) -> for_all f mL && f k e && for_all f mR

  let rec exists f = function
    | O -> false
    | Y (_, _, k, e, mL, mR) -> exists f mL || f k e || exists f mR
end

module Make1 (Key : Map.OrderedType) (Elt : Monoid1) = struct
  include Make_ (Key)
    (struct
      include Elt
      type 'a t_ = 'a t
      type 'a generator_ = 'a t
      let of_generator x = x
    end)
  type 'a elt = 'a elt_
  type 'a result = 'a result_
  type 'a t = 'a t_
end

module Make (Key : Map.OrderedType) (Elt : Monoid) = struct
  include Make_ (Key)
    (struct
      include Elt
      type 'a t_ = t
      type 'a generator_ = t
      let of_generator x = x
    end)
  type elt = unit elt_
  type result = unit result_
  type t = unit t_
end

module MakeG1 (Key : Map.OrderedType) (Elt : MonoidG1) = struct
  include Make_ (Key)
    (struct
      include Elt
      type 'a t_ = 'a t
      type 'a generator_ = 'a generator
    end)
  type 'a elt = 'a elt_
  type 'a result = 'a result_
  type 'a t = 'a t_
end

module MakeG (Key : Map.OrderedType) (Elt : MonoidG) = struct
  include Make_ (Key)
    (struct
      include Elt
      type 'a t_ = t
      type 'a generator_ = generator
    end)
  type elt = unit elt_
  type result = unit result_
  type t = unit t_
end
