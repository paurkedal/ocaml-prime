(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

module type RETRACTABLE = sig
  type key
  type t
  val compare : t -> t -> int
  val compare_key : key -> t -> int
end

module type S = sig
  type key
  type elt
  type t
  val empty : t
  val singleton : elt -> t
  val is_empty : t -> bool
  val cardinal : t -> int
  val elements : t -> elt list
  val mem : key -> t -> bool
  val mem_elt : elt -> t -> bool
  val app : t -> key -> elt option
  val find : key -> t -> elt
  val locate : key -> t -> bool * int
  val locate_elt : elt -> t -> bool * int
  val get : t -> int -> elt
  val min_exn : t -> elt
  val max_exn : t -> elt
  val pred_exn : t -> key -> elt
  val succ_exn : t -> key -> elt
  val elt_pred_exn : t -> elt -> elt
  val elt_succ_exn : t -> elt -> elt
  val add : elt -> t -> t
  val pop : key -> t -> (elt * t) option
  val pop_min_exn : t -> elt * t
  val pop_max_exn : t -> elt * t
  val remove : key -> t -> t
  val cut : key -> t -> elt option * t * t
  val search : (elt -> 'a option) -> t -> 'a option
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_rev : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (elt -> unit) -> t -> unit
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val finter : (elt -> elt -> elt option) -> t -> t -> t
  val funion : (elt -> elt -> elt option) -> t -> t -> t
  val fcompl : (elt -> elt -> elt option) -> t -> t -> t

end

exception Keep

module Make (Elt : RETRACTABLE) = struct

  type key = Elt.key
  type elt = Elt.t
  type t = O | Y of int * elt * t * t

  type enumeration = End | More of elt * t * enumeration

  let rec cons_enum c q =
    match c with
    | O -> q
    | Y (_, e, cL, cR) -> cons_enum cL (More (e, cR, q))

  let empty = O
  let singleton e = Y (1, e, O, O)

  let is_empty = function O -> true | _ -> false

  let cardinal = function O -> 0 | Y (n, _, _, _) -> n

  let rec mem k = function
    | O -> false
    | Y (_, eC, cL, cR) ->
      let o = Elt.compare_key k eC in
      if o < 0 then mem k cL else
      if o > 0 then mem k cR else
      true

  let rec mem_elt e = function
    | O -> false
    | Y (_, eC, cL, cR) ->
      let o = Elt.compare e eC in
      if o < 0 then mem_elt e cL else
      if o > 0 then mem_elt e cR else
      true

  let rec app m k =
    match m with
    | O -> None
    | Y (_, eC, cL, cR) ->
      let o = Elt.compare_key k eC in
      if o < 0 then app cL k else
      if o > 0 then app cR k else
      Some eC

  let rec find k = function
    | O -> raise Not_found
    | Y (_, eC, cL, cR) ->
      let o = Elt.compare_key k eC in
      if o < 0 then find k cL else
      if o > 0 then find k cR else
      eC

  let rec locate' i k = function
    | O -> false, i
    | Y (_n, eC, cL, cR) ->
      let o = Elt.compare_key k eC in
      if o < 0 then locate' i k cL else
      if o > 0 then locate' (i + cardinal cL + 1) k cR else
      true, i + cardinal cL
  let locate = locate' 0

  let rec locate_elt' i e' = function
    | O -> false, i
    | Y (_n, eC, cL, cR) ->
      let o = Elt.compare e' eC in
      if o < 0 then locate_elt' i e' cL else
      if o > 0 then locate_elt' (i + cardinal cL + 1) e' cR else
      true, i + cardinal cL
  let locate_elt = locate_elt' 0


  let rec get m i =
    match m with
    | O -> invalid_arg "Prime_collection.get: Index out of bounds."
    | Y (_n, eC, cL, cR) ->
      let nL = cardinal cL in
      if i < nL then get cL i else
      if i > nL then get cR (i - nL - 1) else
      eC

  let rec min_exn = function
    | O -> raise Not_found
    | Y (_, eC, O, _) -> eC
    | Y (_, _, cL, _) -> min_exn cL

  let rec max_exn = function
    | O -> raise Not_found
    | Y (_, eC, _, O) -> eC
    | Y (_, _, _, cR) -> max_exn cR

  let rec pred_exn = function
    | O -> fun _ -> raise Not_found
    | Y (_, eC, cL, cR) -> fun k ->
      let o = Elt.compare_key k eC in
      if o < 0 then pred_exn cL k else
      if o > 0 then try pred_exn cR k with Not_found -> eC else
      max_exn cL

  let rec succ_exn = function
    | O -> fun _ -> raise Not_found
    | Y (_, eC, cL, cR) -> fun k ->
      let o = Elt.compare_key k eC in
      if o < 0 then try succ_exn cL k with Not_found -> eC else
      if o > 0 then succ_exn cR k else
      min_exn cR

  let rec elt_pred_exn = function
    | O -> fun _ -> raise Not_found
    | Y (_, eC, cL, cR) -> fun e ->
      let o = Elt.compare e eC in
      if o < 0 then elt_pred_exn cL e else
      if o > 0 then try elt_pred_exn cR e with Not_found -> eC else
      max_exn cL

  let rec elt_succ_exn = function
    | O -> fun _ -> raise Not_found
    | Y (_, eC, cL, cR) -> fun e ->
      let o = Elt.compare e eC in
      if o < 0 then try elt_succ_exn cL e with Not_found -> eC else
      if o > 0 then elt_succ_exn cR e else
      min_exn cR

  let bal_y n eC cL cR =
    match cL, cR with
    | _, Y (nR, eCR, cLR, cRR) when 4 * cardinal cL < nR ->
      Y (n, eCR, Y (cardinal cL + cardinal cLR + 1, eC, cL, cLR), cRR)
    | Y (nL, eCL, cLL, cRL), _ when 4 * cardinal cR < nL ->
      Y (n, eCL, cLL, Y (cardinal cRL + cardinal cR + 1, eC, cRL, cR))
    | _, _ ->
      Y (n, eC, cL, cR)

  let rec add_min e = function
    | O -> Y (1, e, O, O)
    | Y (n, eC, cL, cR) -> bal_y (n + 1) eC (add_min e cL) cR

  let rec add_max e = function
    | O -> Y (1, e, O, O)
    | Y (n, eC, cL, cR) -> bal_y (n + 1) eC cL (add_max e cR)

  let rec add' e = function
    | O -> Y (1, e, O, O)
    | Y (n, eC, cL, cR) ->
      let o = Elt.compare e eC in
      if o < 0 then bal_y (n + 1) eC (add' e cL) cR else
      if o > 0 then bal_y (n + 1) eC cL (add' e cR) else
      raise Keep
  let add e c = try add' e c with Keep -> c

  let rec pop_min_exn = function
    | O -> raise Not_found
    | Y (_, eC, O, cR) -> eC, cR
    | Y (n, eC, cL, cR) ->
      let e', cL' = pop_min_exn cL in e', bal_y (n - 1) eC cL' cR

  let rec pop_max_exn = function
    | O -> raise Not_found
    | Y (_, eC, cL, O) -> eC, cL
    | Y (n, eC, cL, cR) ->
      let e', cR' = pop_max_exn cR in e', bal_y (n - 1) eC cL cR'

  let rec glue e cL cR =
    match cL, cR with
    | O, _ -> add_min e cR
    | _, O -> add_max e cL
    | Y (nL, eL, cLL, cRL), Y (nR, eR, cLR, cRR) ->
      if nL < 4 * nR then bal_y (nL + nR + 1) eR (glue e cL cLR) cRR else
      if nR < 4 * nL then bal_y (nL + nR + 1) eL cLL (glue e cRL cR) else
      Y (nL + nR + 1, e, cL, cR)

  let cat cL cR =
    match cL, cR with
    | O, s | s, O -> s
    | _ -> let e, cL' = pop_max_exn cL in glue e cL' cR

  let cat_balanced n cL cR =
    if n = 0 then O else
    if cardinal cL > cardinal cR then
      let eC', cL' = pop_max_exn cL in Y (n, eC', cL', cR)
    else
      let eC', cR' = pop_min_exn cR in Y (n, eC', cL, cR')

(*
  let glue_opt = function None -> cat | Some e -> glue e
*)

  let rec cut k = function
    | O -> None, O, O
    | Y (_n, e, cL, cR) ->
      let o = Elt.compare_key k e in
      if o < 0 then let eo, cLL, cRL = cut k cL in eo, cLL, glue e cRL cR else
      if o > 0 then let eo, cLR, cRR = cut k cR in eo, glue e cL cLR, cRR else
      Some e, cL, cR

  let rec cut_elt ek = function
    | O -> None, O, O
    | Y (_n, e, cL, cR) ->
      let o = Elt.compare ek e in
      if o < 0 then let e_opt, cLL, cRL = cut_elt ek cL in
                    e_opt, cLL, glue e cRL cR else
      if o > 0 then let e_opt, cLR, cRR = cut_elt ek cR in
                    e_opt, glue e cL cLR, cRR else
      Some e, cL, cR

  let rec remove' k = function
    | O -> raise Keep
    | Y (n, eC, cL, cR) ->
      let o = Elt.compare_key k eC in
      if o < 0 then bal_y (n - 1) eC (remove' k cL) cR else
      if o > 0 then bal_y (n - 1) eC cL (remove' k cR) else
      cat_balanced (n - 1) cL cR
  let remove k c = try remove' k c with Keep -> c

  let rec pop k = function
    | O -> None
    | Y (n, eC, cL, cR) ->
      let o = Elt.compare_key k eC in
      if o < 0 then
        match pop k cL with
        | None -> None
        | Some (e, cL') -> Some (e, bal_y (n - 1) eC cL' cR) else
      if o > 0 then
        match pop k cR with
        | None -> None
        | Some (e, cR') -> Some (e, bal_y (n - 1) eC cL cR') else
      Some (eC, cat_balanced (n - 1) cL cR)

  let rec search f = function
    | O -> None
    | Y (_, eC, cL, cR) ->
      match f eC with
      | Some _ as z -> z
      | None ->
        begin match search f cL with
        | Some _ as z -> z
        | None -> search f cR
        end

  let rec fold f = function
    | O -> fun acc -> acc
    | Y (_, eC, cL, cR) -> fun acc -> fold f cR (f eC (fold f cL acc))

  let rec fold_rev f = function
    | O -> fun acc -> acc
    | Y (_, eC, cL, cR) -> fun acc -> fold_rev f cL (f eC (fold_rev f cR acc))

  let elements c = fold_rev (fun e es -> e :: es) c []

  let rec iter f = function
    | O -> ()
    | Y (_, eC, cL, cR) -> iter f cL; f eC; iter f cR

  let rec for_all f = function
    | O -> true
    | Y (_, eC, cL, cR) -> f eC && for_all f cL && for_all f cR

  let rec exists f = function
    | O -> false
    | Y (_, eC, cL, cR) -> f eC || exists f cL || exists f cR

  let rec filter f = function
    | O -> O
    | Y (_, eC, cL, cR) ->
      let cL' = filter f cL in
      let cR' = filter f cR in
      if f eC then glue eC cL' cR' else cat cL' cR'

  let compare cA cB =
    let rec aux = function
      | End, End -> 0
      | End, More _ -> -1
      | More _, End -> 1
      | More (eA, cA, qA), More (eB, cB, qB) ->
        let c = Elt.compare eA eB in if c <> 0 then c else
        aux (cons_enum cA qA, cons_enum cB qB) in
    aux (cons_enum cA End, cons_enum cB End)

  let equal cA cB = compare cA cB = 0

  let check_glue_opt ~msg f eA eB cL cR =
    match f eA eB with
    | None -> cat cL cR
    | Some e -> if Elt.compare e eB <> 0 then invalid_arg msg else glue e cL cR

  let rec finter f cA cB =
    let msg = "finter: Inconsistent merge function." in
    match cA, cB with
    | O, _ | _, O -> O
    | Y (nA, eA, cLA, cRA), Y (nB, eB, cLB, cRB) ->
      if nA < nB then
        let eA_opt, cLA, cRA = cut_elt eB cA in
        let cL = finter f cLA cLB in
        let cR = finter f cRA cRB in
        match eA_opt with
        | None -> cat cL cR
        | Some eA -> check_glue_opt ~msg f eA eB cL cR
      else
        let eB_opt, cLB, cRB = cut_elt eA cB in
        let cL = finter f cLA cLB in
        let cR = finter f cRA cRB in
        match eB_opt with
        | None -> cat cL cR
        | Some eB -> check_glue_opt ~msg f eA eB cL cR

  let rec funion f cA cB =
    let msg = "funion: Inconsistent merge function." in
    match cA, cB with
    | O, c | c, O -> c
    | Y (nA, eA, cLA, cRA), Y (nB, eB, cLB, cRB) ->
      if nA < nB then
        let eA_opt, cLA, cRA = cut_elt eB cA in
        let cL = funion f cLA cLB in
        let cR = funion f cRA cRB in
        match eA_opt with
        | None -> glue eB cL cR
        | Some eA -> check_glue_opt ~msg f eA eB cL cR
      else
        let eB_opt, cLB, cRB = cut_elt eA cB in
        let cL = funion f cLA cLB in
        let cR = funion f cRA cRB in
        match eB_opt with
        | None -> glue eA cL cR
        | Some eB -> check_glue_opt ~msg f eA eB cL cR

  let rec fcompl f cA cB =
    let msg = "fcompl: Inconsistent merge function." in
    match cA, cB with
    | O, _ -> cB
    | _, O -> O
    | Y (nA, eA, cLA, cRA), Y (nB, eB, cLB, cRB) ->
      if nA < nB then
        let eA_opt, cLA, cRA = cut_elt eB cA in
        let cL = fcompl f cLA cLB in
        let cR = fcompl f cRA cRB in
        match eA_opt with
        | None -> glue eB cL cR
        | Some eA -> check_glue_opt ~msg f eA eB cL cR
      else
        let eB_opt, cLB, cRB = cut_elt eA cB in
        let cL = fcompl f cLA cLB in
        let cR = fcompl f cRA cRB in
        match eB_opt with
        | None -> cat cL cR
        | Some eB -> check_glue_opt ~msg f eA eB cL cR

end
