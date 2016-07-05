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

open Prime_sigs
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
  val is_empty : t -> bool
  val cardinal : t -> int
  val contains : elt -> t -> bool
  val locate : elt -> t -> bool * int
  val get : int -> t -> elt
  val choose : t -> elt
  val min_elt : t -> elt
  val max_elt : t -> elt
  val pred_e : t -> elt -> elt
  val succ_e : t -> elt -> elt
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val cut_element : elt -> t -> bool * t * t
  val pop_min : t -> elt * t
  val pop_max : t -> elt * t
  val elements : t -> elt list
  val of_ordered_elements : elt list -> t
  val search : (elt -> 'a option) -> t -> 'a option
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_rev : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (elt -> unit) -> t -> unit
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val compl : t -> t -> t

  val card : t -> int
  val cut : elt -> t -> bool * t * t
end

module type S_monadic = sig
  type elt
  type t
  type 'a monad
  val fold_s : (elt -> 'a -> 'a monad) -> t -> 'a -> 'a monad
  val iter_s : (elt -> unit monad) -> t -> unit monad
  val search_s : (elt -> 'a option monad) -> t -> 'a option monad
  val for_all_s : (elt -> bool monad) -> t -> bool monad
  val exists_s : (elt -> bool monad) -> t -> bool monad
  val filter_s : (elt -> bool monad) -> t -> t monad
end

module type S_with_monadic = sig
  include S
  include S_monadic with type elt := elt and type t := t
end

exception Keep

module Make (E : OrderedType) = struct
  type elt = E.t
  type t = O | Y of int * elt * t * t

  type enumeration = End | More of elt * t * enumeration

  let rec cons_enum s q =
    match s with
    | O -> q
    | Y (_, e, sL, sR) -> cons_enum sL (More (e, sR, q))

  let empty = O
  let singleton e = Y (1, e, O, O)

  let is_empty = function O -> true | _ -> false

  let rec contains e = function
    | O -> false
    | Y (_, eC, sL, sR) ->
      let o = E.compare e eC in
      if o < 0 then contains e sL else
      if o > 0 then contains e sR else
      true

  let cardinal = function O -> 0 | Y (n, _, _, _) -> n

  let rec locate' i e = function
    | O -> false, i
    | Y (n, eC, sL, sR) ->
      let o = E.compare e eC in
      if o < 0 then locate' i e sL else
      if o > 0 then locate' (i + cardinal sL + 1) e sR else
      true, i + cardinal sL
  let locate = locate' 0

  let rec get i = function
    | O -> invalid_arg "Prime_enumset.get: Index out of bounds."
    | Y (n, eC, sL, sR) ->
      let nL = cardinal sL in
      if i < nL then get i sL else
      if i > nL then get (i - nL - 1) sR else
      eC

  let choose = function
    | O -> raise Not_found
    | Y (_, eC, _, _) -> eC

  let rec min_elt = function
    | O -> raise Not_found
    | Y (_, eC, O, _) -> eC
    | Y (_, eC, sL, _) -> min_elt sL

  let rec max_elt = function
    | O -> raise Not_found
    | Y (_, eC, _, O) -> eC
    | Y (_, eC, _, sR) -> max_elt sR

  let rec pred_e = function
    | O -> fun _ -> raise Not_found
    | Y (_, eC, sL, sR) -> fun e ->
      let o = E.compare e eC in
      if o < 0 then pred_e sL e else
      if o > 0 then try pred_e sR e with Not_found -> eC else
      max_elt sL

  let rec succ_e = function
    | O -> fun _ -> raise Not_found
    | Y (_, eC, sL, sR) -> fun e ->
      let o = E.compare e eC in
      if o < 0 then try succ_e sL e with Not_found -> eC else
      if o > 0 then succ_e sR e else
      min_elt sR

  let bal_y n eC sL sR =
    match sL, sR with
    | _, Y (nR, eCR, sLR, sRR) when 4 * cardinal sL < nR ->
      Y (n, eCR, Y (cardinal sL + cardinal sLR + 1, eC, sL, sLR), sRR)
    | Y (nL, eCL, sLL, sRL), _ when 4 * cardinal sR < nL ->
      Y (n, eCL, sLL, Y (cardinal sRL + cardinal sR + 1, eC, sRL, sR))
    | _, _ ->
      Y (n, eC, sL, sR)

  (* Pre: e is strictly less than the elements of the set. *)
  let rec add_min e = function
    | O -> Y (1, e, O, O)
    | Y (n, eC, sL, sR) -> bal_y (n + 1) eC (add_min e sL) sR

  (* Pre: e is strictly greater than the elements of the set. *)
  let rec add_max e = function
    | O -> Y (1, e, O, O)
    | Y (n, eC, sL, sR) -> bal_y (n + 1) eC sL (add_max e sR)

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

  let rec add' e = function
    | O -> Y (1, e, O, O)
    | Y (n, eC, sL, sR) ->
      let o = E.compare e eC in
      if o < 0 then bal_y (n + 1) eC (add' e sL) sR else
      if o > 0 then bal_y (n + 1) eC sL (add' e sR) else
      raise Keep
  let add e s = try add' e s with Keep -> s

  (* Pre: e is strictly between elements of sL and sR. *)
  let rec glue e sL sR =
    match sL, sR with
    | O, _ -> add_min e sR
    | _, O -> add_max e sL
    | Y (nL, eL, sLL, sRL), Y (nR, eR, sLR, sRR) ->
      if nL < 4 * nR then bal_y (nL + nR + 1) eR (glue e sL sLR) sRR else
      if nR < 4 * nL then bal_y (nL + nR + 1) eL sLL (glue e sRL sR) else
      Y (nL + nR + 1, e, sL, sR)

  (* Pre: Elements of sL are strictly less then elements of sR. *)
  let cat sL sR =
    match sL, sR with
    | O, s | s, O -> s
    | _, _ -> let e, sL' = pop_max sL in glue e sL' sR

  let rec cut_element eC = function
    | O -> (false, O, O)
    | Y (n, e, sL, sR) ->
      let c = E.compare eC e in
      if c = 0 then (true, sL, sR) else
      if c < 0 then
        let pres, sLL, sRL = cut_element eC sL in
        (pres, sLL, glue e sRL sR)
      else
        let pres, sLR, sRR = cut_element eC sR in
        (pres, glue e sL sLR, sRR)

  let rec remove' e = function
    | O -> raise Keep
    | Y (n, eC, sL, sR) ->
      let o = E.compare e eC in
      if o < 0 then bal_y (n - 1) eC (remove' e sL) sR else
      if o > 0 then bal_y (n - 1) eC sL (remove' e sR) else
      if n = 1 then O else
      if cardinal sL > cardinal sR
        then let eC', sL' = pop_max sL in Y (n - 1, eC', sL', sR)
        else let eC', sR' = pop_min sR in Y (n - 1, eC', sL, sR')
  let remove e s = try remove' e s with Keep -> s

  let elements s =
    let rec loop acc = function
      | O -> acc
      | Y (_, eC, sL, sR) -> loop (eC :: loop acc sR) sL in
    loop [] s

  let of_ordered_elements es =
    let rec count_and_check n e' = function
      | [] -> n
      | e :: es ->
        if E.compare e' e >= 0 then
          invalid_arg "Prime_enumset.of_ordererd_elements";
        count_and_check (succ n) e es in
    let rec build n es =
      if n = 0 then O, es else
      if n = 1 then Y (1, List.hd es, O, O), List.tl es else
      let sL, es = build (n / 2) es in
      let e = List.hd es in
      let sR, es = build ((n - 1) / 2) (List.tl es) in
      Y (n, e, sL, sR), es in
    match es with
    | [] -> O
    | e :: es' -> fst (build (count_and_check 1 e es') es)

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
    | O -> fun acc -> acc
    | Y (_, eC, sL, sR) -> fun acc -> fold f sR (f eC (fold f sL acc))

  let rec fold_rev f = function
    | O -> fun acc -> acc
    | Y (_, eC, sL, sR) -> fun acc -> fold_rev f sL (f eC (fold_rev f sR acc))

  let rec for_all f = function
    | O -> true
    | Y (_, e, sL, sR) -> f e && for_all f sL && for_all f sR

  let rec exists f = function
    | O -> false
    | Y (_, e, sL, sR) -> f e || exists f sL || exists f sR

  let rec filter f = function
    | O -> O
    | Y (_, e, sL, sR) ->
      let sL' = filter f sL in
      let sR' = filter f sR in
      if f e then glue e sL' sR' else cat sL' sR'

  let compare sA sB =
    let rec aux = function
      | End, End -> 0
      | End, More _ -> -1
      | More _, End -> 1
      | More (eA, sA, qA), More (eB, sB, qB) ->
        let c = E.compare eA eB in if c <> 0 then c else
        aux (cons_enum sA qA, cons_enum sB qB) in
    aux (cons_enum sA End, cons_enum sB End)

  let equal sA sB = compare sA sB = 0

  let rec subset sA sB =
    match sA, sB with
    | O, _ -> true
    | _, O -> false
    | Y (_, eA, sLA, sRA), (Y (_, eB, sLB, sRB) as sB) ->
      let c = E.compare eA eB in
      if c < 0 then subset (Y (0, eA, sLA, O)) sLB && subset sRA sB else
      if c > 0 then subset (Y (0, eA, O, sRA)) sRB && subset sLA sB else
      subset sLA sLB && subset sRA sRB

  let rec union sA sB =
    let aux sC e sL sR =
      let _, sLC, sRC = cut_element e sC in
      glue e (union sLC sL) (union sRC sR) in
    match sA, sB with
    | O, s | s, O -> s
    | Y (nA, eA, sLA, sRA), Y (nB, eB, sLB, sRB) ->
      if nA < nB then aux sA eB sLB sRB
                 else aux sB eA sLA sRA

  let rec inter sA sB =
    let aux sC e sL sR =
      let pres, sLC, sRC = cut_element e sC in
      let sL' = inter sLC sL in
      let sR' = inter sRC sR in
      if pres then glue e sL' sR' else cat sL' sR' in
    match sA, sB with
    | O, _ | _, O -> O
    | Y (nA, eA, sLA, sRA), Y (nB, eB, sLB, sRB) ->
      if nA < nB then aux sA eB sLB sRB
                 else aux sB eA sLA sRA

  let rec compl sA sB =
    match sA, sB with
    | O, _ -> sB
    | _, O -> O
    | _, Y (nB, eB, sLB, sRB) ->
      let presA, sLA, sRA = cut_element eB sA in
      if presA then cat     (compl sLA sLB) (compl sRA sRB)
               else glue eB (compl sLA sLB) (compl sRA sRB)

  (* Deprecated *)
  let card = cardinal
  let cut = cut_element

  module Make_monadic (M : Monad) = struct

    type 'a monad = 'a M.t

    let rec fold_s f = function
      | O -> M.return
      | Y (_, eC, sL, sR) ->
        fun acc -> M.bind (fold_s f sL acc) @@
          fun acc -> M.bind (f eC acc) (fold_s f sR)

    let rec iter_s f = function
      | O -> M.return ()
      | Y (_, eC, sL, sR) ->
        M.bind (iter_s f sL) @@ fun () ->
        M.bind (f eC) @@ fun () ->
        iter_s f sR

    let rec search_s f = function
      | O -> M.return None
      | Y (_, eC, sL, sR) ->
        M.bind (search_s f sL) begin function
        | Some _ as r -> M.return r
        | None ->
          M.bind (f eC) begin function
          | Some _ as r -> M.return r
          | None -> search_s f sR
          end
        end

    let rec for_all_s f = function
      | O -> M.return true
      | Y (_, eC, sL, sR) ->
        M.bind (for_all_s f sL) begin function
        | false -> M.return false
        | true ->
          M.bind (f eC) begin function
          | false -> M.return false
          | true -> for_all_s f sR
          end
        end

    let rec exists_s f = function
      | O -> M.return false
      | Y (_, eC, sL, sR) ->
        M.bind (exists_s f sL) begin function
        | true -> M.return true
        | false ->
          M.bind (f eC) begin function
          | true -> M.return true
          | false -> exists_s f sR
          end
        end

    let rec filter_s f = function
      | O -> M.return O
      | Y (_, e, sL, sR) ->
        M.bind (filter_s f sL) @@ fun sL' ->
        M.bind (filter_s f sR) @@ fun sR' ->
        M.bind (f e) @@ fun c ->
        M.return (if c then glue e sL' sR' else cat sL' sR')

  end
end

module Make_monadic (Elt : OrderedType) (Monad : Monad) = struct
  include Make (Elt)
  include Make_monadic (Monad)
end