(* Copyright (C) 2015--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

type 'a t = O | Y of int * 'a * 'a t * 'a t

type 'a enumeration = End | More of 'a * 'a t * 'a enumeration

let rec cons_enum s q =
  match s with
  | O -> q
  | Y (_, e, sL, sR) -> cons_enum sL (More (e, sR, q))

let is_empty = function O -> true | Y _ -> false

let length = function O -> 0 | Y (n, _, _, _) -> n

let empty = O

let singleton x = Y (1, x, O, O)

let sample f =
  let rec aux i n =
    if n = 0 then O else
    let nL = n / 2 in
    let nR = n - nL - 1 in
    Y (n, f (i + nL), aux i nL, aux (i + nL + 1) nR) in
  aux 0

let rec get s i =
  match s with
  | O -> invalid_arg "Prime_enumlist.get"
  | Y (_n, eC, sL, sR) ->
    let nL = length sL in
    if i < nL then get sL i else
    if i > nL then get sR (i - nL - 1) else
    eC

let rec set i e s =
  match s with
  | O -> invalid_arg "Prime_enumlist.set"
  | Y (n, eC, sL, sR) ->
    let nL = length sL in
    if i < nL then Y (n, eC, set i e sL, sR) else
    if i > nL then Y (n, eC, sL, set (i - nL - 1) e sR) else
    Y (n, e, sL, sR)

let rec update i f s =
  match s with
  | O -> invalid_arg "Prime_enumlist.update"
  | Y (n, eC, sL, sR) ->
    let nL = length sL in
    if i < nL then Y (n, eC, update i f sL, sR) else
    if i > nL then Y (n, eC, sL, update (i - nL - 1) f sR) else
    Y (n, f eC, sL, sR)

let rec first_exn = function
  | O -> invalid_arg "Prime_enumlist.first_exn"
  | Y (_, eC, O, _) -> eC
  | Y (_, _, sL, _) -> first_exn sL

let rec last_exn = function
  | O -> invalid_arg "Prime_enumlist.last_exn"
  | Y (_, eC, _, O) -> eC
  | Y (_, _, _, sR) -> last_exn sR

let bal n eC sL sR =
  match sL, sR with
  | _, Y (nR, eCR, sLR, sRR) when 4 * length sL < nR ->
    Y (n, eCR, Y (length sL + length sLR + 1, eC, sL, sLR), sRR)
  | Y (nL, eCL, sLL, sRL), _ when 4 * length sR < nL ->
    Y (n, eCL, sLL, Y (length sRL + length sR + 1, eC, sRL, sR))
  | _, _ ->
    Y (n, eC, sL, sR)

let rec pop_first_exn = function
  | O -> invalid_arg "Prime_enumlist.pop_first_exn"
  | Y (_, eC, O, sR) -> eC, sR
  | Y (n, eC, sL, sR) ->
    let e', sL' = pop_first_exn sL in
    (e', bal (n-1) eC sL' sR)

let rec pop_last_exn = function
  | O -> invalid_arg "Prime_enumlist.pop_last_exn"
  | Y (_, eC, sL, O) -> eC, sL
  | Y (n, eC, sL, sR) ->
    let e', sR' = pop_last_exn sR in
    (e', bal (n-1) eC sL sR')

let rec push_first e = function
  | O -> Y (1, e, O, O)
  | Y (n, eC, sL, sR) -> bal (n + 1) eC (push_first e sL) sR

let rec push_last e = function
  | O -> Y (1, e, O, O)
  | Y (n, eC, sL, sR) -> bal (n + 1) eC sL (push_last e sR)

let rec glue e sL sR =
  match sL, sR with
  | O, _ -> push_first e sR
  | _, O -> push_last e sL
  | Y (nL, eL, sLL, sRL), Y (nR, eR, sLR, sRR) ->
    if nL < 4 * nR then bal (nL + nR + 1) eR (glue e sL sLR) sRR else
    if nR < 4 * nL then bal (nL + nR + 1) eL sLL (glue e sRL sR) else
    Y (nL + nR + 1, e, sL, sR)

let cat sL sR =
  match sL, sR with
  | O, s | s, O -> s
  | _, _ -> let e, sL' = pop_last_exn sL in glue e sL' sR

let rec cut i = function
  | O -> if i = 0 then (O, O) else invalid_arg "Prime_enumlist.cut_at"
  | Y (_n, e, sL, sR) ->
    let nL = length sL in
    if i < nL then let sX, sL' = cut i sL in (sX, glue e sL' sR) else
    if i > nL then let sR', sX = cut (i - nL - 1) sR in (glue e sL sR', sX) else
    (sL, push_first e sR)

let rec delete i = function
  | O -> invalid_arg "Prime_enumlist.delete"
  | Y (n, eC, sL, sR) ->
    let nL = length sL in
    if i < nL then bal (n - 1) eC (delete i sL) sR else
    if i > nL then bal (n - 1) eC sL (delete (i - nL - 1) sR) else
    if n = 1 then O else
    if length sL > length sR
    then let eC', sL' = pop_last_exn sL in Y (n - 1, eC', sL', sR)
    else let eC', sR' = pop_first_exn sR in Y (n - 1, eC', sL, sR')

let rec insert i e = function
  | O -> if i = 0 then Y (1, e, O, O) else invalid_arg "Prime_enumlist.insert"
  | Y (n, eC, sL, sR) ->
    let nL = length sL in
    if i < nL then bal (n + 1) eC (insert i e sL) sR else
    if i > nL then bal (n + 1) eC sL (insert (i - nL - 1) e sR) else
    if length sL > length sR then Y (n + 1, e, sL, push_first eC sR)
                             else Y (n + 1, eC, push_last e sL, sR)

let of_list xs =
  let rec aux n xs =
    if n = 0 then (O, xs) else
    let sL, xs = aux (n / 2) xs in
    let e,  xs = List.hd xs, List.tl xs in
    let sR, xs = aux ((n - 1) / 2) xs in
    (Y (n, e, sL, sR), xs) in
  fst (aux (List.length xs) xs)

let rec push_elements = function
  | O -> fun acc -> acc
  | Y (_, e, sL, sR) -> fun acc ->
    push_elements sL (e :: push_elements sR acc)

let to_list s = push_elements s []

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
  | Y (_, e, sL, sR) -> iter f sL; f e; iter f sR

let iteri f =
  let rec aux i = function
    | O -> ()
    | Y (_n, e, sL, sR) ->
      let nL = length sL in
      aux i sL; f (i + nL) e; aux (i + nL + 1) sR in
  aux 0

let rec fold f = function
  | O -> fun acc -> acc
  | Y (_, e, sL, sR) -> fun acc -> acc |> fold f sL |> f e |> fold f sR

let foldi f =
  let rec aux i = function
    | O -> fun acc -> acc
    | Y (_n, e, sL, sR) ->
      let nL = length sL in
      fun acc -> acc |> aux i sL |> f (i + nL) e |> aux (i + nL + 1) sR in
  aux 0

let rec for_all f = function
  | O -> true
  | Y (_, e, sL, sR) -> f e && for_all f sL && for_all f sR

let rec exists f = function
  | O -> false
  | Y (_, e, sL, sR) -> f e || exists f sL || exists f sR

let rec filter f = function
  | O -> O
  | Y (_, e, sL, sR) ->
    let sL', sR' = filter f sL, filter f sR in
    if f e then glue e sL' sR' else cat sL' sR'

let rec map f = function
  | O -> O
  | Y (n, e, sL, sR) -> Y (n, f e, map f sL, map f sR)

let mapi f =
  let rec aux i = function
    | O -> O
    | Y (n, e, sL, sR) ->
      let nL = length sL in
      Y (n, f (i + nL) e, aux i sL, aux (i + nL + 1) sR) in
  aux 0

let rec filter_map f = function
  | O -> O
  | Y (_n, e, sL, sR) ->
    let sL', sR' = filter_map f sL, filter_map f sR in
    match f e with
    | Some e' -> glue e' sL' sR'
    | None -> cat sL' sR'

let filter_mapi f =
  let rec aux i = function
    | O -> O
    | Y (_n, e, sL, sR) ->
      let nL = length sL in
      let sL' = aux i sL in
      let sR' = aux (i + nL + 1) sR in
      match f (i + nL) e with
      | Some e' -> glue e' sL' sR'
      | None -> cat sL' sR' in
  aux 0

let fmap = filter_map
let fmapi = filter_mapi

let compare f sA sB =
  let rec aux = function
    | End, End -> 0
    | End, More _ -> -1
    | More _, End -> 1
    | More (eA, sA, qA), More (eB, sB, qB) ->
      let c = f eA eB in if c <> 0 then c else
      aux (cons_enum sA qA, cons_enum sB qB) in
  aux (cons_enum sA End, cons_enum sB End)

let equal f sA sB =
  let rec aux = function
    | End, End -> true
    | End, More _ | More _, End -> false
    | More (eA, sA, qA), More (eB, sB, qB) ->
      f eA eB && aux (cons_enum sA qA, cons_enum sB qB) in
  aux (cons_enum sA End, cons_enum sB End)
