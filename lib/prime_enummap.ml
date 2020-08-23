(* Copyright (C) 2013--2020  Petter A. Urkedal <paurkedal@gmail.com>
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
  val mem : key -> 'a t -> bool
  val app : 'a t -> key -> 'a option
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val locate : key -> 'a t -> bool * int
  val get : 'a t -> int -> 'a
  val get_binding : 'a t -> int -> key * 'a
  val min_binding : 'a t -> (key * 'a) option
  val max_binding : 'a t -> (key * 'a) option
  val pred_binding : 'a t -> key -> (key * 'a) option
  val succ_binding : 'a t -> key -> (key * 'a) option
  val add : key -> 'a -> 'a t -> 'a t
  val pop : key -> 'a t -> ('a * 'a t) option
  val pop_min : 'a t -> key * 'a * 'a t
  val pop_max : 'a t -> key * 'a * 'a t
  val remove : key -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val cut_binding : key -> 'a t -> 'a option * 'a t * 'a t
  val bindings : 'a t -> (key * 'a) list
  val of_ordered_bindings : (key * 'a) list -> 'a t
  val asc_bindings : ?where: (key -> int) -> 'a t -> (key * 'a) Seq.t
  val dsc_bindings : ?where: (key -> int) -> 'a t -> (key * 'a) Seq.t
  val search : (key -> 'a -> 'b option) -> 'a t -> 'b option
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold_rev : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
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

end

module type S_monadic = sig
  type key
  type 'a t
  type 'a monad
  val fold_s : (key -> 'a -> 'b -> 'b monad) -> 'a t -> 'b -> 'b monad
  val iter_s : (key -> 'a -> unit monad) -> 'a t -> unit monad
  val search_s : (key -> 'a -> 'b option monad) -> 'a t -> 'b option monad
  val for_all_s : (key -> 'a -> bool monad) -> 'a t -> bool monad
  val exists_s : (key -> 'a -> bool monad) -> 'a t -> bool monad
  val filter_s : (key -> 'a -> bool monad) -> 'a t -> 'a t monad
  val map_s : ('a -> 'b monad) -> 'a t -> 'b t monad
  val mapi_s : (key -> 'a -> 'b monad) -> 'a t -> 'b t monad
  val fmapi_s : (key -> 'a -> 'b option monad) -> 'a t -> 'b t monad
end

module type S_with_monadic = sig
  include S
  include S_monadic with type key := key and type 'a t := 'a t
end

exception Keep

module Make (K : OrderedType) = struct
  type key = K.t
  type 'a t = O | Y of int * key * 'a * 'a t * 'a t

  type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

  let rec cons_enum m q =
    match m with
    | O -> q
    | Y (_n, k, e, mL, mR) -> cons_enum mL (More (k, e, mR, q))

  let empty = O
  let singleton k e = Y (1, k, e, O, O)

  let is_empty = function O -> true | _ -> false

  let rec mem k = function
    | O -> false
    | Y (_, kC, _, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then mem k mL else
      if o > 0 then mem k mR else
      true

  let rec app m k =
    match m with
    | O -> None
    | Y (_, kC, eC, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then app mL k else
      if o > 0 then app mR k else
      Some eC

  let rec find k = function
    | O -> raise Not_found
    | Y (_, kC, eC, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then find k mL else
      if o > 0 then find k mR else
      eC

  let find_opt k m = try Some (find k m) with Not_found -> None

  let cardinal = function O -> 0 | Y (n, _, _, _, _) -> n

  let rec locate' i k = function
    | O -> false, i
    | Y (_n, kC, _, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then locate' i k mL else
      if o > 0 then locate' (i + cardinal mL + 1) k mR else
      true, i + cardinal mL
  let locate k = locate' 0 k

  let rec get m i =
    match m with
    | O -> invalid_arg "Prime_enummap.get_e: Index out of bounds."
    | Y (_n, _kC, eC, mL, mR) ->
      let nL = cardinal mL in
      if i < nL then get mL i else
      if i > nL then get mR (i - nL - 1) else
      eC

  let rec get_binding m i =
    match m with
    | O -> invalid_arg "Prime_enummap.get_binding: Index out of bounds."
    | Y (_n, kC, eC, mL, mR) ->
      let nL = cardinal mL in
      if i < nL then get_binding mL i else
      if i > nL then get_binding mR (i - nL - 1) else
      (kC, eC)

  let rec min_binding = function
    | O -> None
    | Y (_, kC, eC, O, _) -> Some (kC, eC)
    | Y (_, _, _, mL, _) -> min_binding mL

  let rec max_binding = function
    | O -> None
    | Y (_, kC, eC, _, O) -> Some (kC, eC)
    | Y (_, _, _, _, mR) -> max_binding mR

  let rec pred_binding = function
    | O -> fun _k -> None
    | Y (_, kC, eC, mL, mR) -> fun k ->
      let o = K.compare k kC in
      if o < 0 then pred_binding mL k else
      if o > 0 then
        match pred_binding mR k with
        | Some _ as b -> b
        | None -> Some (kC, eC) else
      max_binding mL

  let rec succ_binding = function
    | O -> fun _k -> None
    | Y (_, kC, eC, mL, mR) -> fun k ->
      let o = K.compare k kC in
      if o < 0 then
        match succ_binding mL k with
        | Some _ as b -> b
        | None -> Some (kC, eC) else
      if o > 0 then succ_binding mR k else
      min_binding mR

  let bal_y n kC eC mL mR =
    match mL, mR with
    | _, Y (nR, kCR, eCR, mLR, mRR) when 4 * cardinal mL < nR ->
      Y (n, kCR, eCR, Y (cardinal mL + cardinal mLR + 1, kC, eC, mL, mLR), mRR)
    | Y (nL, kCL, eCL, mLL, mRL), _ when 4 * cardinal mR < nL ->
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
    | Y (_, kC, eC, O, mR) -> kC, eC, mR
    | Y (n, kC, eC, mL, mR) ->
      let k', e', mL' = pop_min mL in k', e', bal_y (n - 1) kC eC mL' mR

  let rec pop_max = function
    | O -> raise Not_found
    | Y (_, kC, eC, mL, O) -> kC, eC, mL
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

  let cat_balanced n mL mR =
    if n = 0 then O else
    if cardinal mL > cardinal mR then
      let kC', eC', mL' = pop_max mL in Y (n, kC', eC', mL', mR)
    else
      let kC', eC', mR' = pop_min mR in Y (n, kC', eC', mL, mR')

  let glue_opt k e_opt mL mR =
    match e_opt with None -> cat mL mR | Some e -> glue k e mL mR

  let rec cut_binding kC = function
    | O -> (None, O, O)
    | Y (_, k, e, mL, mR) ->
      let c = K.compare kC k in
      if c = 0 then (Some e, mL, mR) else
      if c < 0 then
        let eC, mLL, mRL = cut_binding kC mL in
        (eC, mLL, glue k e mRL mR)
      else
        let eC, mLR, mRR = cut_binding kC mR in
        (eC, glue k e mL mLR, mRR)

  let rec remove' k = function
    | O -> raise Keep
    | Y (n, kC, eC, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then bal_y (n - 1) kC eC (remove' k mL) mR else
      if o > 0 then bal_y (n - 1) kC eC mL (remove' k mR) else
      cat_balanced (n - 1) mL mR
  let remove k m = try remove' k m with Keep -> m

  let rec update' k f = function
   | O ->
      (match f None with
       | None -> raise Keep
       | Some e -> (1, Y (1, k, e, O, O)))
   | Y (n, kC, eC, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then
        let dn, mL' = update' k f mL in
        (dn, bal_y (n + dn) kC eC mL' mR)
      else
      if o > 0 then
        let dn, mR' = update' k f mR in
        (dn, bal_y (n + dn) kC eC mL mR')
      else
      (match f (Some eC) with
       | None -> (-1, cat mL mR)
       | Some e -> (0, bal_y n kC e mL mR))
  let update k f m = try snd (update' k f m) with Keep -> m

  let rec pop k = function
    | O -> None
    | Y (n, kC, eC, mL, mR) ->
      let o = K.compare k kC in
      if o < 0 then
        match pop k mL with
        | None -> None
        | Some (e, mL') -> Some (e, bal_y (n - 1) kC eC mL' mR) else
      if o > 0 then
        match pop k mR with
        | None -> None
        | Some (e, mR') -> Some (e, bal_y (n - 1) kC eC mL mR') else
      Some (eC, cat_balanced (n - 1) mL mR)

  let bindings m =
    let rec loop acc = function
      | O -> acc
      | Y (_, kC, eC, mL, mR) -> loop ((kC, eC) :: loop acc mR) mL in
    loop [] m

  let of_ordered_bindings kes =
    let rec count_and_check n k' = function
      | [] -> n
      | (k, _) :: kes ->
        if K.compare k' k >= 0 then
          invalid_arg "Prime_enummap.of_ordererd_bindings";
        count_and_check (succ n) k kes in
    let rec build n kes =
      if n = 0 then O, kes else
      if n = 1 then let k, e = List.hd kes in Y(1, k, e, O, O), List.tl kes else
      let mL, kes = build (n / 2) kes in
      let k, e = List.hd kes in
      let mR, kes = build ((n - 1) / 2) (List.tl kes) in
      Y (n, k, e, mL, mR), kes in
    match kes with
    | [] -> O
    | (k0, _) :: kes' -> fst (build (count_and_check 1 k0 kes') kes)

  let rec asc_full_seq ~cont m rest () =
    (match m, rest with
     | O, [] -> cont
     | O, (ke', m') :: rest' -> Seq.Cons (ke', asc_full_seq ~cont m' rest')
     | Y (_, kC, eC, mL, mR), _ ->
        asc_full_seq ~cont mL (((kC, eC), mR) :: rest) ())

  let rec asc_upper_seq ~where ~cont m rest () =
    (match m, rest with
     | O, [] -> cont
     | O, (ke', m') :: rest' -> Seq.Cons (ke', asc_full_seq ~cont m' rest')
     | Y (_, kC, eC, mL, mR), _ ->
        let c = where kC in
        if c < 0 then asc_upper_seq ~where ~cont mR rest () else
        if c > 0 then invalid_arg "Prime_enummap.asc_elements" else
        asc_upper_seq ~where ~cont mL (((kC, eC), mR) :: rest) ())

  let rec asc_lower_seq ~where m rest () =
    (match m, rest with
     | O, [] -> Seq.Nil
     | O, (ke', m') :: rest' -> Seq.Cons (ke', asc_lower_seq ~where m' rest')
     | Y (_, kC, eC, mL, mR), _ ->
        let c = where kC in
        if c < 0 then invalid_arg "Prime_enummap.asc_elements" else
        if c > 0 then asc_lower_seq ~where mL rest () else
        let cont = Seq.Cons ((kC, eC), asc_lower_seq ~where mR rest) in
        asc_full_seq ~cont mL [] ())

  let asc_bindings ?where m =
    (match where with
     | None -> asc_full_seq ~cont:Seq.Nil m []
     | Some where ->
        let rec seek = function
         | O -> Seq.empty
         | Y (_, kC, eC, mL, mR) ->
            let c = where kC in
            if c < 0 then seek mR else
            if c > 0 then seek mL else
            let cont = Seq.Cons ((kC, eC), asc_lower_seq ~where mR []) in
            asc_upper_seq ~where ~cont mL []
        in seek m)

  let rec desc_full_seq ~cont m rest () =
    (match m, rest with
     | O, [] -> cont
     | O, (ke', m') :: rest' -> Seq.Cons (ke', desc_full_seq ~cont m' rest')
     | Y (_, kC, eC, mL, mR), _ ->
        desc_full_seq ~cont mR (((kC, eC), mL) :: rest) ())

  let rec desc_lower_seq ~where ~cont m rest () =
    (match m, rest with
     | O, [] -> cont
     | O, (ke', m') :: rest' -> Seq.Cons (ke', desc_full_seq ~cont m' rest')
     | Y (_, kC, eC, mL, mR), _ ->
        let c = where kC in
        if c < 0 then invalid_arg "Prime_enummap.dsc_elements" else
        if c > 0 then desc_lower_seq ~where ~cont mL rest () else
        desc_lower_seq ~where ~cont mR (((kC, eC), mL) :: rest) ())

  let rec desc_upper_seq ~where m rest () =
    (match m, rest with
     | O, [] -> Seq.Nil
     | O, (ke', m') :: rest' -> Seq.Cons (ke', desc_upper_seq ~where m' rest')
     | Y (_, kC, eC, mL, mR), _ ->
        let c = where kC in
        if c < 0 then desc_upper_seq ~where mR rest () else
        if c > 0 then invalid_arg "Prime_enummap.dsc_elements" else
        let cont = Seq.Cons ((kC, eC), desc_upper_seq ~where mL rest) in
        desc_full_seq ~cont mR [] ())

  let dsc_bindings ?where m =
    (match where with
     | None -> desc_full_seq ~cont:Seq.Nil m []
     | Some where ->
        let rec seek = function
         | O -> Seq.empty
         | Y (_, kC, eC, mL, mR) ->
            let c = where kC in
            if c < 0 then seek mR else
            if c > 0 then seek mL else
            let cont = Seq.Cons ((kC, eC), desc_upper_seq ~where mL []) in
            desc_lower_seq ~where ~cont mR []
        in seek m)

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
    | O -> fun acc -> acc
    | Y (_, kC, eC, mL, mR) -> fun acc -> fold f mR (f kC eC (fold f mL acc))

  let rec fold_rev f = function
    | O -> fun acc -> acc
    | Y (_, kC, eC, mL, mR) ->
      fun acc -> fold_rev f mL (f kC eC (fold_rev f mR acc))

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
    | Y (_, k, e, mL, mR) ->
      let mL' = fmapi f mL and mR' = fmapi f mR in
      match f k e with
      | None -> cat mL' mR'
      | Some e' -> glue k e' mL' mR'

  let rec filter f = function
    | O -> O
    | Y (_, k, e, mL, mR) ->
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
      let eB_opt, mLB, mRB = cut_binding kA mB in
      glue_opt kA (f kA (Some eA) eB_opt) (merge f mLA mLB) (merge f mRA mRB)
    | _, Y (_nB, kB, eB, mLB, mRB) ->
      let eA_opt, mLA, mRA = cut_binding kB mA in
      glue_opt kB (f kB eA_opt (Some eB)) (merge f mLA mLB) (merge f mRA mRB)
    | _ -> assert false

  let rec finter f mA mB =
    match mA, mB with
    | O, _ | _, O -> O
    | Y (_nA, kA, eA, mLA, mRA), _ ->
      let eB_opt, mLB, mRB = cut_binding kA mB in
      let mL = finter f mLA mLB in
      let mR = finter f mRA mRB in
      match eB_opt with
      | None -> cat mL mR
      | Some eB -> glue_opt kA (f kA eA eB) mL mR

  let rec funion f mA mB =
    match mA, mB with
    | O, m | m, O -> m
    | Y (_nA, kA, eA, mLA, mRA), _ ->
      let eB_opt, mLB, mRB = cut_binding kA mB in
      let mL = funion f mLA mLB in
      let mR = funion f mRA mRB in
      match eB_opt with
      | None -> glue kA eA mL mR
      | Some eB -> glue_opt kA (f kA eA eB) mL mR

  let rec fcompl f mA mB =
    match mA, mB with
    | O, _ -> mB
    | _, O -> O
    | _, Y (_nB, kB, eB, mLB, mRB) ->
      let eA_opt, mLA, mRA = cut_binding kB mA in
      let mL = fcompl f mLA mLB in
      let mR = fcompl f mRA mRB in
      match eA_opt with
      | None -> glue kB eB mL mR
      | Some eA -> glue_opt kB (f kB eA eB) mL mR

  let rec fpatch f mA mB =
    match mA, mB with
    | O, _ -> mB
    | _, O -> fmapi (fun k eA -> f k eA None) mA
    | _, Y (_nB, k, eB, mLB, mRB) ->
      let eA_opt, mLA, mRA = cut_binding k mA in
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

  module Make_monadic (M : Monad) = struct

    type 'a monad = 'a M.t

    let rec fold_s f = function
      | O -> M.return
      | Y (_, k, e, mL, mR) ->
        fun acc -> M.bind (fold_s f mL acc) @@
          fun acc -> M.bind (f k e acc) (fold_s f mR)

    let rec iter_s f = function
      | O -> M.return ()
      | Y (_, k, e, mL, mR) ->
        M.bind (iter_s f mL) @@ fun () ->
        M.bind (f k e) @@ fun () ->
        iter_s f mR

    let rec search_s f = function
      | O -> M.return None
      | Y (_, k, e, mL, mR) ->
        M.bind (search_s f mL) begin function
        | Some _ as r -> M.return r
        | None ->
          M.bind (f k e) begin function
          | Some _ as r -> M.return r
          | None -> search_s f mR
          end
        end

    let rec for_all_s f = function
      | O -> M.return true
      | Y (_, k, e, mL, mR) ->
        M.bind (for_all_s f mL) begin function
        | false -> M.return false
        | true ->
          M.bind (f k e) begin function
          | false -> M.return false
          | true -> for_all_s f mR
          end
        end

    let rec exists_s f = function
      | O -> M.return false
      | Y (_, k, e, mL, mR) ->
        M.bind (exists_s f mL) begin function
        | true -> M.return true
        | false ->
          M.bind (f k e) begin function
          | true -> M.return true
          | false -> exists_s f mR
          end
        end

    let rec filter_s f = function
      | O -> M.return O
      | Y (_, k, e, sL, sR) ->
        M.bind (filter_s f sL) @@ fun sL' ->
        M.bind (filter_s f sR) @@ fun sR' ->
        M.bind (f k e) @@ fun c ->
        M.return (if c then glue k e sL' sR' else cat sL' sR')

    let rec map_s f = function
      | O -> M.return O
      | Y (_, k, e, mL, mR) ->
        M.bind (map_s f mL) @@ fun mL' ->
        M.bind (map_s f mR) @@ fun mR' ->
        M.bind (f e) @@ fun e' ->
        M.return (glue k e' mL' mR')

    let rec mapi_s f = function
      | O -> M.return O
      | Y (_, k, e, mL, mR) ->
        M.bind (mapi_s f mL) @@ fun mL' ->
        M.bind (mapi_s f mR) @@ fun mR' ->
        M.bind (f k e) @@ fun e' ->
        M.return (glue k e' mL' mR')

    let rec fmapi_s f = function
      | O -> M.return O
      | Y (_, k, e, mL, mR) ->
        M.bind (fmapi_s f mL) @@ fun mL' ->
        M.bind (fmapi_s f mR) @@ fun mR' ->
        M.bind (f k e) @@ function
        | None -> M.return (cat mL' mR')
        | Some e' -> M.return (glue k e' mL' mR')
  end

end

module Make_monadic (Key : OrderedType) (Monad : Monad) = struct
  include Make (Key)
  include Make_monadic (Monad)
end
