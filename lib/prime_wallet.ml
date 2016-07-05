(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Unprime

let bad_arg s = invalid_arg ("Prime_wallet." ^ s)

type 'a t =
  | Empty
  | Even of ('a * 'a) t
  | Odd of 'a * ('a * 'a) t

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let singleton x = Odd (x, Empty)

let even = function
  | Empty -> Empty
  | t -> Even t

let rec length : type a. a t -> int = function
  | Empty      -> 0
  | Even t     -> 2 * length t
  | Odd (_, t) -> 2 * length t + 1

let rec push : type a. a -> a t -> a t = fun x -> function
  | Empty -> Odd (x, Empty)
  | Even t -> Odd (x, t)
  | Odd (y, t) -> Even (push (x, y) t)

let rec copush : type a. a -> a t -> a t = fun x -> function
  | Empty -> Odd (x, Empty)
  | Even t -> Odd (x, t)
  | Odd (y, t) -> Even (copush (y, x) t)

let rec pop : type a. a t -> a * a t = function
  | Empty -> bad_arg "pop: Empty."
  | Even t -> let (x, y), t' = pop t in x, Odd (y, t')
  | Odd (z, Empty) -> z, Empty
  | Odd (z, t) -> z, Even t

let rec copop : type a. a t -> a * a t = function
  | Empty -> bad_arg "pop: Empty."
  | Even t -> let (y, x), t' = copop t in x, Odd (y, t')
  | Odd (z, Empty) -> z, Empty
  | Odd (z, t) -> z, Even t

let sample f n =
  let rec loop n w = if n < 0 then w else loop (n - 1) (push (f n) w) in
  loop (n - 1) empty

let rec get : type a. int -> a t -> a = fun i -> function
  | Empty -> bad_arg "get: Index out of bounds."
  | Even t -> (if i land 1 = 0 then fst else snd) (get (i lsr 1) t)
  | Odd (z, t) ->
    if i = 0 then z else
    let j = i - 1 in
    (if j land 1 = 0 then fst else snd) (get (j lsr 1) t)

let rec coget : type a. int -> a t -> a = fun i -> function
  | Empty -> bad_arg "get: Index out of bounds."
  | Even t -> (if i land 1 = 0 then snd else fst) (coget (i lsr 1) t)
  | Odd (z, t) ->
    if i = 0 then z else
    let j = i - 1 in
    (if j land 1 = 0 then snd else fst) (coget (j lsr 1) t)

let rec modify : type a. int -> (a -> a) -> a t -> a t = fun i f -> function
  | Empty -> bad_arg "set: Index out of bounds."
  | Even t ->
    let h (x, y) = if i land 1 = 0 then (f x, y) else (x, f y) in
    Even (modify (i lsr 1) h t)
  | Odd (x, t) ->
    if i = 0 then Odd (f x, t) else
    let j = i - 1 in
    let h (x, y) = if j land 1 = 0 then (f x, y) else (x, f y) in
    Odd (x, modify (j lsr 1) h t)

let rec comodify : type a. int -> (a -> a) -> a t -> a t = fun i f -> function
  | Empty -> bad_arg "set: Index out of bounds."
  | Even t ->
    let h (y, x) = if i land 1 = 0 then (y, f x) else (f y, x) in
    Even (comodify (i lsr 1) h t)
  | Odd (z, t) ->
    if i = 0 then Odd (f z, t) else
    let j = i - 1 in
    let h (y, x) = if j land 1 = 0 then (y, f x) else (f y, x) in
    Odd (z, comodify (j lsr 1) h t)

let set i x = modify i (fun _ -> x)
let coset i x = comodify i (fun _ -> x)

let rec for_all : type a. (a -> bool) -> a t -> bool = fun f -> function
  | Empty -> true
  | Even t -> for_all (fun (x, y) -> f x && f y) t
  | Odd (z, t) -> f z && for_all (fun (x, y) -> f x && f y) t

let rec for_all2 : type a b. (a -> b -> bool) -> a t -> b t -> bool =
  fun f tA' tB' ->
  match tA', tB' with
  | Empty, Empty -> true
  | Even tA, Even tB ->
    for_all2 (fun (xA, yA) (xB, yB) -> f xA xB && f yA yB) tA tB
  | Odd (zA, tA), Odd (zB, tB) ->
    f zA zB && for_all2 (fun (xA, yA) (xB, yB) -> f xA xB && f yA yB) tA tB
  | Even _, _ | Odd _, _ | Empty, _ -> bad_arg "for_all2: Different size."

let rec search : type a. (a -> 'b option) -> a t -> 'b option = fun f t' ->
  let g (x, y) =
    match f x with
    | Some _ as r -> r
    | None -> f y in
  match t' with
  | Empty -> None
  | Even t -> search g t
  | Odd (z, t) ->
    begin match f z with
    | Some _ as r -> r
    | None -> search g t
    end

let rec map : type a b. (a -> b) -> a t -> b t = fun f -> function
  | Empty -> Empty
  | Even t -> Even (map (fun (x, y) -> (f x, f y)) t)
  | Odd (z, t) -> Odd (f z, map (fun (x, y) -> (f x, f y)) t)

let rec map2 : type a b c. (a -> b -> c) -> a t -> b t -> c t = fun f tA' tB' ->
  match tA', tB' with
  | Empty, Empty -> Empty
  | Even tA, Even tB ->
    Even (map2 (fun (xA, yA) (xB, yB) -> (f xA xB, f yA yB)) tA tB)
  | Odd (zA, tA), Odd (zB, tB) ->
    Odd (f zA zB, map2 (fun (xA, yA) (xB, yB) -> (f xA xB, f yA yB)) tA tB)
  | Empty, _ | Even _, _ | Odd _, _ -> bad_arg "map2: Different size."

let rec mapj_gen : type a b. (int -> a -> b) -> int -> int -> a t -> b t =
  fun f m i ->
  function
  | Empty -> Empty
  | Even t ->
    Even (mapj_gen (fun i (x, y) -> (f i x, f (i + m) y)) (2*m) i t)
  | Odd (z, t) ->
    Odd (f i z, mapj_gen (fun i (x, y) -> (f i x, f (i + m) y)) (2*m) (i + m) t)
let mapj f i w = mapj_gen f 1 i w

let rec comapj_rev_gen : type a b. (int -> a -> b) -> int -> int -> a t -> b t =
  fun f m i ->
  function
  | Empty -> Empty
  | Even t ->
    Even (comapj_rev_gen (fun i (y, x) -> (f (i - m) y, f i x)) (2*m) i t)
  | Odd (z, t) ->
    Odd (f i z,
         comapj_rev_gen (fun i (y, x) -> (f (i - m) y, f i x)) (2*m) (i - m) t)
let comapj_rev f j w = comapj_rev_gen f 1 (j - 1) w

let rec iter : type a. (a -> unit) -> a t -> unit = fun f -> function
  | Empty -> ()
  | Even t -> iter (fun (x, y) -> f x; f y) t
  | Odd (z, t) -> f z; iter (fun (x, y) -> f x; f y) t

let rec iter2 : type a b. (a -> b -> unit) -> a t -> b t -> unit =
  fun f tA' tB' ->
  match tA', tB' with
  | Empty, Empty -> ()
  | Even tA, Even tB -> iter2 (fun (xA, yA) (xB, yB) -> f xA xB; f yA yB) tA tB
  | Odd (zA, tA), Odd (zB, tB) ->
    f zA zB; iter2 (fun (xA, yA) (xB, yB) -> f xA xB; f yA yB) tA tB
  | Empty, _ | Even _, _ | Odd _, _ -> bad_arg "iter2: Different size."

let rec coiter_rev : type a. (a -> unit) -> a t -> unit = fun f -> function
  | Empty -> ()
  | Even t -> coiter_rev (fun (x, y) -> f x; f y) t
  | Odd (z, t) -> coiter_rev (fun (x, y) -> f x; f y) t; f z

let rec fold : type a. (a -> 'b -> 'b) -> a t -> 'b -> 'b = fun f -> function
  | Empty -> ident
  | Even t -> fold (fun (x, y) -> f y <@ f x) t
  | Odd (z, t) -> fold (fun (x, y) -> f y <@ f x) t <@ f z

let rec fold2 : type a b. (a -> b -> 'c -> 'c) -> a t -> b t -> 'c -> 'c =
  fun f tA' tB' ->
  match tA', tB' with
  | Empty, Empty -> ident
  | Even tA, Even tB ->
    fold2 (fun (xA, yA) (xB, yB) -> f yA yB <@ f xA xB) tA tB
  | Odd (zA, tA), Odd (zB, tB) ->
    fold2 (fun (xA, yA) (xB, yB) -> f yA yB <@ f xA xB) tA tB <@ f zA zB
  | Empty, _ | Even _, _ | Odd _, _ -> bad_arg "fold2: Different size."

let rec cofold_rev : type a. (a -> 'b -> 'b) -> a t -> 'b -> 'b = fun f ->
  function
  | Empty -> ident
  | Even t -> cofold_rev (fun (x, y) -> f y <@ f x) t
  | Odd (z, t) -> f z <@ cofold_rev (fun (x, y) -> f y <@ f x) t

let rec split : type a. a t -> a t * a t = function
  | Empty -> Empty, Empty
  | Odd (x, Empty) as t -> Empty, t
  | Even (Odd ((x, y), Empty)) -> Odd (x, Empty), Odd (y, Empty)
  | Odd (x, t) -> let tR, tC = split t in Odd (x, tR), Even tC
  | Even t     -> let tR, tC = split t in even tR, Even tC

let split_snd_length n =
  if n <= 1 then n else 1 lsl Prime_int.floor_log2 (2 * n / 3)

let rec cosplit : type a. a t -> a t * a t = function
  | Empty -> Empty, Empty
  | Odd (x, Empty) as t -> Empty, t
  | Even (Odd ((y, x), Empty)) -> Odd (x, Empty), Odd (y, Empty)
  | Odd (x, t) -> let tR, tC = cosplit t in Odd (x, tR), Even tC
  | Even t     -> let tR, tC = cosplit t in even tR, Even tC
