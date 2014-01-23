(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

let rec pop : type a. a t -> a * a t = function
  | Empty -> invalid_arg "pop: Empty."
  | Even t -> let (x, y), t' = pop t in x, Odd (y, t')
  | Odd (z, Empty) -> z, Empty
  | Odd (z, t) -> z, Even t

let sample f n =
  let rec loop n w = if n < 0 then w else loop (n - 1) (push (f n) w) in
  loop (n - 1) empty

let rec get : type a. int -> a t -> a = fun i -> function
  | Empty -> invalid_arg "get: Index out of bounds."
  | Even t -> (if i land 1 = 0 then fst else snd) (get (i lsr 1) t)
  | Odd (z, t) ->
    if i = 0 then z else
    let j = i - 1 in
    (if j land 1 = 0 then fst else snd) (get (j lsr 1) t)

let rec modify : type a. int -> (a -> a) -> a t -> a t = fun i f -> function
  | Empty -> invalid_arg "set: Index out of bounds."
  | Even t ->
    let h (x, y) = if i land 1 = 0 then (f x, y) else (x, f y) in
    Even (modify (i lsr 1) h t)
  | Odd (x, t) ->
    if i = 0 then Odd (f x, t) else
    let j = i - 1 in
    let h (x, y) = if j land 1 = 0 then (f x, y) else (x, f y) in
    Odd (x, modify (j lsr 1) h t)

let set i x = modify i (fun _ -> x)

let rec map : type a b. (a -> b) -> a t -> b t = fun f -> function
  | Empty -> Empty
  | Even t -> Even (map (fun (x, y) -> (f x, f y)) t)
  | Odd (z, t) -> Odd (f z, map (fun (x, y) -> (f x, f y)) t)

let rec iter : type a. (a -> unit) -> a t -> unit = fun f -> function
  | Empty -> ()
  | Even t -> iter (fun (x, y) -> f x; f y) t
  | Odd (z, t) -> f z; iter (fun (x, y) -> f x; f y) t

let rec fold : type a. (a -> 'b -> 'b) -> a t -> 'b -> 'b = fun f -> function
  | Empty -> ident
  | Even t -> fold (fun (x, y) -> f y *< f x) t
  | Odd (z, t) -> fold (fun (x, y) -> f y *< f x) t *< f z
