(* Copyright (C) 2013--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open List

let sample f n =
  if n < 0 then invalid_arg "Negative length passed to Prime_list.sample" else
  let rec loop i accu =
    if i < 0 then accu else
    loop (i - 1) (f i :: accu) in
  loop (n - 1) []

let push x xs = x :: xs

let of_option = function
  | None -> []
  | Some x -> [x]

let count f =
  let rec loop n = function
    | [] -> n
    | x :: xs -> loop (if f x then n + 1 else n) xs in
  loop 0

let rec search f = function
  | [] -> None
  | x :: xs -> match f x with None -> search f xs | y -> y

let rec fold f xs accu =
  match xs with
  | [] -> accu
  | x :: xs' -> fold f xs' (f x accu)

let rev_filter f xs =
  let rec loop acc = function
   | [] -> acc
   | x :: xs -> loop (if f x then x :: acc else acc) xs in
  loop [] xs

let fmap f xs =
  let rec loop ys = function
    | [] -> ys
    | x :: xs -> loop (match f x with None -> ys | Some y -> (y :: ys)) xs in
  rev (loop [] xs)

let flatten_map f xs =
  let rec loop ys = function
    | [] -> ys
    | x :: xs -> loop (rev_append (f x) ys) xs in
  rev (loop [] xs)

let rev_flatten xss = fold rev_append xss []

let rec iter2t f xs ys =
  match xs, ys with
  | [], _ | _, [] -> ()
  | x :: xs', y :: ys' -> f x y; iter2t f xs' ys'

let rec fold2 f xs ys accu =
  match xs, ys with
  | [], [] -> accu
  | x :: xs', y :: ys' -> fold2 f xs' ys' (f x y accu)
  | _ -> invalid_arg "Prime_list.fold2"

let rec fold2t f xs ys accu =
  match xs, ys with
  | [], _ | _, [] -> accu
  | x :: xs', y :: ys' -> fold2t f xs' ys' (f x y accu)

let rev_map2t f xs ys =
  let rec loop accu = function
    | [], _ | _, [] -> accu
    | x :: xs', y :: ys' -> loop (f x y :: accu) (xs', ys') in
  loop [] (xs, ys)

let map2t f xs ys = rev (rev_map2t f xs ys)

let rev_map_diff f xs =
  let rec loop accu xi = function
    | xj :: xs -> loop (f xi xj :: accu) xj xs
    | [] -> accu in
  match xs with
  | [] -> invalid_arg "Prime_list.map_diff: Empty list."
  | x :: xs -> loop [] x xs

let map_diff f xs = rev (rev_map_diff f xs)

let drop n xs =
  if n < 0 then invalid_arg "Prime_list.drop" else
  let rec loop n xs =
    if n = 0 then xs else
    match xs with
    | [] -> failwith "Prime_list.drop"
    | _ :: xs' -> loop (n - 1) xs' in
  loop n xs

let take n xs =
  if n < 0 then invalid_arg "Prime_list.take" else
  let rec loop zs = function
    | 0, _ -> rev zs
    | _, [] -> failwith "Prime_list.take"
    | n, y :: ys -> loop (y :: zs) (n - 1, ys) in
  loop [] (n, xs)

let rec drop_while f = function
  | [] -> []
  | x :: xs -> if f x then drop_while f xs else x :: xs

let take_while f xs =
  let rec loop zs = function
    | [] -> xs
    | y :: ys -> if f y then loop (y :: zs) ys else rev zs in
  loop [] xs

let rev_interfix x ys =
  let rec loop acc = function
   | [] -> acc
   | y :: ys -> loop (y :: x :: acc) ys in
  (match ys with
   | [] -> failwith "Prime_list.rev_interfix"
   | y :: ys -> loop [y] ys)

let interfix x ys = List.rev (rev_interfix x ys)

(* Deprecated *)
let filter_map = fmap
