(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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

let rec search f = function
  | [] -> None
  | x :: xs -> match f x with None -> search f xs | y -> y

let rec fold f xs accu =
  match xs with
  | [] -> accu
  | x :: xs' -> fold f xs' (f x accu)

let zip ?(trunc = false) =
  let rec loop zs = function
    | (x :: xs, y :: ys) -> loop ((x, y) :: zs) (xs, ys)
    | ([], []) | _ when trunc -> rev zs
    | _ -> invalid_arg "Prime_list.zip: Lists have different length." in
  loop []

let unzip zs =
  let rec loop xs ys = function
    | (x, y) :: zs -> loop (x :: xs) (y :: ys) zs
    | [] -> (rev xs, rev ys) in
  loop [] [] zs

let fold_zip ?(trunc = false) f =
  let rec loop = function
    | (x :: xs, y :: ys) -> fun accu -> loop (xs, ys) (f (x, y) accu)
    | ([], []) | _ when trunc -> fun accu -> accu
    | _ -> invalid_arg "Prime_list.fold_zip: Lists have different length." in
  loop

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
