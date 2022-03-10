(* Copyright (C) 2020--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

type 'a t = 'a Seq.t

let const x =
  let rec node () = Seq.Cons (x, node) in
  node

let cons x xs () = Seq.Cons (x, xs)

let rec iterate f x () = Seq.Cons (x, iterate f (f x))

let rec iterate_while f x () =
  (match f x with
   | None -> Seq.Nil
   | Some y -> Seq.Cons (x, iterate_while f y))

let rec memoize xs =
  let node = lazy
    (match xs () with
     | Seq.Nil -> Seq.Nil
     | Seq.Cons (x, xs') -> Seq.Cons (x, memoize xs'))
  in
  fun () -> Lazy.force node

let rec fold f xs acc =
  (match xs () with
   | Seq.Nil -> acc
   | Seq.Cons (x, xs') -> fold f xs' (f x acc))

let rec find f xs =
  (match xs () with
   | Seq.Nil -> None
   | Seq.Cons (x, xs) -> if f x then Some x else find f xs)

let length =
  let rec loop n = function
   | Seq.Nil -> n
   | Seq.Cons (_, xs) -> loop (n + 1) (xs ())
  in
  fun xs -> loop 0 (xs ())

let count f =
  let rec loop n = function
   | Seq.Nil -> n
   | Seq.Cons (x, xs) -> loop (if f x then n + 1 else n) (xs ())
  in
  fun xs -> loop 0 (xs ())

let rec for_all f xs =
  (match xs () with
   | Seq.Nil -> true
   | Seq.Cons (x, xs') -> f x && for_all f xs')

let rec exists f xs =
  (match xs () with
   | Seq.Nil -> false
   | Seq.Cons (x, xs') -> f x || exists f xs')

let rec differentiate f x xs () =
  (match xs () with
   | Seq.Nil -> Seq.Nil
   | Seq.Cons (x', xs') -> Seq.Cons (f x x', differentiate f x' xs'))

let rec integrate f xs y () =
  (match xs () with
   | Seq.Nil -> Seq.Nil
   | Seq.Cons (x, xs') ->
      let y' = f x y in
      Seq.Cons (y', integrate f xs' y'))

let rec skip_upto n xs =
  if n < 0 then invalid_arg "Prime_seq.skip_upto" else
  if n = 0 then xs else
  fun () ->
    (match xs () with
     | Seq.Nil -> Seq.Nil
     | Seq.Cons (_, xs') -> skip_upto (n - 1) xs' ())

let rec skip_while f xs () =
  (match xs () with
   | Seq.Nil -> Seq.Nil
   | Seq.Cons (x, xs') ->
      if f x then skip_while f xs' () else Seq.Cons (x, xs'))

let rec take_upto n xs =
  if n < 0 then invalid_arg "Prime_seq.take_upto" else
  if n = 0 then Seq.empty else
  fun () ->
    (match xs () with
     | Seq.Nil -> Seq.Nil
     | Seq.Cons (x, xs') -> Seq.Cons (x, take_upto (n - 1) xs'))

let rec take_while f xs () =
  (match xs () with
   | Seq.Nil -> Seq.Nil
   | Seq.Cons (x, xs') ->
      if f x then Seq.Cons (x, take_while f xs') else Seq.Nil)

let rec cat xs ys () =
  (match xs () with
   | Seq.Nil -> ys ()
   | Seq.Cons (x, xs') -> Seq.Cons (x, cat xs' ys))

let rec flatten xss () =
  (match xss () with
   | Seq.Nil -> Seq.Nil
   | Seq.Cons (xs, xss') -> cat xs (flatten xss') ())

module Infix = struct
  let ( >>=* ) xs f = Seq.flat_map f xs
  let ( >|=* ) xs f = Seq.map f xs
end
