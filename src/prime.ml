(* Copyright (C) 2013--2014  Petter Urkedal <paurkedal@gmail.com>
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

type counit = {absurd : 'a. 'a}
let absurd z = z.absurd

let ident x = x
let konst x y = x
let ( *< ) f g x = f (g x)
let ( *> ) f g x = g (f x)
let ( |> ) x f = f x
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

let finally cleanup thunk =
  let r = try thunk ()
	  with xc -> cleanup (); raise xc in
  cleanup (); r
