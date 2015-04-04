(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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

let with0 ?(n = 64) f =
  let buf = Buffer.create n in f buf; Buffer.contents buf
let with1 ?(n = 64) f x0 =
  let buf = Buffer.create n in f buf x0; Buffer.contents buf
let with2 ?(n = 64) f x0 x1 =
  let buf = Buffer.create n in f buf x0 x1; Buffer.contents buf
