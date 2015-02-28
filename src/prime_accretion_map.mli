(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

(** Accumulation of a monoid over a map ({i prime.unstable}). *)

include module type of Prime_accretion_map_intf

module Make1 (Key : Map.OrderedType) (Elt : Monoid1) :
  S1 with type key = Key.t
      and type 'a elt = 'a Elt.t
      and type 'a result = 'a Elt.t

module Make (Key : Map.OrderedType) (Elt : Monoid) :
  S with type key = Key.t
     and type elt = Elt.t
     and type result = Elt.t

module MakeG1 (Key : Map.OrderedType) (Elt : MonoidG1) :
  S1 with type key = Key.t
      and type 'a elt = 'a Elt.generator
      and type 'a result = 'a Elt.t

module MakeG (Key : Map.OrderedType) (Elt : MonoidG) :
  S with type key = Key.t
     and type elt = Elt.generator
     and type result = Elt.t
