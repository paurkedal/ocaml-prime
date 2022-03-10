(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Accumulation of a monoid over a map ({e prime.unstable}).

    Given a monoid operation, say ∗, these map-like structures maintains the
    of nested application of the operator over the elements of the map in
    key-order.  E.g. given 5 elements inserted with keys such that the order
    is {i a}, ..., {i e}, then the accumulated result may be computed as ({i
    a} ∗ {i b}) ∗ ({i c} ∗ ({i d} ∗ {i e})) or any other associative
    combination, yielding the same result.  The map is represented as a
    balanced tree, storing intermediate results, so adding or removing
    elements is amortized {i O}(log {i n}).

    There are four functors depending on parametricity of the element type and
    whether or not the generator type is explicit. *)

include module type of Prime_accretion_map_intf

(** Accretion map of a polymorphic monoid type. *)
module Make1 (Key : Map.OrderedType) (Elt : Monoid1) :
  S1 with type key = Key.t
      and type 'a elt = 'a Elt.t
      and type 'a result = 'a Elt.t

(** Accretion map of a monomorphic monoid type. *)
module Make (Key : Map.OrderedType) (Elt : Monoid) :
  S with type key = Key.t
     and type elt = Elt.t
     and type result = Elt.t

(** Accretion map of a polymorphic monoid type with an explicit generator. *)
module MakeG1 (Key : Map.OrderedType) (Elt : MonoidG1) :
  S1 with type key = Key.t
      and type 'a elt = 'a Elt.generator
      and type 'a result = 'a Elt.t

(** Accretion map of a monomorphic monoid type with an explicit generator. *)
module MakeG (Key : Map.OrderedType) (Elt : MonoidG) :
  S with type key = Key.t
     and type elt = Elt.generator
     and type result = Elt.t
