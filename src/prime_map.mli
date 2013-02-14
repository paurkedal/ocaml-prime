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

(** Amendment to [Map]. *)

module type OrderedType = Map.OrderedType

module type S = sig
  include Map.S

  val fold2 : (key -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  (** [fold2 f m0 m1] returns the composition [f kₙ vₙ wₙ ∘ ⋯ ∘ f k₁ v₁ w₁]
      where [k₁, …, kₙ] are the coinciding indices of [m0] and [m1], and [v₁,
      …, vₙ] and [w₁, …, wₙ] are the correspoding values from [m0] and [m1],
      respectively. *)
end

module Make (K : OrderedType) : S with type key = K.t
