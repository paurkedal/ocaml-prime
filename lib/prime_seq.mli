(* Copyright (C) 2020  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Amendment to the standard library [Seq] structure.  This does not include
    the original functions, see {!Unprime_seq} for a full replacement.

    In the following, sequences are denoted with angle brackets as [⟨x₁; ...;
    xₙ⟩].  Despite this notation, we allow for unbounded sequences as
    long as inspecting a finite subsequence is sufficient to carry out the
    computation.  The implied limit [n → ∞] is merely formal, and serves to
    avoid duplicating formulas for finite and infinite cases. *)

type 'a t = 'a Seq.t


(** {2 Construction} *)

val const : 'a -> 'a t
(** [const x] is the infinite sequence where every element is [x]. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x xs] is the sequence of the element [x] followed by the elemnts of
    [xs]. *)

val iterate : ('a -> 'a) -> 'a -> 'a t
(** Given an endofunction [f] and a starting point [x], [iterate f x] is the
    infinite sequence [⟨x; f x; f (f x); f (f (f x)); ...⟩] of iterated
    applications of [f] starting from the point [x]. *)

val iterate_while : ('a -> 'a option) -> 'a -> 'a t
(** Given a partial endofunction [f : 'a -> 'a] completed as [f' : 'a -> 'a
    option], and a starting point [x], [iterate_while f' x] is the finite or
    infinite sequence [⟨x; f x; f (f x); f (f (f x)); ...⟩], terminated where
    [f] would have been applied outside its domain. *)

val memoize : 'a t -> 'a t
(** [memoize] is the identity function on sequences, except that it ensures that
    each step in the computation involved in the original sequence is performed
    only once, on demand.  This will incur a memory overhead proportional to the
    distance from the earliest still retained node to the lastest visited
    node. *)


(** {2 Consumption} *)

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f ⟨x₁; ...; xₙ⟩] is the composition [f xₙ ∘ ... ∘ f x₁]. *)

val find : ('a -> bool) -> 'a t -> 'a option
(** [find f xs] is the first [x] in [xs], if any, such that [f x] holds. *)

val length : 'a t -> int
(** [length xs] is the number of elements in [xs]. *)

val count : ('a -> bool) -> 'a t -> int
(** [count f xs] is the number of elements [x] in [xs] for which [f x] holds. *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all f xs] is true iff [f x] is true for all [x] in [xs]. *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists f xs] is true iff [f x] is true for some [x] in [xs]. *)


(** {2 Slicing} *)

val skip_upto : int -> 'a t -> 'a t
(** [skip_upto n xs] is the subsequence following the first [n] elements of
    [xs], or the empty sequence if [xs] has less than [n] elements.

    @raise Invalid_argument if the first argument is negative. *)

val skip_while : ('a -> bool) -> 'a t -> 'a t
(** [skip_while f xs] is subsequence following the longest prefix of [xs] for
    which each element [x] validates [f x]. *)

val take_upto : int -> 'a t -> 'a t
(** [take_upto n xs] is the sequence of the first [n] elements of [xs], or as
    many as present.

    @raise Invalid_argument if the first argument is negative. *)

val take_while : ('a -> bool) -> 'a t -> 'a t
(** [take_while f xs] is the longest prefix of [xs] for which each element [x]
    validates [f x]. *)


(** {2 Transformation} *)

val differentiate : ('a -> 'a -> 'b) -> 'a -> 'a t -> 'b t
(** [differentiate f x₀ ⟨x₁; ...; xₙ⟩] is the sequence [⟨f x₀ x₁; ...; f xₙ₋₁
    xₙ⟩]. *)

val integrate : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b t
(** [integrate f ⟨v₁; ...; vₙ⟩ x₀] is [⟨x₁; ...; xₙ⟩] where [xᵢ = f vᵢ xᵢ₋₁] for
    [i ∈ {1, ..., n}].

    This can be seen an opposite operation of {!differentiate} in the sense that
    given two functinos [f : α -> β -> β] and [f' : β -> β -> α] such that [f' x
    (f v x) = v] for all [x : β] and [v : α], then [differentiate f' x₀
    (integrate f ⟨v₁; ...; vₙ⟩ x₀) = ⟨v₁; ...; vₙ⟩]. *)


(** {2 Combination} *)

val cat : 'a t -> 'a t -> 'a t
(** [cat xs ys] is the sequence of elements of [xs] followed by elements of
    [ys]. *)

val flatten : 'a t t -> 'a t
(** Given a sequence of sequences [xss], [flatten xss] is the sequence formed by
    concatenating elements of each nestded sequence of [xss], preserving order
    at both levels.  That is, [flatten ⟨xs₁; ...; xsₙ⟩] is [cat xs₁ @@ ...  @@
    cat xsₙ₋₁ @@ xsₙ] *)


(** {2 Operator Shortcuts} *)

module Infix : sig
  val ( >>=* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [xs >>=* f] is [Seq.flat_map f xs]. *)

  val ( >|=* ) : 'a t -> ('a -> 'b) -> 'b t
  (** [xs >|=* f] is [Seq.map f xs]. *)
end
