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

(** A constructive stack-like container ({e prime.unstable}).

    This container provides functionality similar to arrays or stacks with
    constructive updates.  Random access, including modifications, have
    logarithmic complexity and functions are provided to add or remove
    elements from the front.

    A wallet is represented as an optional element and a wallet of pairs,
    {[type 'a t = Empty | Even of ('a * 'a) t | Odd 'a * ('a * 'a) t]}
    which amounts to a sequence of complete trees of increasing length. *)

type 'a t

val empty : 'a t
(** The empty wallet. *)

val is_empty : 'a t -> bool
(** [is_empty w] is true iff [w] is the empty wallet. *)

val singleton : 'a -> 'a t
(** [singleton x] has length 1 and contains the element [x]. *)

val sample : (int -> 'a) -> int -> 'a t
(** [sample f n] has length [n] and contains [f i] at [i]. *)

val length : 'a t -> int
(** [length w] is the number of elements of [w]. *)

val push : 'a -> 'a t -> 'a t
(** [push x w] is [w] with [x] added to the front. *)

val pop : 'a t -> 'a * 'a t
(** [pop w] is [x, w'] where [x] is the first element of [w] and [w'] contains
    the remaining elements in the original order.
    @raise Invalid_argument if [w] is empty. *)

val get : int -> 'a t -> 'a
(** [get i w] is element [i] of [w].
    @raise Invalid_argument if [i] is out of bounds. *)

val set : int -> 'a -> 'a t -> 'a t
(** [set i x w] is [w] with element [i] replaced by [x].
    @raise Invalid_argument if [i] is out of bounds. *)

val modify : int -> ('a -> 'a) -> 'a t -> 'a t
(** Given a [w] with [x] at [i], [modify i f w] is [w] with element [i]
    replaced by [f x].
    @raise Invalid_argument if [i] is out of bounds. *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all f w] is true iff [f x] is true for all [x] in [w]. *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [for_all2 f wA wB] is true iff [f xA xB] is true for each pair of [xA] and
    [xB] taken from the same position of [wA] and [wB], respectively.
    @raise Invalid_argument if [wA] and [wB] have different size. *)

val search : ('a -> 'b option) -> 'a t -> 'b option
(** [search f w] is the first [f x] different from [None] for [x] in [w], or
    [None] if no such [x] is found. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f w] is the image of [w] under [f]. *)

val mapj : (int -> 'a -> 'b) -> int -> 'a t -> 'b t
(** [mapj f w] is the wallet with [f i x] at [i] where [x] is element [i] of
    [w]. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f wA wB] is the wallet of [f xA xB] at [i] where [xA] and [xB] are
    the element [i] of [wA] and [wB], respectively.
    @raise Invalid_argument if [wA] and [wB] have different size. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f w] calls [f] on each element of [w] in order. *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter f wA wB] calls [f] on each pair of successive elements of [wA] and
    [wB].
    @raise Invalid_argument if [wA] and [wB] have different size. *)

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f w] is the composition of [f x] for [x] in [w], passed in order of
    occurrence. *)

val fold2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
(** [fold2 f wA wB] is the composition of [f xA xB] for [xA] and [xB] pairwise
    taken from [wA] and [wB] at the same successive positions. *)

val split : 'a t -> 'a t * 'a t
(** [split w] splits [w] into two wallets of comparable where the second is a
    complete tree.  In particular the second tree can be passed to algorithms
    using the opposite functions below, and similarly {!cosplit} returns as
    its second component a complete tree which can be used by the primary
    functions. *)

val split_snd_length : int -> int
(** [split_snd_length n] is the length of [snd (split w)] for any [w] of
    length [n]. *)

(** {2 Opposite Functions}

    These functions are not needed for normal usage.  They are variants of the
    main functions with internal trees reversed, and can be used to share
    [split] elements when two wallets are combined into a symmetric
    datastructure. *)

val copush : 'a -> 'a t -> 'a t
val copop : 'a t -> 'a * 'a t
val coget : int -> 'a t -> 'a
val coset : int -> 'a -> 'a t -> 'a t
val comodify : int -> ('a -> 'a) -> 'a t -> 'a t
val comapj_rev : (int -> 'a -> 'b) -> int -> 'a t -> 'b t
val coiter_rev : ('a -> unit) -> 'a t -> unit
val cofold_rev : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val cosplit : 'a t -> 'a t * 'a t
