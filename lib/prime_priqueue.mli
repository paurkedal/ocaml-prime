(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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

(** A pure priority queue with element removal.

    This implements priority queue in terms of a pure pairing heap.  The
    additional [remove] operation is supported by adding removal nodes
    alongside regular nodes, and erasing matching regular nodes as they get
    merged by the [remove_min] operation.

    This is most efficient when {!S.remove} is rare compared to {S.add} and
    {!S.remove_min}, otherwise a balanced tree may be better suited.  In the
    most feasible case, the deletion-nodes created by {!S.remove} gets flushed
    by {!S.remove_min} at a rate comparable to their creation.  If this is not
    the case, the {!S.gc} function should be called at suitable intervals to
    keep the space overhead within a constant factor. *)

module type S = sig
  type elt
  type t

  val empty : t
  (** The empty queue. *)

  val is_empty : t -> bool
  (** [is_empty q] is true iff [q] has no elements. *)

  val add : elt -> t -> t
  (** [add e q] is [q] with an additional element [e], keeping duplicates. *)

  val remove : elt -> t -> t
  (** [remove e q] is [q] with one [e] element removed, if any.  The
      implementation adds a node marking [e] for deletion, if an element [e]
      would not be immediatly accessible if present, but care is taken that
      later additions of [e] is unaffected by the deletion marker. *)

  val find_min : t -> elt
  (** [find_min q] is the minimal element of [q].
      @raise Not_found if [q] is empty. *)

  val remove_min : t -> t
  (** [remove_min q] is [q] with one less copy of its minimal element. *)

  val pop_min : t -> (elt * t) option
  (** [pop_min q] gives the minimum element of [q] and [q] without the same
      element if [q] is non-empty. *)

  val gc : t -> t
  (** [gc q] is an optimised queue containing the same elements as [q].  It is
      built from the empty queue, adding each element of [q]. *)
end

module Make (Elt : Set.OrderedType) : S with type elt = Elt.t
