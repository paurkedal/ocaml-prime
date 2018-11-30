(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Memory cache based on hash tables ({e prime.unstable}). *)

type ('a, 'b) t

val create : cache_metric: Prime_cache_metric.t -> int -> ('a, 'b) t
(** [create ~cache_metric n] creates a cache initially scaled for about [n]
    entries.
    @param cache_metric The heuristics for when to retain a cache entry. *)

val clear : ('a, 'b) t -> unit
(** [clear ct] removes all cached entries from [ct]. *)

val app : ('a, 'b) t -> 'a -> 'b option
(** [app ck k] is the cached mapping of [k] if any. *)

val find : ('a, 'b) t -> 'a -> 'b
(** [find ct k] returnes the object associated with [k].
    @raise Not_found if [k] has no associated object. *)

val replace : ('a, 'b) t -> float -> 'a -> 'b -> unit
(** [replace ct g k v] replaces the mapping for [k] with [v] and associates it
    with a grade [g]. *)

val remove : ('a, 'b) t -> 'a -> unit
(** [remove ct k] removes any entry cached under [k] from [ct].  Does nothing
    if [k] is not in the cache. *)
