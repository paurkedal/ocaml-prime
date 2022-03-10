(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Helpers for standard library channels. *)

val with_file_in : (in_channel -> 'a) -> string -> 'a
(** [with_file_in f fp] is [f ic] where [ic] is a channel opened for input
    from the file at [fp].  [ic] is closed when [f] returns or raises an
    exception. *)

val with_file_out : (out_channel -> 'a) -> string -> 'a
(** [with_file_out f fp] calls [f oc] where [oc] is a channel opened for
    output to [fp].  [oc] is closed when [f] returns or raises an exception.
    In the latter case the file is also removed. *)

val with1_file_in : (in_channel -> 'a -> 'b) -> string -> 'a -> 'b
(** [with1_file_in f fp acc] is [f ic acc] where [ic] is a channel opened for
    reading from [fp] during the call.  Due to the effect involved, this is
    not a special case of {!with_file_in}. *)

val with1_file_out : (out_channel -> 'a -> 'b) -> string -> 'a -> 'b
(** [with1_file_out f fp acc] is [f oc acc] where [oc] is a channel opened for
    writing to [fp] during the call.  Due to the effect involved, this is not
    a special case of {!with_file_out}. *)

val fold_input : ?close: bool -> (in_channel -> 'a -> 'a) ->
                                 in_channel -> 'a -> 'a
(** [fold_input f ic] forms the composition of successive calls to [f ic]
    until [End_of_file] is raised.
    @param close If true, close [ic] before returning, including if an
           exception occurs while evaluating [f ic acc] for some [acc]. *)

val iter_input : ?close: bool -> (in_channel -> unit) -> in_channel -> unit
(** [iter_input f ic] calls [f ic] until [End_of_file] is raised.
    @param close If true, close [ic] before returning, including if an
           exception occurs while executing [f ic]. *)
