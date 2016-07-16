(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

type 'b element = {
  e_value : 'b;
  e_grade : float;
  mutable e_access_count : int;
  mutable e_access_start : float;
}

type ('a, 'b) t = {
  c_tbl : ('a, 'b element) Hashtbl.t;
  mutable c_add_count : int;
  c_cm : Prime_cache_metric.t;
}

let scrub c =
  let cs = Prime_cache_metric.check_start c.c_cm in
  let check k e ks =
    if Prime_cache_metric.check cs e.e_access_count e.e_access_start e.e_grade
    then ks
    else k :: ks in
  [] |> Hashtbl.fold check c.c_tbl
     |> List.iter (fun k -> Hashtbl.remove c.c_tbl k);
  Prime_cache_metric.check_stop cs

let monitor wc =
  let alarm_r = ref None in
  let on_gc () =
    match Weak.get wc 0 with
    | Some c -> scrub c
    | None -> Prime_option.iter Gc.delete_alarm !alarm_r in
  let alarm = Gc.create_alarm on_gc in
  alarm_r := Some alarm

let create ~cache_metric n =
  let c = {
    c_tbl = Hashtbl.create n;
    c_add_count = 0;
    c_cm = cache_metric;
  } in
  let wc = Weak.create 1 in
  Weak.set wc 0 (Some c);
  monitor wc; c

let clear c =
  Hashtbl.reset c.c_tbl;
  c.c_add_count <- 0

let charge c e =
  e.e_access_start <-
    Prime_cache_metric.access_step c.c_cm e.e_access_count e.e_access_start;
  e.e_access_count <- e.e_access_count + 1

let find c k = let e = Hashtbl.find c.c_tbl k in charge c e; e.e_value

let app c k = try Some (find c k) with Not_found -> None
let find_o = app

let replace c g k v =
  let e = {
    e_value = v;
    e_grade = g;
    e_access_count = 1;
    e_access_start = Prime_cache_metric.access_init c.c_cm;
  } in
  Hashtbl.replace c.c_tbl k e;
  c.c_add_count <- c.c_add_count + 1

let remove c k = Hashtbl.remove c.c_tbl k
