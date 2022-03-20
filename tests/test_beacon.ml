(* Copyright (C) 2020--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

module Beacon = Prime_beacon.Make (struct

  let cache_hertz = Int64.to_float ExtUnix.Specific.(sysconf CLK_TCK)

  let cache_metric =
    let current_time () =
      let tms = Unix.times () in
      Unix.(tms.tms_utime +. tms.tms_stime)
    in
    let current_memory_pressure () = cache_hertz in
    Prime_cache_metric.create ~current_time ~current_memory_pressure ()

end)

type t = {
  beacon: Beacon.t;
  serial: int;
}

let run () =
  let dummy = {beacon = Beacon.dummy; serial = -1} in
  let retained = Array.make 10_000 dummy in
  for _ = 1 to 1_000_000 do
    let i = Random.int 10_000 in
    let grade = Random.float 9.99 +. 0.01 in
    retained.(i) <- Beacon.embed grade (fun beacon -> {beacon; serial = i})
  done
