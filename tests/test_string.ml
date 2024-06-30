(* Copyright (C) 2013--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

module A = Alcotest.V1
open Unprime_char
open Unprime_list
open Unprime_string

let test_misc () =
  let s = "I won't make bugs.  I won't make bugs.  I won't make bugs." in
  let s' = "IwontmakebugsIwontmakebugsIwontmakebugs" in
  let n = String.length s in
  let xs = String.to_chars s in
  A.(check string) "of_chars" s (String.of_chars xs);
  A.(check (list char)) "fold" (List.rev xs) (String.fold List.cons s []);
  A.(check bool) "all ascii" true (String.for_all Char.is_ascii s);
  A.(check bool) "not all alpha" false (String.for_all Char.is_ascii_alpha s);
  A.(check bool) "exists punct" true (String.exists Char.is_ascii_punct s);
  A.(check bool) "not exists digit" false (String.exists Char.is_ascii_digit s);
  A.(check string) "filter alpha" s' (String.filter Char.is_ascii_alpha s);

  A.(check int) "skip1" 12 (String.skip_while Char.is_ascii_alpha s 8);
  A.(check int) "skip2" 12 (String.skip_until Char.is_ascii_space s 8);
  A.(check int) "skip3" 8 (String.rskip_while Char.is_ascii_alpha s 12);
  A.(check int) "skip4" 8 (String.rskip_until Char.is_ascii_space s 12);
  A.(check int) "skip5" n (String.skip_while Char.is_ascii s 0);
  A.(check int) "skip6" 0 (String.rskip_while Char.is_ascii s n);

  for i = 0 to n do
    A.(check (option int)) "skipaffi1" (Some i) (String.skip_affix "" s i);
    A.(check (option int)) "skipaffi2" (Some i) (String.rskip_affix "" s i);
    A.(check (option int)) "skipaffi3" None (String.skip_affix "$" s i)
  done;
  A.(check (option int)) "skipaff1" (Some 2) (String.skip_affix "->" "->" 0);
  A.(check (option int)) "skipaff2" (Some 3) (String.skip_affix "->" "a->b" 0);
  A.(check (option int)) "skipaff3" (Some 3) (String.skip_affix "->" "a->b" 1);
  A.(check (option int)) "skipaff4" None (String.skip_affix "->" "a->b" 2);
  A.(check (option int)) "skipaff5" (Some 0) (String.rskip_affix "->" "->" 2);
  A.(check (option int)) "skipaff6" (Some 1) (String.rskip_affix "->" "a->b" 4);
  A.(check (option int)) "skipaff7" (Some 1) (String.rskip_affix "->" "a->b" 3);
  A.(check (option int)) "skipaff8" None (String.rskip_affix "->" "a->b" 2);

  A.(check string) "slice 0" "" (String.slice 8 8 s);
  A.(check string) "slice 1" "make" (String.slice 8 12 s);
  A.(check string) "slice 2" "bugs." (String.slice (n - 5) n s);
  A.(check bool) "has_prefix 1" true (String.has_prefix "" s);
  A.(check bool) "has_prefix 2" true (String.has_prefix "I won't" s);
  A.(check bool) "has_prefix 3" false (String.has_prefix "n" s);
  A.(check bool) "has_suffix 1" true (String.has_suffix "" s);
  A.(check bool) "has_suffix 2" true (String.has_suffix "bugs." s);
  A.(check bool) "has_suffix 3" false (String.has_suffix "?" s);
  A.(check bool) "has_slice" true (String.has_slice 8 "make" s);

  A.(check (option (pair string string))) "cut_affix 1" (Some ("", "a"))
    (String.cut_affix "" "a");
  A.(check (option (pair string string))) "cut_affix 2" (Some ("", ""))
    (String.cut_affix "->" "->");
  A.(check (option (pair string string))) "cut_affix 3" (Some ("a", "b->c"))
    (String.cut_affix "->" "a->b->c");
  A.(check (option (pair string string))) "cut_affix 4" None
    (String.cut_affix "--" "a-a");
  A.(check (option (pair string string))) "rcut_affix 1" (Some ("a", ""))
    (String.rcut_affix "" "a");
  A.(check (option (pair string string))) "rcut_affix 2" (Some ("", ""))
    (String.rcut_affix "->" "->");
  A.(check (option (pair string string))) "rcut_affix 3" (Some ("a->b", "c"))
    (String.rcut_affix "->" "a->b->c");
  A.(check (option (pair string string))) "rcut_affix 4" None
    (String.rcut_affix "--" "a-a");

  A.(check (list string)) "chop_affix 1" []
    (String.chop_affix "->" "");
  A.(check (list string)) "chop_affix 2" ["a"]
    (String.chop_affix "->" "a");
  A.(check (list string)) "chop_affix 3" [""; ""]
    (String.chop_affix "->" "->");
  A.(check (list string)) "chop_affix 4" ["a"; ""]
    (String.chop_affix "->" "a->");
  A.(check (list string)) "chop_affix 5" [""; "b"]
    (String.chop_affix "->" "->b");
  A.(check (list string)) "chop_affix 6" ["a"; "b"]
    (String.chop_affix "->" "a->b");
  A.(check (list string)) "chop_affix 7" ["a-";"b-";"c+"]
    (String.chop_affix "->" "a-->b-->c+");

  A.(check (option (pair string string))) "cut_consecutive 1"
    None
    (String.cut_consecutive Char.is_ascii_space "");
  A.(check (option (pair string string))) "cut_consecutive 2"
    None
    (String.cut_consecutive Char.is_ascii_space "  ");
  A.(check (option (pair string string))) "cut_consecutive 3"
    (Some ("","< >"))
    (String.cut_consecutive Char.is_ascii_space " < >");
  A.(check (option (pair string string))) "cut_consecutive 4"
    (Some ("< >",""))
    (String.rcut_consecutive Char.is_ascii_space "< > ");
  A.(check (option (pair string string))) "rcut_consecutive 1"
    (Some ("<>", "< >"))
    (String.cut_consecutive Char.is_ascii_space "<> < >");
  A.(check (option (pair string string))) "rcut_consecutive 2"
    (Some ("< >", "<>"))
    (String.rcut_consecutive Char.is_ascii_space "< > <>");

  A.(check (list string)) "chop_consecutive 1" []
    (String.chop_consecutive Char.is_ascii_space "");
  A.(check (list string)) "chop_consecutive 2" []
    (String.chop_consecutive Char.is_ascii_space "  ");
  A.(check (list string)) "chop_consecutive 3" ["three"; "small"; "words"]
    (String.chop_consecutive Char.is_ascii_space "three small words");
  A.(check (list string)) "chop_consecutive 4" ["three"; "small"; "words"]
    (String.chop_consecutive Char.is_ascii_space "  three   small  words  ");

  ()

let test_cases = [
  A.test_case "misc" `Quick test_misc;
]
