(* Copyright (C) 2013--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Unprime
open Unprime_char
open Unprime_list
open Unprime_string
open OUnit

let print_string_list xs =
  "[" ^ String.concat "; " (List.map String.escaped xs) ^ "]"
let assert_eq_int = OUnit.assert_equal ~printer:string_of_int
let assert_eq_string = OUnit.assert_equal ~printer:String.escaped
let assert_eq_string_list = OUnit.assert_equal ~printer:print_string_list

let run () =
  let s = "I won't make bugs.  I won't make bugs.  I won't make bugs." in
  let s' = "IwontmakebugsIwontmakebugsIwontmakebugs" in
  let n = String.length s in
  let xs = String.to_list s in
  assert_eq_string s (String.of_list xs);
  assert_equal (List.rev xs) (String.fold List.push s []);
  assert (String.for_all Char.is_ascii s);
  assert (not (String.for_all Char.is_alpha s));
  assert (String.exists Char.is_punct s);
  assert (not (String.exists Char.is_digit s));
  assert_equal (s') (String.filter Char.is_alpha s);

  assert_eq_int 12 (String.skip_while Char.is_alpha s 8);
  assert_eq_int 12 (String.skip_until Char.is_space s 8);
  assert_eq_int 8 (String.rskip_while Char.is_alpha s 12);
  assert_eq_int 8 (String.rskip_until Char.is_space s 12);
  assert_eq_int n (String.skip_while Char.is_ascii s 0);
  assert_eq_int 0 (String.rskip_while Char.is_ascii s n);

  for i = 0 to n do
    assert_equal (Some i) (String.skip_affix "" s i);
    assert_equal (Some i) (String.rskip_affix "" s i);
    assert_equal None (String.skip_affix "$" s i)
  done;
  assert_equal (Some 2) (String.skip_affix "->" "->" 0);
  assert_equal (Some 3) (String.skip_affix "->" "a->b" 0);
  assert_equal (Some 3) (String.skip_affix "->" "a->b" 1);
  assert_equal None (String.skip_affix "->" "a->b" 2);
  assert_equal (Some 0) (String.rskip_affix "->" "->" 2);
  assert_equal (Some 1) (String.rskip_affix "->" "a->b" 4);
  assert_equal (Some 1) (String.rskip_affix "->" "a->b" 3);
  assert_equal None (String.rskip_affix "->" "a->b" 2);

  assert_eq_string "" (String.slice 8 8 s);
  assert_eq_string "make" (String.slice 8 12 s);
  assert_eq_string "bugs." (String.slice (n - 5) n s);
  assert (String.has_prefix "" s);
  assert (String.has_prefix "I won't" s);
  assert (not (String.has_prefix "n" s));
  assert (String.has_suffix "" s);
  assert (String.has_suffix "bugs." s);
  assert (not (String.has_suffix "?" s));
  assert (String.has_slice 8 "make" s);

  assert_equal (Some ("", "a")) (String.cut_affix "" "a");
  assert_equal (Some ("", "")) (String.cut_affix "->" "->");
  assert_equal (Some ("a", "b->c")) (String.cut_affix "->" "a->b->c");
  assert_equal None (String.cut_affix "--" "a-a");
  assert_equal (Some ("a", "")) (String.rcut_affix "" "a");
  assert_equal (Some ("", "")) (String.rcut_affix "->" "->");
  assert_equal (Some ("a->b", "c")) (String.rcut_affix "->" "a->b->c");
  assert_equal None (String.rcut_affix "--" "a-a");

  assert_eq_string_list [] (String.chop_affix "->" "");
  assert_eq_string_list ["a"] (String.chop_affix "->" "a");
  assert_eq_string_list [""; ""] (String.chop_affix "->" "->");
  assert_eq_string_list ["a"; ""] (String.chop_affix "->" "a->");
  assert_eq_string_list [""; "b"] (String.chop_affix "->" "->b");
  assert_eq_string_list ["a"; "b"] (String.chop_affix "->" "a->b");
  assert_eq_string_list ["a-";"b-";"c+"] (String.chop_affix "->" "a-->b-->c+");

  assert_equal None (String.cut_consecutive Char.is_space "");
  assert_equal None (String.cut_consecutive Char.is_space "  ");
  assert_equal (Some ("","< >")) (String.cut_consecutive Char.is_space " < >");
  assert_equal (Some ("< >","")) (String.rcut_consecutive Char.is_space "< > ");
  assert_equal (Some ("<>", "< >"))
               (String.cut_consecutive Char.is_space "<> < >");
  assert_equal (Some ("< >", "<>"))
               (String.rcut_consecutive Char.is_space "< > <>");

  assert_eq_string_list [] (String.chop_consecutive Char.is_space "");
  assert_eq_string_list [] (String.chop_consecutive Char.is_space "  ");
  assert_eq_string_list ["three"; "small"; "words"]
    (String.chop_consecutive Char.is_space "three small words");
  assert_eq_string_list ["three"; "small"; "words"]
    (String.chop_consecutive Char.is_space "  three   small  words  ");

  ()
