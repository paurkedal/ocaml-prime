## v0.9.3 - 2022-03-20

- Add iterators for `Prime_cache`.
- In `Prime_enummap`, rename `pop_min` to `pop_min_exn` and `pop_max` to
  `pop_max_exn`.
- Rename `search` and `search_s` to `find_map` and `find_map_s` in all
  modules and signatures where present.
- Switch to LGPL-3.0 Linking Exception.

## v0.9.2 - 2020-11-01

- Fix atomicity issue in `Prime_beacon`.
- Add `min_elt` and `max_elt` to `Prime_enumset`.
- Add `_exn` suffixes to `pop_min` and `pop_max` in `Prime_enumset`.

## v0.9.1 - 2020-09-20

- Add `Prime_list.compare` and `Prime_list.equal`.
- Add `Prime_enummap.find_opt` and `Prime_enummap.update`.
- Add `Prime_enumset.disjoint`.
- Add `Prime_seq`.
- Add sequence extraction for `Prime_enumset` and `Prime_enummap`.

## v0.9.0 - 2019-11-13

- Removed most deprecated functions from last release.
- Rename `Prime_char` predicates to indicate ASCII limitation and optimize.
- Renamed `fmap` to `filter_map` for consistency with stdlib.
- Removed `Unprime_list.cons` since it exists in stdlib since 4.03.

Internal:
- Updated to dune and opam 2.
- Minor test improvement for `Prime_int`.

## v0.8.2 - 2018-05-02

- Rename `_e` extensions to `_exn` many places for consistency with stdlib.
- Rename all `contains` and `contains_elt` to `mem` and `mem_elt` for
  consistency with stdlib.
- In `Prime_enumset`, rename `{min,max}_elt` to `{min,max}_elt_exn`.
- Add `List.rev_filter`.

## v0.8.1 - 2018-04-02

- Add integer functions: `pow`, `fact`, and `binom`.
- Add list functions `interfix` and `rev_interfix`.

## v0.8.0 - 2017-08-11

- Change composition operators to `(%)` and `(%>)` for consistency with
  `ppx_compose` and Batteries.
- In `Prime_list`, fix `Prime_list.iter2t` and deprecate `push`.
- In `Prime_string`, add `count` and rename `of_list` to `of_chars` and
  `to_list` to `to_chars`.
- Add a new `Prime_unknowable` module in `prime.unstable`.
