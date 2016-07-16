# Change Log

## [0.7.0]

### Incompatible Changes
- Add or change container `get` operations to be consistent with stdlib.
  This breaks backwards compatibility for `Prime_enumlist`, `Prime_enumset`,
  and `Prime_wallet`.
- Let `Prime_map.S.pop` return an option instead of raising `Not_found`.

### Added
- Function `Prime_priqueue.pop_min`
- Function `List.rev_flatten`
- Composition operators `@>` and `<@`, to replace the old.
- Function `Prime_retraction.elements`.
- Functions `app` to make-like containers, as an alternative to `find`.
- A stdlib compatible function `Prime_retraction.find`.
- Function `Prime_array.fmap`.
- Functions `Prime_enummap.pop` and `Prime_retraction.pop`.

### Deprecated
- All `get_o`, `get_e`, `find_o`, `find_e` functions.
- Composition operators `*>` and `*<`.
- `Prime_array.filter_map`.

[0.7.0]: https://github.com/paurkedal/ocaml-prime/compare/0.6.9...0.7.0
