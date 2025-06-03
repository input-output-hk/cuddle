# Revision history for cuddle

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## 0.3.0.0 --2024-07-25

* Rationalise the choice operators. Drop (//) and provide detailed comments
  explaining the use of (/).

## 0.3.0.1 -- 2024-07-31

* Improvements in the pretty printer - various groups should be formatted more
  cleanly

## 0.3.1.0 -- 2024-08-15

* `collectFrom` now adds a first definition in the generated Huddle referencing
  the top level elements, to be compatible with the CDDL spec.

## 0.3.2.0 -- 2024-09-11

* Leading rather than trailing commas in the pretty printer.

## 0.3.3.0 -- 2024-11-13

* Introduce HuddleM, another way to define a Huddle spec. This allows total
  control over the order that items are presented in the CDDL, at the cost
  of making it somewhat harder to re-use items (they need to be returned from
  the monad).

## O.3.5.0 -- 2024-11-25

* Add support for constraints on references and generic references.
* Add support for using references as range bounds. Note that this breaks
  backwards compatibility - because the range arguments are now more generic,
  additional hints are required to type literal numerics correctly. Typically
  this is most easily fixed by adding a call `int` for any numeric literals in
  ranges. An example is shown in `example/Conway.hs`

## 0.3.6.0 -- 2024-12-02
* Support having keys in group entries. This is needed when using a group to
  define a map, or when wishing to include keys in for-use-in-array groups for
  documentation purposes. This may introduce problems with existing specifications
  where some type hints (using 'a') are needed to properly type entries in groups,
  where previously they were unambiguous.

  Note that it is not yet supported to use a group inside a map, where the
  issue of merging keys arises.

## 0.4.0.0 -- 2025-05-02
* Rewrote the prettyprinter to make the output more human-readable.
* Added `collectFromInit` to make it possible to specify the order of
  definitions manually. `collectFrom` and `collectFromInit` now expect
  `HuddleItem`s in their arguments
* More Huddle terms now accept comments (e.g. `ArrayEntry`, `ArrayChoice`)
* Parser now preserves most comments.

## 0.5.0.0 -- 2025-06-03

* Implement a validator for CBOR terms.
