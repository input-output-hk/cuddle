# Changelog for `cuddle`

## 1.1.0.0

* Change the order of fields in `GroupEntry`; the extension field is now the last field
* Add `IndexMappable` to help with traversing `CDDL` trees
* Add an index type parameter to all `CDDL` terms
* Remove `Codec.CBOR.Cuddle.CDDL.Prelude`
* Replace `cddlPrelude` with `cddlPostlude`, `prependPrelude` with `appendPostlude`
* Move `PTerm` to `Codec.CBOR.Cuddle.CDDL.CTree`
* Remove `CTreeRoot'`
* Changed the type in `CTreeRoot` to a map of resolved `CTree`s
* Changed the type of the first argument for `generateCBORTerm` and 
  `generateCBORTerm'` to `CTreeRoot`
* Removed all exports in `Codec.CBOR.Cuddle.CBOR.Validator` except for 
  `validateCBOR`, `validateCBOR'`, `CBORTermResult` and `CDDLResult`

## 1.0.0.0

* First official release to Hackage
* Added one more parameter to `BindingEnv`

## 0.5.0.0

* Implement a validator for CBOR terms.

## 0.4.0.0

* Rewrote the prettyprinter to make the output more human-readable.
* Added `collectFromInit` to make it possible to specify the order of
  definitions manually. `collectFrom` and `collectFromInit` now expect
  `HuddleItem`s in their arguments
* More Huddle terms now accept comments (e.g. `ArrayEntry`, `ArrayChoice`)
* Parser now preserves most comments.

## 0.3.6.0

* Support having keys in group entries. This is needed when using a group to
  define a map, or when wishing to include keys in for-use-in-array groups for
  documentation purposes. This may introduce problems with existing specifications
  where some type hints (using 'a') are needed to properly type entries in groups,
  where previously they were unambiguous.

  Note that it is not yet supported to use a group inside a map, where the
  issue of merging keys arises.

## O.3.5.0

* Add support for constraints on references and generic references.
* Add support for using references as range bounds. Note that this breaks
  backwards compatibility - because the range arguments are now more generic,
  additional hints are required to type literal numerics correctly. Typically
  this is most easily fixed by adding a call `int` for any numeric literals in
  ranges. An example is shown in `example/Conway.hs`

## 0.3.3.0

* Introduce HuddleM, another way to define a Huddle spec. This allows total
  control over the order that items are presented in the CDDL, at the cost
  of making it somewhat harder to re-use items (they need to be returned from
  the monad).

## 0.3.2.0

* Leading rather than trailing commas in the pretty printer.

## 0.3.1.0

* `collectFrom` now adds a first definition in the generated Huddle referencing
  the top level elements, to be compatible with the CDDL spec.

## 0.3.0.1

* Improvements in the pretty printer - various groups should be formatted more
  cleanly

## 0.3.0.0

* Rationalise the choice operators. Drop (//) and provide detailed comments
  explaining the use of (/).

## 0.1.0.0

* First version. Released on an unsuspecting world.

