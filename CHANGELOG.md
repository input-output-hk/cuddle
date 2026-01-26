# Changelog for `cuddle`

## 1.2.0.0

* Export `Type0`
* Removed `Codec.CBOR.Cuddle.Huddle.HuddleM`
* Add `format-cbor` subcommand
* Changed `--cbor` option of `validate` to a proper argument
* Added `binary` and `hex` output formats. Providing an output file argument to 
  `gen` no longer affects the output format.
* Add `format-cbor` subcommand
* Changed `--cbor` option of `validate` to a proper argument
* Added `binary` and `hex` output formats. Providing an output file argument to 
  `gen` no longer affects the output format.
* Add `seed` and `size` options to `generate` subcommand
* Replace `generateCBORTerm` with `generateFromName`
* Remove `generateCBORTerm'`
* Change the type of `withGenerator` and `CBORGenerator`
* Change `--rule` option of `gen` to an argument
* Add `withValidator`
* Add `showSimple`, `ValidatorStageSimple`
* Add `CBORValidator`, `CustomValidatorResult`, `HasValidator`
* Add custom validator field to `XRule CTreePhase`
* Added index type parameter to `CDDLResult`, `CBORTermResult`, `AMatchedItem`, `ANonMatchedItem`
* Remove `CDDL` and `Rule` type synonyms from `Codec.CBOR.Cuddle.CBOR.Validator`
* Add `--negative` option to `gen` for generating negative examples
* `CBORGenerator` now uses `AntiGen` instead of `Gen`
* Add `withAntiGen`

## 1.1.1.0

* Removed traces from `Codec.CBOR.Cuddle.CBOR.Validator`
* Add `isCBORTermResultValid`
* Add `Eq` instance to `XTerm ValidatorStage`, `XXCTree ValidatorStage` and `CBORTermResult`

## 1.1.0.0

* Change the type of field of `T2Group` to `GroupDef`
* Remove `Named` from exports
* Changed `T2Ref` to take a `Rule` instead of `Named Type0`
* Add `GroupDef`, `HIGroup` constructor now expects a `GroupDef` instead of `Named Group`
* Changed the following type synonyms to proper datatypes:
  - `GRuleDef` 
  - `GRuleCall`
* Removed `Codec.CBOR.Cuddle.Huddle.Optics`
* Changed the `comment` to take a `Comment` argument instead of `Text`
* Changed the following functions to take a `Name` instead of `Text`:
  - `(=:=)`
  - `(=:~)`
  - `(=::=)`
  - `unsafeIncludeFromHuddle`
* Changed the type of `name` field in `Named` to `Name`
* Remove the description field from `Named`
* Renamed the `name` field to `unName` in `Name`
* Add `HasName`
* Add `bool` to `Huddle` module
* Removed most `Show` instances from `Huddle` as they were unlawful
* Added `ctrTerm` and `ctrResult` field accessors to `CBORTermResult`
* Add `ValidatorStage`
* Remove `validateCBOR'`, copied its implementation to `validateCBOR`
* Add extra information field to `UnapplicableRule`
* Implement `IndexMappable` instance for
  - `HuddleStage` to `CTreePhase`
  - `HuddleStage` to `PrettyStage`
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

