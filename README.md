<h1 align="center">Cuddle</h1>

Cuddle is a library for generating and manipulating [CDDL](https://datatracker.ietf.org/doc/html/rfc8610).

<p align="center">
  <a href="https://github.com/input-output-hk/cuddle/actions/workflows/ci.yml">
    <img alt="GitHub Workflow Status (master)" src="https://img.shields.io/github/actions/workflow/status/input-output-hk/cuddle/ci.yml?branch=master&style=for-the-badge" />
  </a>
</p>

## Supported features

Cuddle currently supports the following CDDL features:

- Groups
- Values
- Choices
- Maps
  - Structs
  - Tables
- Predefined CDDL types
- Tags
- Unwrapping
- Sockets/plugs
- Generics

### Partial support

- Representation types
  - Representation types are correctly parsed and formatted, but only certain
    types are understood in CBOR generation.
- Cuts
  - Cut syntax is parsed and formatted, but ignored for CBOR generation.
- Controls
  - Controls are correctly parsed and formatted, but only certain controls are
    understood in CBOR generation.
- Operator precedence
- Comments
  - Comments are currently stripped during formatting.

### Unsupported

- Escaping in bytestrings


## The cuddle tool

Included in this package is a command line tool for working with CDDL files. It
currently supports four functions:

- Formatting of CDDL files
- Validating that a CDDL file is legal
- Generating random CBOR terms matching CDDL productions
- Testing compliance of a CBOR file against a CDDL spec.

# Huddle

One of the principal features of Cuddle is the ability to define your CDDL in a
Haskell DSL, called Huddle. This offers the following benefits:

- Ability to use Haskell's abstraction facilities to define more complex CDDL
  constructions.
- Some measure of compile-time safety for your CDDL. Attempting to reference
  an undefined identifier will be a compile-time error, for example.
- The ability to modularise your configuration. CDDL lacks any real facility
  for splitting a CDDL spec amongst multiple files. We solve this instead using
  Haskell's module system.

Obviously, this comes with the downside of needing to sensibly mesh the
different abstraction facilities offered by Haskell and CDDL. We have tried to
find a balance where the Huddle code roughly matches the CDDL but gains many
of the advantages of writing in Haskell.

[A guide to writing Huddle](docs/huddle.md) goes into more details.
