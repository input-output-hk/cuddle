# Defining CDDL in Huddle

The Huddle modules in Haskell provide a way to define CDDL (Concise Data
Definition Language) using Haskell's higher-level capabilities. This guide,
based on the provided sources, will cover the key concepts and syntax for
defining CDDL in Huddle.

## Core Types
Huddle utilizes several core types to represent CDDL constructs:
- Huddle: The top-level type representing a collection of rules.
- HuddleItem: Represents individual items within a Huddle, such as rules, groups, or generic rules.
- Rule: A named type definition.
- Named: A type wrapper for associating a name, value, and optional description with an item.
- Value: A type representing primitive CBOR values.
- Group: Represents a collection of entries within a map or array.

## Language Extensions

Huddle makes use of some Haskell language extensions which liberalise aspects
of Haskell's syntax. We recommend enabling them whenever working with Huddle:

```haskell
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
```

In addition, if using hlint, we suggest disabling the following hints:

```haskell
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Evaluate" #-}

```
  
## Rules and Assignment
Rules are defined using the =:= operator. The left-hand side of the operator is
the rule name (a T.Text value), and the right-hand side is the type definition.

`ruleName =:= typeDefinition`

### Example:
```haskell
age =:= VUInt
```

## Maps
Maps are defined using the mp function and the ==> operator to specify key-value
pairs.

`mapName =:= mp [ key1 ==> value1, key2 ==> value2 ]`

### Example:
```haskell
location =:= mp [
  "latitude" ==> float,
  "longitude" ==> float
]
``` 

## Arrays
Arrays are defined using the arr function and the a function to indicate array elements.

`arrayName =:= arr [ a element1, a element2 ]`

### Example:
```haskell
point =:= arr [ a int, a int ]
```

## Groups
Groups are collections of entries within maps or arrays. They can be named using
the =:~ operator.

`groupName =:~ grp [ entry1, entry2 ]`

### Example:
```haskell
personalinfo =:~ grp [
  "name" ==> tstr,
  "age" ==> uint
]
```

## Choices

Huddle represents choices between types using the / operator.

### Example:
```haskell
value =:= int / tstr
```

Huddle does not have a direct equivalent for the CDDL // operator (group
choice). Instead, choices within arrays are represented by creating separate
array definitions and combining them using the / operator.

### Example:
```haskell
optionA =:= arr [ a int, a tstr ]
optionB =:= arr [ a tstr, a int ]
choice =:= optionA / optionB
```

## Quantifiers
Huddle provides functions to specify occurrence quantifiers for group entries
and array elements:
- `<+`: Lower bound
- `+>`: Upper bound
- `opt`: Optional (0 or 1 occurrences)

### Example:
```haskell
dat =:= arr [ 1 <+ a int +> 10 ] -- Array of 1 to 10 integers
```

## Comments
The comment function can be used to add descriptions to rules and group entries,
which will be included as comments in the generated CDDL.

### Example:
```haskell
person =:= comment "Represents a person" $ mp [
  "name" ==> VBytes & comment "Person's name",
  "age" ==> VUIntf
]
```

## Generic Rules
Huddle supports defining generic rules using the binding function.

### Example:
```haskell
message = binding $ \t -> "message" =:= {
  "type" ==> t,
  "payload" ==> any
}
```
  
## Converting to CDDL
The `toCDDL` and `toCDDLNoRoot` functions convert a Huddle definition to CDDL.
`toCDDL` generates a top-level root element, while `toCDDLNoRoot` skips the root
element.

## Example File (Conway.hs)
The `Conway.hs` example file showcases a practical application of Huddle to
define the CDDL for a specific data structure. The file defines numerous rules
and groups using the Huddle syntax and functions described above.
