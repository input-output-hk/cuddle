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
