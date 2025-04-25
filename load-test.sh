gen () { cabal run cuddle -- gen -r $1 -o example.cbor $2 }
pprint () { cbor2pretty.rb example.cbor }
validate () { cabal run cuddle -- validate-cbor -r $1 -c example.cbor $2 }
cuddle-test () { gen $1 $2; pprint; validate $1 $2 }
