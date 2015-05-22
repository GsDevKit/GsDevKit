tests - Integer
testGsIntegerRadix
  | aNumber rs |
  aNumber := '16#11' asNumber.
  self assert: 17 = aNumber.
  self assert: (SqNumberParser parse: '16#11') = 17