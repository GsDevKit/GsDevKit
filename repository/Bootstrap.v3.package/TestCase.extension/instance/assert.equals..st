*bootstrap
assert: actual equals: expected
  ^ self
    assert: expected = actual
    description: (self comparingStringBetween: expected and: actual)