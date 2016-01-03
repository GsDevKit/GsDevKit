*gssqueakcommon-testsv30
testPrimesUpTo
  "this test is green for nn>25000, see #testLargePrimesUpTo"

  | primes nn |
  primes := Integer primesUpTo: 100.
  self
    assert:
      primes
        =
          #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97).
  primes := Integer primesUpTo: 5.	"upTo: semantics means 'non-inclusive'"
  self assert: primes = #(2 3).
  nn := 5.
  self deny: (Integer primesUpTo: nn) last = nn.
  self assert: (Integer primesUpTo: nn + 1) last = nn