*core-squeak
nextNumber: n put: v 
    "Append to the receiver the argument, v, which is a positive 
    Integer, as the next n bytes.
    Possibly pad with leading zeros."

    v < 0 ifTrue:[ ArgumentError signal:'expected a positive Integer'].
    1 to: n do: [:i | self nextPut: (v digitAt: n+1-i)].
    ^ v