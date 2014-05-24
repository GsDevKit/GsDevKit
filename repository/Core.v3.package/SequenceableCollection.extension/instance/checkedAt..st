*core-squeak
checkedAt: index
    ^ self at: index  "at: will signal error if offset > self size"