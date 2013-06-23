Accessing
next
 "Answer the next random number."

"Real Random Number generator Version 2 from Park and Miller, 1988"
  hi := self seed quo: q.
  lo := seed - (q * hi).
  seed := (a * lo) - (r * hi).
  seed > 0 ifFalse: [
    seed := seed + m].
  ^ seed / m.