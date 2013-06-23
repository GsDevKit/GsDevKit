*core-squeak
first
    "Answer the first element of the receiver.
    Raise an error if the collection is empty."

    ^ self at: 1  "raises error if size == 0"
