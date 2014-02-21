*squeak
compare: aString caseSensitive: aBool
        "Answer a comparison code telling how the receiver sorts relative to
        aString: 1 - before
        2 - equal
        3 - after.
        "
        |len|
        len := self size min: aString size.
        ^(self _compare: aString caseInsensitive: aBool not from: 1 to: len) + 2