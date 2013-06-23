*core-squeak
asString
        "Convert to a String with Characters for each byte"

        | str index |
        str := String new: self size.
        1 to: self size do: [:i |str at: i put: (Character value: (self at: i))].
        ^str