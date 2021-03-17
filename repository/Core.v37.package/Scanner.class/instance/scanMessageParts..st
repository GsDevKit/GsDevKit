public access
scanMessageParts: sourceString
    "Return an array of the form (comment keyword comment arg comment keyword comment arg comment) for the message pattern of this method.  Courtesy of Ted Kaehler, June 1999"

    | coll nonKeywords |
    coll := OrderedCollection new.
    self scan: sourceString asString readStream.
    nonKeywords := 0.
    [ tokenType == #'doIt' ]
        whileFalse: [ 
            (currentComment == nil or: [ currentComment isEmpty ])
                ifTrue: [ coll addLast: nil ]
                ifFalse: [ 
                    coll addLast: currentComment removeFirst.
                    [ currentComment isEmpty ]
                        whileFalse: [ coll at: coll size put: coll last , ' ' , currentComment removeFirst ] ].
            (tokenType == #'number'
                or: [ 
                    token isArray
                        or: [ token numArgs < 1 or: [ (token = #'|' and: [ coll size > 1 ]) or: [ token = #'<' and: [ coll size > 1 ] ] ] ] ])
                ifTrue: [ 
                    (nonKeywords := nonKeywords + 1) > 1
                        ifTrue: [ ^ coll ] ]
                ifFalse: [ nonKeywords := 0 ].	"done with header"
            coll addLast: token.
            self scanToken ].
    (currentComment == nil or: [ currentComment isEmpty ])
        ifTrue: [ coll addLast: nil ]
        ifFalse: [ 
            coll addLast: currentComment removeFirst.
            [ currentComment isEmpty ]
                whileFalse: [ coll at: coll size put: coll last , ' ' , currentComment removeFirst ] ].
    ^ coll