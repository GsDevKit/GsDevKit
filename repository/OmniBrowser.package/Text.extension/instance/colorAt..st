*omnibrowser-morphic
colorAt: characterIndex
    | textColor | 
    textColor _ (self attributesAt: characterIndex) detect: [:att | att isMemberOf: TextColor] ifNone: [TextColor black].
    ^textColor color