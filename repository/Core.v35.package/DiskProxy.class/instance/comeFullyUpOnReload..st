loading
comeFullyUpOnReload: smartRefStream
  "Internalize myself into a fully alive object after raw loading from a DataStream. (See my class comment.)  DataStream will substitute the object from this eval for the DiskProxy."
  | globalObj symbol |

  symbol := globalObjectName.
  "See if class is mapped to another name"
  globalObj := System myUserProfile symbolList objectNamed: symbol.
  globalObj == nil ifTrue: [
    ^ self error: 'Global "', symbol, '" not found'].
  preSelector ifNotNil: [
    Symbol hasInterned: preSelector ifTrue: [:selector |
      [globalObj := globalObj perform: selector] on: Error do: [:ex |
        ex messageText = 'key not found' ifTrue: [^ nil].
        ^ ex signal]]
  ].
  constructorSelector ifNil: [^ globalObj].
  Symbol hasInterned: constructorSelector ifTrue: [:selector |
    [^ globalObj perform: selector withArguments: constructorArgs] on: Error do: [:ex |
      ex messageText = 'key not found' ifTrue: [^ nil].
      ^ ex signal]
  ].
        "args not checked against Renamed"
  ^ nil   "was not in proper form"