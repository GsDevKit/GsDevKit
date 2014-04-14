tests
testByteSubclassOfByteClass

  | theClass |
  theClass := String byteSubclass: self issue4ClassName
	classVars: #() classInstVars: #() poolDictionaries: #() inDictionary: ''
	category: self issue4CategoryName
	options: #().

  self assert: false equals: theClass _portableIsIndexable.
  (theClass respondsTo: #asClassDefinition) ifTrue: [ "monticello specific"
    self
      assert: #'bytes' equals: theClass asClassDefinition type;
      assert: #'bytes' equals: theClass typeOfClass].
  (theClass respondsTo: #definition) ifTrue: ["change-notification"
    self assert: self issue4Definition equals: theClass definition].