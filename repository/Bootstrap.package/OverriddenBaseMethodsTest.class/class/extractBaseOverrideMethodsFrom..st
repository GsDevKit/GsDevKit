private
extractBaseOverrideMethodsFrom: aDictionary

	| results |
	results := OrderedCollection new.
	aDictionary keysAndValuesDo: [:behavior :set | | stream methodName |
		stream := WriteStream on: String new.
		methodName := behavior isMeta
			ifTrue: [ behavior theNonMetaClass name asString, 'Class' ]
			ifFalse: [ behavior name asString ].
		stream 
			nextPutAll: 'baseOverrides', methodName, ': known';
			cr;
			tab; nextPutAll: 'self'; cr;
			tab; tab; nextPutAll: 'addTo: known'; cr;
			tab; tab; nextPutAll: 'forBehavior: ', behavior name asString; cr;
			tab; tab; nextPutAll: 'overrides: {'; cr.
		set do: [:array | | method |
			method := array at: 1.
			stream tab; tab; tab; 
				nextPutAll: method selector printString, ' -> ', method sourceString printString, '.';
				cr ].
		stream 
			tab; 
			nextPut: $}.
		results add: stream contents].
	^results