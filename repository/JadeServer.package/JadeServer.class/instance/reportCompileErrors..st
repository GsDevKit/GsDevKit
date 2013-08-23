GsCompiler Support
reportCompileErrors: anArrayOfArrays

	| stream |
	stream := WriteStream on: String new.
	anArrayOfArrays do: [:eachArray | 
		eachArray do: [:each | 
			stream nextPutAll: each asString; tab.
		].
		stream cr.
	].
	^stream contents.