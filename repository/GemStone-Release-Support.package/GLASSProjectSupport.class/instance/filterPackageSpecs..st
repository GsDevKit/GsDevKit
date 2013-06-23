bootstrap packages
filterPackageSpecs: specs
	"remove duplicate files"
	
	| dict uniqueSpecs |
	dict := Dictionary new.
	uniqueSpecs := OrderedCollection new.
	specs do: [:spec | | list |
		list := dict 
			at: spec name 
				ifAbsent: [
					list := Dictionary new.
					dict at: spec name put: list.
					list ].
		list at: spec file put:  spec ].
	dict values do: [:list | uniqueSpecs addAll: list values ].
	^uniqueSpecs