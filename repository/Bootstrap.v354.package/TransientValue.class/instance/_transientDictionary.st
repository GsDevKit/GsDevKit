private
_transientDictionary

	| dict |
	dict := SessionTemps current at: #TransientValue otherwise: nil.
	dict == nil
		ifTrue: [
			dict := IdentityKeyValueDictionary new.
			SessionTemps current at: #TransientValue put: dict ].
	^dict