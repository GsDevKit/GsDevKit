private
concentrateUpdates: updateArray

	| dict count concentrated |
	dict := IdentityKeyValueDictionary new.
	count := 0.
	updateArray do: [:assoc | | bag |
		bag := dict at: assoc value ifAbsent: [].
		bag == nil 
			ifTrue: [ 
				bag := IdentityBag new.
				dict at: assoc value put: bag ].
		bag add: assoc key.
		count := count + 1].
	dict size == count 
		ifTrue: [ 
			"no duplicate objects, so no value in concention pass"
			^updateArray ]. 
	concentrated := Array new.
	updateArray do: [:assoc | | bag |
		bag := dict at: assoc value.
		(bag occurrencesOf: assoc key) == 1
			ifTrue: [ concentrated add: assoc ].
		bag remove: assoc key ].
	^concentrated