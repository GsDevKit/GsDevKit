private
addTo: known forBehavior: behavior overrides: overrides

	| dict |
	dict := known at: behavior ifAbsent: [ 
		dict := Dictionary new.
		known at: behavior put: dict.
		dict ].
	overrides do: [:assoc | | set |
		set := dict 
			at: assoc key 
			ifAbsent: [
				set := Set new.
				dict at: assoc key put: set.
				set ].
		set add: assoc value ].