private
cacheDictionary

	| d |
	d := SessionTemps current at: self cacheName otherwise: nil.
	d == nil 
		ifTrue: [
			d := SessionTemps new.
			SessionTemps current at: self cacheName put: d].
	^d