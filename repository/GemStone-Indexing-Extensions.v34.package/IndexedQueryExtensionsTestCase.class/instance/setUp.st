running
setUp

	| nsc |
	super setUp.
	nscs := Array new.
	nsc := RcIdentityBag new.
	nscs add: nsc. "allow indexes to be removed during teardown"
	1 to: 100 do: [:i | | assoc |
		assoc := i -> i printString.
		i == 50 ifTrue: [ queryValue := assoc ].
		nsc add: assoc ].
