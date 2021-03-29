tests
testEqualsSensitivity
	| message a b defA args defB |
	message := self creationMessage.
	a := #(TraitA CompTraitA CategoryA 'A comment' 'A').
	b := #(TraitB CompTraitB CategoryB 'B comment' 'B').
	
	defA := message valueWithArguments: a.
	1 to: 4 do: [:index |
				args := a copy.
				args at: index put: (b at: index).
				defB := message valueWithArguments: args.
				self deny: defA = defB.]