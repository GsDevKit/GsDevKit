tests
testEquals
	| a b |
	a := self mockTrait: 'TraitA' composition: 'SuperA'.
	b := self mockTrait: 'TraitA' composition: 'SuperA'.
	self assert: a = b