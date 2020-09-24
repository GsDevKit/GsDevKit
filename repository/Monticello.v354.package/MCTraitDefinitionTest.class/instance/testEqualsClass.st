tests
testEqualsClass
	| a b |
	a := self mockClass: 'ClassA' super: 'SuperA' traitComposition: 'TraitA' classTraitComposition: 'TraitB'.
	b := self mockClass: 'ClassA' super: 'SuperA' traitComposition: 'TraitA' classTraitComposition: 'TraitB'.
	self assert: a = b