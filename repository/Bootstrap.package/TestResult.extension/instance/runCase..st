*bootstrap
runCase: aTestCase

	[
		[
			aTestCase runCase.
			passed add: aTestCase.
		] sunitOn: self class failure do: [:signal |
			failures add: aTestCase.
			^self.
		].
	] sunitOn: self class error do: [:signal |
		errors add: aTestCase.
		^self.
	].
