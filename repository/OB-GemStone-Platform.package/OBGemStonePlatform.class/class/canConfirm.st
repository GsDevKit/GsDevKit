testing
canConfirm

	^(self clientForwarderMap at:#confirmation ifAbsent: [ nil ]) ~~ nil