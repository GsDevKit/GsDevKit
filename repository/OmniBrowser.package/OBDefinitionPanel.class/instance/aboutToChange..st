updating
aboutToChange: ann
	| ok |
	self canDiscardEdits ifTrue: [^ true].
	ok := OBConfirmationRequest
			prompt: 'Changes have not been saved.
Is it OK to discard those changes?'
			confirm: 'Discard changes'.
	(ok isNil not and: [ok])
		ifTrue: [self changed: #clearUserEdits]
		ifFalse: [ann veto]