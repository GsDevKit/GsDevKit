private
pickAncestorVersionInfo
	| ancestors index |
	ancestors := self workingCopy ancestry breadthFirstAncestors.
	^OBChoiceRequest 
		prompt: 'Package:' 
		labels: (ancestors collect: [:ea | ea name])
		values: ancestors.