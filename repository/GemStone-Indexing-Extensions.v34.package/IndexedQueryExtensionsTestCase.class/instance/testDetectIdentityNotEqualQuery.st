test detect
testDetectIdentityNotEqualQuery
	"detect: {:ea | ea.value ~~ '50'} "
	
	| nsc result queryBlock |
	nsc := nscs first.
	queryBlock := [ | noneDetected |
		result := nsc detect: #(value) comparing: #~~ with: queryValue value.
		self assert: result ~~ queryValue.
		self assert: result key ~= queryValue key.

		noneDetected := false.
		result := nsc detect: #(value) comparing: #~~ with: queryValue value copy ifNone: [ noneDetected := true ].
		self deny: noneDetected ].
	
	queryBlock value.
	nsc createRcEqualityIndexOn: 'value' withLastElementClass: String.
	queryBlock value.
	nsc removeEqualityIndexOn: 'value'.
	nsc createIdentityIndexOn: 'value'.
	queryBlock value.
