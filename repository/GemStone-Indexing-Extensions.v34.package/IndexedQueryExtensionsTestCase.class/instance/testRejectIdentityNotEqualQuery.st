test reject
testRejectIdentityNotEqualQuery
	"reject: {:ea | ea.key ~~ '50'} "
	
	| nsc result queryBlock |
	nsc := nscs first.
	queryBlock := [
		result := nsc reject: #(value) comparing: #~~ with: queryValue value.
		self assert: result size = 1.
		self assert: (result includesIdentical: queryValue).

		result := nsc reject: #(value) comparing: #~~ with: (queryValue value) copy.
		self assert: result isEmpty ].
	
	queryBlock value.
	nsc createRcEqualityIndexOn: 'value' withLastElementClass: String.
	queryBlock value.
	nsc removeEqualityIndexOn: 'value'.
	nsc createIdentityIndexOn: 'value'.
	queryBlock value.
