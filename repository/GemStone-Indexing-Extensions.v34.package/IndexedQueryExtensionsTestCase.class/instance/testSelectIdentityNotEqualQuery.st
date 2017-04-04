test select
testSelectIdentityNotEqualQuery
	"select: {:ea | ea.value ~~ '50'} "
	
	| nsc result queryBlock |
	nsc := nscs first.
	queryBlock := [
		result := nsc select: #(value) comparing: #~~ with: queryValue value.
		self assert: result size = 99.
		self deny: (result includesIdentical: queryValue).

		result := nsc select: #(value) comparing: #~~ with: (queryValue value) copy.
		self assert: result size = 100].
	
	queryBlock value.
	nsc createRcEqualityIndexOn: 'value' withLastElementClass: String.
	queryBlock value.
	nsc removeEqualityIndexOn: 'value'.
	nsc createIdentityIndexOn: 'value'.
	queryBlock value.
