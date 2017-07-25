test select
testSelectIdentityEqualQuery
	"select: {:ea | ea.key == '50'} "
	
	| nsc result queryBlock |
	nsc := nscs first.
	queryBlock := [
		result := nsc select: #(value) comparing: #== with: queryValue value.
		self assert: result size = 1.
		self assert: (result includesIdentical: queryValue).

		result := nsc select: #(value) comparing: #== with: (queryValue value) copy.
		self assert: result isEmpty].
	
	queryBlock value.
	nsc createRcEqualityIndexOn: 'value' withLastElementClass: String.
	queryBlock value.
	nsc removeEqualityIndexOn: 'value'.
	nsc createIdentityIndexOn: 'value'.
	queryBlock value.
