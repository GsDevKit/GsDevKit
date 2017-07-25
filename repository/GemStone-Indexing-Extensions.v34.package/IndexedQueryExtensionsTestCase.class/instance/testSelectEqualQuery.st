test select
testSelectEqualQuery
	"select: {:ea | ea.key = 50} "
	
	| nsc result queryBlock |
	nsc := nscs first.
	queryBlock := [
		result := nsc select: #(key) comparing: #= with: queryValue key.
		self assert: result size = 1.
		self assert: (result includes: queryValue) ].
	
	queryBlock value.
	nsc createRcEqualityIndexOn: 'key' withLastElementClass: Number.
	queryBlock value.
