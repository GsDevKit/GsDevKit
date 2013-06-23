test select
testSelectNotEqualQuery
	"select: {:ea | ea.key ~= 50} "
	
	| nsc result queryBlock |
	nsc := nscs first.
	queryBlock := [
		result := nsc select: #(key) comparing: #'~=' with: queryValue key.
		self assert: result size = 99.
		self assert: (result includes: queryValue) not ].
	
	queryBlock value.
	nsc createRcEqualityIndexOn: 'key' withLastElementClass: Number.
	queryBlock value.
