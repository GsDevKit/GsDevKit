test detect
testDetectGreaterThanQuery
	"detect: {:ea | ea.key > 50} "
	
	| nsc result queryBlock |
	nsc := nscs first.
	queryBlock := [
		result := nsc detect: #(key) comparing: #> with: queryValue key.
		self assert: result key > queryValue key ].
	
	queryBlock value.
	nsc createRcEqualityIndexOn: 'key' withLastElementClass: Number.
	queryBlock value.
