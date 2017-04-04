test detect
testDetectEqualQuery
	"detect: {:ea | ea.key = 50} "
	
	| nsc result queryBlock failedDetectBlock |
	nsc := nscs first.
	queryBlock := [
		result := nsc detect: #(key) comparing: #= with: queryValue key.
		self assert: result == queryValue ].
	failedDetectBlock := [ | noneDetected |
		noneDetected := false.
		result := nsc detect: #(key) comparing: #= with: 101 ifNone: [ noneDetected := true ].
		self assert: noneDetected ].
	
	queryBlock value.
	failedDetectBlock value.
	nsc createRcEqualityIndexOn: 'key' withLastElementClass: Number.
	queryBlock value.
	failedDetectBlock value.
