test range detect
testRangeDetectGreaterThanOrEqualLessThanOrEqual
	"detect: {:ea | (ea.key >= 25) & (ea.key <= 50) } "
	
	| nsc result queryBlock failedDetectBlock |
	nsc := nscs first.
	queryBlock := [
		result := nsc low: 25 comparing: #<= detect: #(key) comparing: #<= high: 50. 
		self assert: (result key >= 25) & (result key <= 50) ].
	failedDetectBlock := [ | noneDetected |
		noneDetected := false.
		result := nsc low: 101 comparing: #<= detect: #(key) comparing: #<= high: 200 ifNone: [ noneDetected := true ]. 
		self assert: noneDetected ].
	
	queryBlock value.
	failedDetectBlock value.
	nsc createRcEqualityIndexOn: 'key' withLastElementClass: Number.
	queryBlock value.
	failedDetectBlock value.
