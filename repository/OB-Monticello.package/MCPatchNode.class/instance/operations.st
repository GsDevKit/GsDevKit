as yet unclassified
operations
	^ patch operations collect: [:ea | self operationNodeClass new operation: ea]