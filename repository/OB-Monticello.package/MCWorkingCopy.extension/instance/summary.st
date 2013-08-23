*ob-monticello
summary
	^ String streamContents:
		[:s |
		self modified ifTrue: [s nextPutAll: 'MODIFIED'; cr].
		s nextPutAll: self ancestry summary.
		s cr; nextPutAll: '==========================='.
		self ancestry ancestors do:
			[:ea |
			s cr; cr.
			s nextPutAll: ea summary.
			s cr; nextPutAll: '---------------------------' ].
		self requiredPackages isEmpty 
			ifFalse: [ 
				s cr; nextPutAll: '---------------------------'.
				s cr; cr; nextPutAll: 'Required: ' ].
		(self requiredPackages sortBy: [:a :b | a name <= b name]) do:
			[:ea |
			s nextPutAll: ea name, ', ']]