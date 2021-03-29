private
continuationLabel

	self isResumed ifTrue: [ ^super continuationLabel ].
	^'-- resumable continuation --'