installing
postload
	
	self isInitializer 
		ifTrue: [ 
			(MCPerformPostloadNotification new postloadClass: self actualClass) signal 
				ifTrue:  [  self actualClass theNonMetaClass initialize ]]