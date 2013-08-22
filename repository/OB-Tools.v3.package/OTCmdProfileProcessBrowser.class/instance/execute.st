execution
execute
	self flag: #porttogemstone.
	"| seconds |
	seconds := (OBTextRequest prompt: 'Profile for how many seconds?' template: '4')
		ifNil: [ ^ self ].
	seconds := seconds asNumber asInteger.
	seconds isZero
		ifTrue: [ ^ self ].
	[ TimeProfileBrowser spyOnProcess: target process forMilliseconds: seconds * 1000 ]
		forkAt: target process priority + 1"