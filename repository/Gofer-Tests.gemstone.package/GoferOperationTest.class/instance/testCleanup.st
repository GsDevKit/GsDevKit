testing
testCleanup
	| class |
	gofer package: 'GoferFoo'; load.
	class := Smalltalk globals classNamed: #GoferFoo.
	class organization addCategory: #empty.
	class class organization addCategory: #empty.
	gofer cleanup.
	self deny: (class organization categories includes: #'empty').
	self deny: (class class organization categories includes: #'empty')