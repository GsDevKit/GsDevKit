*ob-standard-accessing
removeClassHistory
	"remove old classes from the class history"
	| myHistory |
	myHistory := self classHistory.
	myHistory size > 1 ifTrue: [myHistory deleteFrom: 1 to: myHistory size - 1]