*bootstrap-notification
sessionStart

	self subscriptions do: [:objName | | obj |
		obj := System myUserProfile objectNamed: objName.
		obj ~~ nil ifTrue: [ obj sessionStart ]].
