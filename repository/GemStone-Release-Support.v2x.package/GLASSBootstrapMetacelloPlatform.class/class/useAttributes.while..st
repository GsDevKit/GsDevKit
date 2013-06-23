actions
useAttributes: attributes while: aBlock 

	| old |
	old := Current.
	[
		Current := self new.
		Current stoneVersionAttributes: attributes.
		aBlock value ]
			ensure: [ Current := old ]