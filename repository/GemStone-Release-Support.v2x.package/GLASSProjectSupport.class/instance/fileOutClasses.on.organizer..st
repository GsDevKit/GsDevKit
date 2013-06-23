bootstrap fileout
fileOutClasses: symDict on: stream organizer: organizer

	| classes |
	(classes := organizer determineClassFileoutOrder: symDict) do: [:cl |
		self fileOutClass: cl on: stream ].
	classes do: [:cl |
		self fileOutMethodsFor: cl on: stream ]