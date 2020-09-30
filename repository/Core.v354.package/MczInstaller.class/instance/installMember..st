as yet unclassified
installMember: member
	 | str |
	self useNewChangeSetDuring:
		[str _ member contentStream text.
		str fileIn]