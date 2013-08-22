user interface
select: aNode with: anAnnouncer
	(anAnnouncer announce: OBAboutToChange)
		isVetoed ifFalse: [anAnnouncer announce: (OBSelectingNode node: aNode)]