execution
execute
	| definition organizer |
	organizer := target organization.
	definition := OBOrganizationDefinition on: organizer.
	requestor announce: definition.
	self doAutoCommit