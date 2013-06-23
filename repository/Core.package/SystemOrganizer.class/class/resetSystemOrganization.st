class initialization
resetSystemOrganization
	"self resetSystemOrganization"

	| ar |
	ar := System myUserProfile symbolList dictionaryAndSymbolOf: self.
	(ar at: 1) at: #SystemOrganization put: self new.