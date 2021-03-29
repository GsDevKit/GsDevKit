private
generateBaseOverrideMethodsFrom: aDictionary
	"OverriddenBaseMethodsTest generateBaseOverrideMethodsFrom: self"

	(self extractBaseOverrideMethodsFrom: aDictionary) do: [:source |
		self compile: source classified: 'generated' ].