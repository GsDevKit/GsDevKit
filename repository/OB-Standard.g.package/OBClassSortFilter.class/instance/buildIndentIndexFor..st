private
buildIndentIndexFor: sortedNodes
	indents := "Identity"Dictionary new.
	sortedNodes do: [:node | indents at: node put: node theNonMetaClass allSuperclasses size].