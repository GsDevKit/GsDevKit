private
outdent
	currentIndent
		ifNotNil: [
			stream cr.
			currentIndent := currentIndent-1.
			self writeIndent.
			currentIndent := currentIndent-1.]