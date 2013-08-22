private
buildIndentIndex
	indents := "Identity"Dictionary new.
	order do: [:assoc | indents at: assoc value put: assoc key size - 1].