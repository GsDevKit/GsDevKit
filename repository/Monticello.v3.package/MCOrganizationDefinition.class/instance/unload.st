actions
unload
	| empty |
	empty := categories select: [:ea |
		(SystemOrganization listAtCategoryNamed: ea) isEmpty].
	empty do: [:cat |
		SystemOrganization removeSystemCategory: cat].
