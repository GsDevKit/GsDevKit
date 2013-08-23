updating
scanTextCommands: ann 
	(cmdFactories select: [:ea | ea takesText]) do: [:ea | ann addFactory: ea]