updating
scanNodeCommands: ann 
	(cmdFactories select: [:ea | ea takesNodes]) 
		do: [:ea | ann addFactory: ea]