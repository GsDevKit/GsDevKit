accessing
currentProcess

	^SessionTemps current at: #OT_PROCESS_BROWSER_CURRENT_PROCESS ifAbsent: [].