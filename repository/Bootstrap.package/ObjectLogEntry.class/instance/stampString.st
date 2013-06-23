displaying
stampString

	^(self stamp date asStringUsingFormat: #(2 1 3 $/ 1 1 $: false)), ' ', 
		(self stamp time asStringUsingFormat: #($: true false)), ':',
		(self integerAsString: (self stamp time asMilliseconds asInteger \\ 1000))