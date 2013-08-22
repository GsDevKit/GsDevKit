display
name
|name|

	name := '*'.
	query do: [:ea | name := name , ea, '*'].
	^name, ' (classes)'