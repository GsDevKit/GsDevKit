accessing
switchFilter: aFilter
	"We should only be sending #changed: if the filter is actually different
	from the one the switch already has, but that causes some visually odd
	behavior because of the way OBRadioButtonBar resizes its self when updating,
	so for now we always just recreate the button bar."
	
	switch filter: aFilter.
	self changed: #switch