accessing
record: aMethodVersionRecord

	record := aMethodVersionRecord.
	classRef := OBClassReference  to: record behavior.
	category := record category.
	stamp := record changeStamp.
	selector := record selector.