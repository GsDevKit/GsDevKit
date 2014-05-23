testing
hasDataDescriptor
	^ (bitFlag bitAnd: 8)	~= 0 "GPBF_HAS_DATA_DESCRIPTOR_MASK"