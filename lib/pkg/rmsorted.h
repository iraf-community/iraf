define	RMS_OFFSET	5			# Offset to data
define	RMS_LEN		(RMS_OFFSET+3*$1+3)	# Structure length
define	RMS_BOX		Memi[$1]		# Running box size
define	RMS_TYPE	Memi[$1+1]		# Output type
define	RMS_DATA	Memi[$1+2]		# Sorted data (ptr)
define	RMS_IN		Memi[$1+3]		# Mapping to input (ptr)
define	RMS_OUT		Memi[$1+4]		# Mapping to output (ptr)

define	DATA		Memr[RMS_DATA($1)+$2]
define	IN		Mems[P2S(RMS_IN($1)+$2)]
define	OUT		Mems[P2S(RMS_OUT($1)+$2)]

define	RMS_TYMED	1			# Median
define	RMS_TYMAX	2			# Maximum
define	RMS_TYMIN	3			# Minimum
