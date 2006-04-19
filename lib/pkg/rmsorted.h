define	RMS_OFFSET	4			# Offset to data
define	RMS_LEN		(RMS_OFFSET+2*$1+2)	# Structure length
define	RMS_BOX		Memi[$1]		# Running box size
define	RMS_DATA	Memi[$1+1]		# Sorted data (ptr)
define	RMS_IN		Memi[$1+2]		# Mapping to input (ptr)
define	RMS_OUT		Memi[$1+3]		# Mapping to output (ptr)

define	DATA		Memr[RMS_DATA($1)+$2]
define	IN		Mems[RMS_IN($1)+$2]
define	OUT		Mems[RMS_OUT($1)+$2]
