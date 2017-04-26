# TEMPLATE.H -- Structure used to expand image names over groups

define	LEN_TPSTRUCT	5

define	TP_ROOTPTR	Memi[$1]	# Pointer to image root name
define	TP_SECTPTR	Memi[$1+1]	# Pointer to image section
define	TP_START	Memi[$1+2]	# First group
define	TP_COUNT	Memi[$1+3]	# Total number of groups
define	TP_INDEX	Memi[$1+4]	# Current group

define	TP_ROOT		Memc[TP_ROOTPTR($1)]
define	TP_SECT		Memc[TP_SECTPTR($1)]

define	TP_EXT_LIST	"|stf|fxf|oif|plf|qpf|"

define	TP_UNKNOWN	0
define	TP_GEIS		1
define	TP_FITS		2
define	TP_IRAF		3
define	TP_PIXLIST	4
define	TP_QPOE		5
