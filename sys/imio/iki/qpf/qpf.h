# QPF.H -- IKI/QPF internal definitions.

define	QPF_EXTN	"qp"		# image header filename extension
define	MAX_LENEXTN	3		# max length imagefile extension
define	SZ_KWNAME	8		# size of a FITS keyword name
define	SZ_BIGSTR	68		# max length string per FITS card
define	SZ_MAXFILTER	1024		# max size QPIO filter (for log only)

define	LEN_QPFDES	9
define	QPF_IM		Memi[$1]	# backpointer to image descriptor
define	QPF_QP		Memi[$1+1]	# QPOE datafile descriptor
define	QPF_IO		Memi[$1+2]	# QPIO descriptor
define	QPF_BLOCK	Memi[$1+3]	# block factor for sampling
define	QPF_VS		Memi[$1+4+$2-1]	# start vector of active rect
define	QPF_VE		Memi[$1+6+$2-1]	# end vector of active rect
define	QPF_IOSTAT	Memi[$1+8]	# i/o status (byte count)

# QPOE parameters to be omitted from the IMIO header user parameter list.
define	OMIT "|naxes|axlen|datamin|datamax|cretime|modtime|limtime|"
