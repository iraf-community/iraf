# PIXEL LIST descriptor structure

define	LEN_PLSTRUCT	10

define	PRL_NCOLS		Memi[$1]	# number of columns
define	PRL_NLINES		Memi[$1+1]	# number of lines
define	PRL_LINES		Memi[$1+2]	# pointer to the line offsets
define	PRL_LIST		Memi[$1+3]	# pointer to list of ranges
define	PRL_SZLIST		Memi[$1+4]	# size of list in INTS
define	PRL_LP			Memi[$1+5]	# offset to next space in list

