# PIXEL LIST descriptor structure

define	LEN_PLSTRUCT	10

define	PL_NCOLS		Memi[$1]	# number of columns
define	PL_NLINES		Memi[$1+1]	# number of lines
define	PL_LINES		Memi[$1+2]	# pointer to the line offsets
define	PL_LIST			Memi[$1+3]	# pointer to list of ranges
define	PL_SZLIST		Memi[$1+4]	# size of list in INTS
define	PL_LP			Memi[$1+5]	# offset to next space in list

