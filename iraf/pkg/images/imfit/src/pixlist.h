# PIXEL LIST descriptor structure

define	LEN_PLSTRUCT	10

define	PRL_NCOLS		Memz[P2Z($1)]	# number of columns
define	PRL_NLINES		Memz[P2Z($1+1)]	# number of lines
define	PRL_LINES		Memp[$1+2]	# pointer to the line offsets
define	PRL_LIST		Memp[$1+3]	# pointer to list of ranges
define	PRL_SZLIST		Memz[P2Z($1+4)]	# size of list in INTS
define	PRL_LP			Meml[P2L($1+5)]	# offset to next space in list

