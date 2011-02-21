# Header file for IR Mosaicing Routines

# Define the structure

define	LEN_IRSTRUCT	35

define 	IR_NCOLS	Memi[$1]	# x length of single subraster
define	IR_NROWS	Memi[$1+1]	# y length of a single subrasters
define	IR_NXOVERLAP	Memi[$1+2]	# x overlap between subrasters
define	IR_NYOVERLAP	Memi[$1+3]	# y overlap between subrasters
define	IR_NXSUB	Memi[$1+4]	# number of subrasters in x dimension
define	IR_NYSUB	Memi[$1+5]	# number of subrasters in y dimension
define	IR_NXRSUB	Memi[$1+6]	# x index of reference subraster
define	IR_NYRSUB	Memi[$1+7]	# y index of reference subraster
define	IR_XREF		Memi[$1+8]	# x offset of reference subraster
define	IR_YREF		Memi[$1+9]	# y offset of reference subraster
define	IR_CORNER	Memi[$1+10]	# starting corner for insertion
define	IR_ORDER	Memi[$1+11]	# row or column insertion
define	IR_RASTER	Memi[$1+12]	# raster order
define	IR_OVAL		Memr[P2R($1+13)] # undefined value

define	IR_IC1		Memi[$1+14]	# input image lower column limit
define	IR_IC2		Memi[$1+15]	# input image upper column limit
define	IR_IL1		Memi[$1+16]	# input image lower line limit
define	IR_IL2		Memi[$1+17]	# input image upper line limit
define	IR_OC1		Memi[$1+18]	# output image lower column limit
define	IR_OC2		Memi[$1+19]	# output image upper column limit
define	IR_OL1		Memi[$1+20]	# output image lower line limit
define	IR_OL2		Memi[$1+21]	# output image upper line limit
define	IR_DELTAX	Memi[$1+22]	# x shifts
define	IR_DELTAY	Memi[$1+23]	# y shifts
define	IR_DELTAI	Memi[$1+24]	# intensity shifts

define	IR_XRSHIFTS	Memi[$1+25]	# x row links
define	IR_YRSHIFTS	Memi[$1+26]	# y row links
define	IR_NRSHIFTS	Memi[$1+27]	# number of row links
define	IR_XCSHIFTS	Memi[$1+28]	# x column links
define	IR_YCSHIFTS	Memi[$1+29]	# y column links
define	IR_NCSHIFTS	Memi[$1+30]	# number of column links

# Define some useful constants

define	IR_LL		1
define	IR_LR		2
define	IR_UL		3
define	IR_UR		4

define	IR_ROW		1
define	IR_COLUMN	2

define	IR_COORDS	1
define	IR_SHIFTS	2
define	IR_FILE		3

define	MAX_NRANGES	100
