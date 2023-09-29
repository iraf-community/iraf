# PRSTRUCT.H -- Parser symbol table structure.


# Pointer access
define	MEMP		Memi


# ----------------------------------------------------------------------
# Parser symbol structure. The symbol table is handled by the SYMTAB
# procedures. The following is the definition of the structure associated
# with each identifier in the table. The following diagram shows graphically
# this structure:
#
#    offset
#			+---------------------------------------+
#	0		| symbol type 				|
#			+---------------------------------------+
#	1		| symbol  number			|
#			+---------------------------------------+
#	2		| substructure pointer			| ->
#			+---------------------------------------+
#	3		next free location
#

# Symbol structure length
define	LEN_PSYM	3

# Symbol structure definition
define	PSYM_TYPE	Memi[$1+0]	# type
define	PSYM_NUM	Memi[$1+1]	# sequential number
define	PSYM_SUB	MEMP[$1+2]	# symbol substructure
# next free location	    ($1 + 3) == LEN_PSYM !!!


# ----------------------------------------------------------------------
# Input column substructure. This substructure is supported for catalog
# and observation input columns (PTY_CATCOL and PTY_OBSCOL). The following
# diagram shows graphically this structure:
#
#    offset
#			+---------------------------------------+
#	0		| input column number 			|
#			+---------------------------------------+
#	1		| error column number 			|
#			+---------------------------------------+
#	2		| weight column number 			|
#			+---------------------------------------+
#	3		| spare column flag 			|
#			+---------------------------------------+
#	4		next free location
#

# Input column substructure length.
define	LEN_PINP	4

# Input column substructure definition
define	PINP_COL	Memi[$1+0]		# input column
define	PINP_ERRCOL	Memi[$1+1]		# error column
define	PINP_WTSCOL	Memi[$1+2]		# weight column
define	PINP_SPARE	Memi[$1+3]		# spare flag (YES/NO)
# next free location	    ($1 + 4) == LEN_PINP !!!


# ----------------------------------------------------------------------
# Fitting parameter substructure. This substructure is supported for
# fitting and constant parameters (PTY_FITPAR and PTY_CONST). The
# following diagram shows graphically this structure:
#
#    offset
#			+---------------------------------------+
#	0		| parameter value 			|
#			+---------------------------------------+
#	1		| parameter delta			|
#			+---------------------------------------+
#	2		next free location
#

# Fitting parameter substructure length.
define	LEN_PFIT	2

# Fitting parameter substructure definition
define	PFIT_VALUE	Memr[P2R($1+0)]		# parameter value
define	PFIT_DELTA	Memr[P2R($1+1)]		# parameter delta
# next free location	    ($1 + 2) == LEN_PFIT !!!


# ----------------------------------------------------------------------
# Set equation substructure. This substructure is supported for set
# equation symbols (PTY_SETEQ). The length of this substructure is
# fixed.
#
#    offset
#			+---------------------------------------+
#	0 		| set equation string offset		|
#			+---------------------------------------+
#	1 		| error equation string offset		|
#			+---------------------------------------+
#	2 		| min error equation string offset	|
#			+---------------------------------------+
#	3 		| max error equation string offset	|
#			+---------------------------------------+
#	4 		| weight equation string offset		|
#			+---------------------------------------+
#	5 		| min weight equation string offset	|
#			+---------------------------------------+
#	6 		| max weight equation string offset	|
#			+---------------------------------------+
#	7 		| weight equation string offset		|
#			+---------------------------------------+
#	8 		| error equation code pointer		| ->
#			+---------------------------------------+
#	9 		| min error equation code pointer	| ->
#			+---------------------------------------+
#	10 		| max error equation code pointer	| ->
#			+---------------------------------------+
#	11 		| weight equation code pointer		| ->
#			+---------------------------------------+
#	12 		| min weight equation code pointer	| ->
#			+---------------------------------------+
#	13 		| max weight equation code pointer	| ->
#			+---------------------------------------+
#	14		next free location
#

# Set equation substructure length.
define	LEN_PSEQ	14

# Set equation substructure definition
define	PSEQ_EQ		Memi[$1 + 0]	# equation string offset
define	PSEQ_ERROR	Memi[$1 + 1]	# error equation string offset
define	PSEQ_ERRMIN	Memi[$1 + 2]	# min error equation string offset
define	PSEQ_ERRMAX	Memi[$1 + 3]	# max error equation string offset
define	PSEQ_WEIGHT	Memi[$1 + 4]	# weight equation string offset
define	PSEQ_WTSMIN	Memi[$1 + 5]	# min weight equation string offset
define	PSEQ_WTSMAX	Memi[$1 + 6]	# max weight equation string offset
define	PSEQ_RPNEQ	MEMP[$1 + 7]	# equation code
define	PSEQ_RPNERROR	MEMP[$1 + 8] 	# error equation code
define	PSEQ_RPNERRMIN	MEMP[$1 + 9]	# min error equation code
define	PSEQ_RPNERRMAX	MEMP[$1 + 10]	# max error equation code
define	PSEQ_RPNWEIGHT	MEMP[$1 + 11]	# weigt equation code
define	PSEQ_RPNWTSMIN	MEMP[$1 + 12]	# min weight equation code
define	PSEQ_RPNWTSMAX	MEMP[$1 + 13]	# max weight equation code
# next free location	    ($1 + 14) == LEN_PSEQ !!!


# ----------------------------------------------------------------------
# Transformation equation substructure. This substructure is supported
# for transformation equation symbols (PTY_TRNEQ). The length of this
# substructure is variable, and depends on the total number of variables
# (nvar) and parameters (npar) in the equation. The following diagram
# shows graphically this structure:
#
#    offset
#				+---------------------------------------+
#    0				| number of ref. catalog var.   = nrcat	|
#				+---------------------------------------+
#    1 				| number of ref. obs. var.      = nrobs	|
#				+---------------------------------------+
#    2				| total number of ref. var.     = nrvar	|
#				+---------------------------------------+
#    3				| number of fit catalog var.    = nfcat	|
#				+---------------------------------------+
#    4 				| number of fit obs. var.       = nfobs	|
#				+---------------------------------------+
#    5				| total number of fit var.      = nfvar	|
#				+---------------------------------------+
#    6				| total number of variables     = nvar	|
#				+---------------------------------------+
#    7				| ref. variable symbol offsets (nrvar) 	|
#				| ...					|
#				| ...					|
#				+---------------------------------------+
#    1 * nrvar + 7		| ref. variable counters (nrvar)  	|
#				| ...					|
#				| ...					|
#				+---------------------------------------+
#    2 * nrvar + 7		| fit variable symbol offsets (nfvar)  	|
#				| ...					|
#				| ...					|
#				+---------------------------------------+
#    2 * nrvar + 1 * nfvar + 7	| fit variable counters (nfvar)  	|
#				| ...					|
#				| ...					|
#				+---------------------------------------+
#    2 * nvar + 7		| number of parameters           = npar	|
#				+---------------------------------------+
#    2 * nvar + 8		| number of fitting parameters		|
#				+---------------------------------------+
#    2 * nvar + 9		| parameter symbol offsets (npar)	| 
#				| ...					|
#				| ...					|
#				+---------------------------------------+
#    2 * nvar + 1 * npar + 9	| parameter values (npar)		|
#				| ...					|
#				| ...					|
#				+---------------------------------------+
#    2 * nvar + 2 * npar + 9	| parameter list (npar)			|
#				| ...					|
#				| ...					|
#				+---------------------------------------+
#    2 * nvar + 3 * npar + 9	| fit equation string offset		|
#				+---------------------------------------+
#    2 * nvar + 3 * npar + 10	| reference equation string offset	|
#				+---------------------------------------+
#    2 * nvar + 3 * npar + 11	| error equation string offset		|
#				+---------------------------------------+
#    2 * nvar + 3 * npar + 12	| min error equation string offset	|
#				+---------------------------------------+
#    2 * nvar + 3 * npar + 13	| max error equation string offset	|
#				+---------------------------------------+
#    2 * nvar + 3 * npar + 14	| weight equation string offset		|
#				+---------------------------------------+
#    2 * nvar + 3 * npar + 15	| min weight equation string offset	|
#				+---------------------------------------+
#    2 * nvar + 3 * npar + 16	| max weight equation string offset	|
#				+---------------------------------------+
#    2 * nvar + 3 * npar + 17	| X plot equation string offset		|
#				+---------------------------------------+
#    2 * nvar + 3 * npar + 18	| Y plot equation string offset		|
#				+---------------------------------------+
#    2 * nvar + 3 * npar + 19	| derivative equation offsets (npar)	|
#				| ...					|
#				| ...					|
#				+---------------------------------------+
#    2 * nvar + 4 * npar + 19	| fit equation code pointer		| ->
#				+---------------------------------------+
#    2 * nvar + 4 * npar + 20	| reference equation code pointer	| ->
#				+---------------------------------------+
#    2 * nvar + 4 * npar + 21	| error equation code pointer		| ->
#				+---------------------------------------+
#    2 * nvar + 4 * npar + 22	| min error equation code pointer	| ->
#				+---------------------------------------+
#    2 * nvar + 4 * npar + 23	| max error equation code pointer	| ->
#				+---------------------------------------+
#    2 * nvar + 4 * npar + 24	| weight equation code pointer		| ->
#				+---------------------------------------+
#    2 * nvar + 4 * npar + 25	| min weight equation code pointer	| ->
#				+---------------------------------------+
#    2 * nvar + 4 * npar + 26	| max weight equation code pointer	| ->
#				+---------------------------------------+
#    2 * nvar + 4 * npar + 27	| X plot equation code pointer		| ->
#				+---------------------------------------+
#    2 * nvar + 4 * npar + 28	| Y plot equation code pointer		| ->
#				+---------------------------------------+
#    2 * nvar + 4 * npar + 29	| deriv. equation code pointers (npar)	| ->
#				| ...					| ->
#				| ...					| ->
#				+---------------------------------------+
#    2 * nvar + 5 * npar + 29	next free location
#

# Transformation equation substructure length. The macro parameters are
# the total number of variables (npar), and the number of parameters (npar)
# for the equation.
define	LEN_PTEQ	(2 * $1 + 5 * $2 + 29)

# Transformation equation substructure definition
# - number of catalog and observation variables, and total number of variables
#   for reference equation.
define	PTEQ_NRCAT	Memi[$1 + 0]
define	PTEQ_NROBS	Memi[$1 + 1]
define	PTEQ_NRVAR	Memi[$1 + 2]
# - number of catalog and observation variables, and total number of variables
#   for fit equation.
define	PTEQ_NFCAT	Memi[$1 + 3]
define	PTEQ_NFOBS	Memi[$1 + 4]
define	PTEQ_NFVAR	Memi[$1 + 5]
# - total number of variables in the reference and fit equations.
define	PTEQ_NVAR	Memi[$1 + 6]
# - start of reference variable symbol offsets, and variable counters
define	PTEQ_SREFVAR	    ($1 + 7)
define	PTEQ_SREFCNT  	    ($1 + 1 * PTEQ_NRVAR($1) + 7)
# - start of fit variable symbol offsets, and variable counters
define	PTEQ_SFITVAR	    ($1 + 2 * PTEQ_NRVAR($1) + 7)
define	PTEQ_SFITCNT  	    ($1 + 2 * PTEQ_NRVAR($1) + 1 * PTEQ_NFVAR($1) + 7)
# - total number of parameters and number of parameters to be fitted.
define	PTEQ_NPAR	Memi[$1 + 2 * PTEQ_NVAR($1) + 7]
define	PTEQ_NFPAR	Memi[$1 + 2 * PTEQ_NVAR($1) + 8]
# - Start of parameter offsets, parameter values, parameter list
define	PTEQ_SPAR	    ($1 + 2 * PTEQ_NVAR($1) + 9)
define	PTEQ_SPARVAL	    ($1 + 2 * PTEQ_NVAR($1) + 1 * PTEQ_NPAR($1) + 9)
define	PTEQ_SPLIST	    ($1 + 2 * PTEQ_NVAR($1) + 2 * PTEQ_NPAR($1) + 9)
# - fitting and reference equation string offsets
define	PTEQ_FIT	Memi[$1 + 2 * PTEQ_NVAR($1) + 3 * PTEQ_NPAR($1) + 9] 
define	PTEQ_REF	Memi[$1 + 2 * PTEQ_NVAR($1) + 3 * PTEQ_NPAR($1) + 10]
# - error, minimum error, and maximum error equation string offsets
define	PTEQ_ERROR	Memi[$1 + 2 * PTEQ_NVAR($1) + 3 * PTEQ_NPAR($1) + 11]
define	PTEQ_ERRMIN	Memi[$1 + 2 * PTEQ_NVAR($1) + 3 * PTEQ_NPAR($1) + 12]
define	PTEQ_ERRMAX	Memi[$1 + 2 * PTEQ_NVAR($1) + 3 * PTEQ_NPAR($1) + 13]
# - weight, minimum weight, and maximum weight equation string offsets
define	PTEQ_WEIGHT	Memi[$1 + 2 * PTEQ_NVAR($1) + 3 * PTEQ_NPAR($1) + 14]
define	PTEQ_WTSMIN	Memi[$1 + 2 * PTEQ_NVAR($1) + 3 * PTEQ_NPAR($1) + 15]
define	PTEQ_WTSMAX	Memi[$1 + 2 * PTEQ_NVAR($1) + 3 * PTEQ_NPAR($1) + 16]
# - plot equation string offsets
define	PTEQ_XPLOT	Memi[$1 + 2 * PTEQ_NVAR($1) + 3 * PTEQ_NPAR($1) + 17]
define	PTEQ_YPLOT	Memi[$1 + 2 * PTEQ_NVAR($1) + 3 * PTEQ_NPAR($1) + 18]
# - derivative equation string offsets
define	PTEQ_SDER	    ($1 + 2 * PTEQ_NVAR($1) + 3 * PTEQ_NPAR($1) + 19)
# - fitting and reference equation codes
define	PTEQ_RPNFIT	MEMP[$1 + 2 * PTEQ_NVAR($1) + 4 * PTEQ_NPAR($1) + 19]
define	PTEQ_RPNREF	MEMP[$1 + 2 * PTEQ_NVAR($1) + 4 * PTEQ_NPAR($1) + 20]
# - error equation, minimum error, and maximum error codes
define	PTEQ_RPNERROR	MEMP[$1 + 2 * PTEQ_NVAR($1) + 4 * PTEQ_NPAR($1) + 21] 
define	PTEQ_RPNERRMIN	MEMP[$1 + 2 * PTEQ_NVAR($1) + 4 * PTEQ_NPAR($1) + 22]
define	PTEQ_RPNERRMAX	MEMP[$1 + 2 * PTEQ_NVAR($1) + 4 * PTEQ_NPAR($1) + 23]
# - weigth, minimum weight, maximum weight equation codes
define	PTEQ_RPNWEIGHT	MEMP[$1 + 2 * PTEQ_NVAR($1) + 4 * PTEQ_NPAR($1) + 24]
define	PTEQ_RPNWTSMIN	MEMP[$1 + 2 * PTEQ_NVAR($1) + 4 * PTEQ_NPAR($1) + 25]
define	PTEQ_RPNWTSMAX	MEMP[$1 + 2 * PTEQ_NVAR($1) + 4 * PTEQ_NPAR($1) + 26]
# - plot equation code3
define	PTEQ_RPNXPLOT	MEMP[$1 + 2 * PTEQ_NVAR($1) + 4 * PTEQ_NPAR($1) + 27]
define	PTEQ_RPNYPLOT	MEMP[$1 + 2 * PTEQ_NVAR($1) + 4 * PTEQ_NPAR($1) + 28]
# - start of derivarive equation codes
define	PTEQ_SRPNDER	    ($1 + 2 * PTEQ_NVAR($1) + 4 * PTEQ_NPAR($1) + 29)
# - next free location == LEN_PTEQ
# 			    ($1 + 2 * PTEQ_NVAR($1) + 5 * PTEQ_NPAR($1) + 29)

# Vector access
define	PTEQ_AREFVAR	Memi[PTEQ_SREFVAR($1)]	    # ref. variable sym. off.
define	PTEQ_AREFCNT	Memi[PTEQ_SREFCNT($1)]	    # ref. variable counters
define	PTEQ_AFITVAR	Memi[PTEQ_SFITVAR($1)]	    # fit variable sym. off.
define	PTEQ_AFITCNT	Memi[PTEQ_SFITCNT($1)]	    # fit variable counters
define	PTEQ_APAR	Memi[PTEQ_SPAR($1)]	    # parameter sym. offsets
define	PTEQ_APARVAL	Memr[P2R(PTEQ_SPARVAL($1))]	    # parameter values
define	PTEQ_APLIST	Memi[PTEQ_SPLIST($1)]	    # fitting parameter list
define	PTEQ_ADER	Memi[PTEQ_SDER($1)]	    # derivative string offsets
define	PTEQ_ARPNDER	MEMP[PTEQ_SRPNDER($1)]	    # derivative code

# Individual access for variable symbols and counters.
define	PTEQ_REFVAR	Memi[PTEQ_SREFVAR($1) + $2 - 1]	# ref. var. symbol off.
define	PTEQ_REFCNT	Memi[PTEQ_SREFCNT($1) + $2 - 1]	# ref. var. counter
define	PTEQ_FITVAR	Memi[PTEQ_SFITVAR($1) + $2 - 1] # fit var. symbol off.
define	PTEQ_FITCNT	Memi[PTEQ_SFITCNT($1) + $2 - 1] # fit var. counter

# Individual access for fitting parameter symbols, values and list. The
# second argument is the parameter number, relative to the equation.
define	PTEQ_PAR	Memi[PTEQ_SPAR($1)    + $2 - 1] # symbol offset
define	PTEQ_PARVAL	Memr[P2R(PTEQ_SPARVAL($1)) + $2 - 1] # value
define	PTEQ_PLIST	Memi[PTEQ_SPLIST($1)  + $2 - 1] # list element

# Individual access for derivative equation string offsets and codes. The
# second argument is the parameter number relative to the equation.
define	PTEQ_DER	Memi[PTEQ_SDER($1)    + $2 - 1] # string offset
define	PTEQ_RPNDER	Memi[PTEQ_SRPNDER($1) + $2 - 1] # code
