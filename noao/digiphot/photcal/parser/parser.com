# Parser common

# Symbol tables
pointer	symtable		# parser symbol table

# Sequential tables
pointer	obstable		# observational variable table
pointer	cattable		# catalog variable table
pointer	partable		# fitting and constant parameter table
pointer	settable		# set equation table
pointer	exttable		# extinction equation table
pointer	trntable		# transformation equation table
pointer	trcattable		# temporary reference eq. catalog var. table
pointer	trobstable		# temporary ref. eq. observational var. table
pointer	tfcattable		# temporary fit eq. catalog var. table
pointer	tfobstable		# temporary fit eq. observational var. table
pointer	tpartable		# temporary parameter table

# Counters
int	nerrors			# number of semantic errors
int	nwarnings		# number of warnings
int	nobsvars		# number of observational input variables
int	ncatvars		# number of catalog input variables
int	nfitpars		# number of fitting parameters
int	ntotpars		# number of fitting and constant parameters
int	nseteqs			# number of set equations
int	nexteqs			# number of extinction equations
int	ntrneqs			# number of transformation equations

# Column limits
int	mincol			# minumum input column
int	minobscol		# minumum observational column
int	maxobscol		# maximum observational column
int	mincatcol		# minumum catalog column
int	maxcatcol		# maximum catalog column

# Flags
int	flageqsect		# equation section
int	flagerrors		# print error messages (YES/NO)

common	/parcom/ symtable,
		 obstable, cattable, partable,
		 settable, exttable, trntable,
		 trcattable, trobstable, tfcattable, tfobstable, tpartable,
		 nerrors, nwarnings,
		 nobsvars, ncatvars, nfitpars, ntotpars,
		 nseteqs, nexteqs, ntrneqs,
		 mincol, minobscol, maxobscol, mincatcol, maxcatcol,
		 flageqsect, flagerrors
