# PARSER.H - Parser codes to use with the prget and prput procedures.


# Parser tables (pointer)

define	SYMTABLE	1		# symbol table
define	OBSTABLE	2		# observational variable table
define	CATTABLE	3		# catalog variable table
define	PARTABLE	4		# fitting and constant parameter table
define	EXTTABLE	5		# extinction equation table
define	TRNTABLE	6		# transformation equation table
define	SETTABLE	7		# set equation table
define	TRCATTABLE	8		# temporary ref. catalog var. table
define	TROBSTABLE	9		# temporary ref. obs. var. table
define	TFCATTABLE	10		# temporary fit catalog var. table
define	TFOBSTABLE	11		# temporary fit obs. var. table
define	TPARTABLE	12		# temporary parameter table


# Parser counters (int)

define	NERRORS		20		# errors
define	NWARNINGS	21		# warnings
define	NOBSVARS	22		# observational variables
define	NCATVARS	23		# catalog variables
define	NFITPARS	24		# fitting parameters
define	NTOTPARS	25		# parameters (fitting + constant)
define	NSETEQS		26		# set equations
define	NEXTEQS		27		# extinction equations
define	NTRNEQS		28		# transformation equations


# Parser column limit values (int)

define	MINCOL		30		# min. input column
define	MINOBSCOL	31		# min. observational column
define	MAXOBSCOL	32		# max. observational column
define	MINCATCOL	33		# max. catalog column
define	MAXCATCOL	34		# max. catalog column


# Parser flags (int)

define	FLAGEQSECT	40		# equation section
define	FLAGERRORS	41		# flag errors (YES/NO)


# Symbol types (int)

define	PTY_OBSVAR	50		# observation input variable
define	PTY_CATVAR	51		# catalog input variable
define	PTY_FITPAR	52		# fitting parameter
define	PTY_CONST	53		# constant parameter
define	PTY_SETEQ	54		# set equation
define	PTY_EXTEQ	55		# extinction equation
define	PTY_TRNEQ	56		# transformation equation


# Symbol attributes (int, pointer)

define	PSYMTYPE	60		# type         (int)
define	PSYMNUM		61		# number       (int)
define	PSYMSUB		62		# substructure (pointer)


# Input variable attributes (int)

define	PINPCOL		70		# column
define	PINPERRCOL	71		# error column
define	PINPWTSCOL	72		# error column
define	PINPSPARE	73		# spare flag


# Fitting parameter attributes (real)

define	PFITVALUE	80		# value
define	PFITDELTA	81		# delta


# Set equation character strings (int)

define	PSEQEQ		90		# equation
define	PSEQERROR	91		# error
define	PSEQERRMIN	92		# minimum error
define	PSEQERRMAX	93		# maximum error
define	PSEQWEIGHT	94		# weight
define	PSEQWTSMIN	95		# minimum weight
define	PSEQWTSMAX	96		# maximum weight


# Set equation code pointers (int)

define	PSEQRPNEQ	100		# equation
define	PSEQRPNERROR	101		# error
define	PSEQRPNERRMIN	102		# minimum error
define	PSEQRPNERRMAX	103		# maximum error
define	PSEQRPNWEIGHT	104		# weight
define	PSEQRPNWTSMIN	105		# minimum weight
define	PSEQRPNWTSMAX	106		# maximum weight


# Transformation equation counters (int)

define	PTEQNRCAT	110		# number of catalog var. (reference)
define	PTEQNROBS	111		# number of obs. variables (reference)
define	PTEQNRVAR	112		# number of variables (reference)
define	PTEQNFCAT	113		# number of catalog variables (fit)
define	PTEQNFOBS	114		# number of obs. variables (fit)
define	PTEQNFVAR	115		# number of variables (fit)
define	PTEQNVAR	116		# total number of variables (ref + fit)
define	PTEQNPAR	117		# number of parameters
define	PTEQNFPAR	118		# number of fitting parameters


# Transformation equation character strings (pointer)

define	PTEQFIT		120		# fit
define	PTEQREF		121		# reference
define	PTEQDER		122		# derivatives
define	PTEQERROR	123		# error
define	PTEQERRMIN	124		# minimum error
define	PTEQERRMAX	125		# maximum error
define	PTEQWEIGHT	126		# weight
define	PTEQWTSMIN	127		# minimum weight
define	PTEQWTSMAX	128		# maximum weight
define	PTEQXPLOT	129		# x plot
define	PTEQYPLOT	130		# y plot


# Transformation equation codes (pointer)

define	PTEQRPNFIT	140		# fitting
define	PTEQRPNREF	141		# reference
define	PTEQRPNDER	142		# derivatives
define	PTEQRPNERROR	143		# error
define	PTEQRPNERRMIN	144		# minimum error
define	PTEQRPNERRMAX	145		# maximum error
define	PTEQRPNWEIGHT	146		# weight
define	PTEQRPNWTSMIN	147		# minimum weight
define	PTEQRPNWTSMAX	148		# maximum weight
define	PTEQRPNXPLOT	149		# x plot
define	PTEQRPNYPLOT	150		# y plot


# Transformation equation buffers (pointer)

define	PTEQSPAR	160		# parameter symbols
define	PTEQSPARVAL	161		# parameter values
define	PTEQSPLIST	162		# parameter list
define	PTEQSREFVAR	163		# reference eq. variable symbols
define	PTEQSREFCNT	164		# reference eq. variable counters
define	PTEQSFITVAR	165		# fit eq. variable symbols
define	PTEQSFITCNT	166		# fit eq. variable counters


# Transformaion equation variable symbols and counters (int)

define	PTEQREFVAR	170		# reference eq. variable symbols
define	PTEQREFCNT	171		# reference eq. counter
define	PTEQFITVAR	172		# fit eq. variable symbols
define	PTEQFITCNT	173		# fit eq. counter


# Transformation equation fitting and constant parameter
# attributes (int, real)

define	PTEQPAR		180		# parameter symbols (int)
define	PTEQPARVAL	181		# parameter values  (real)
define	PTEQPLIST	182		# parameter list    (int)
