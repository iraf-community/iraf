# Help Definitions.

# Control Structure.  Contains all control parameters.  Pointer to structure
# is passed to Lroff and on by Lroff to the input and output procedures.
# With the exceptions of H_EOF and H_NLINES, the control parameters are
# read only outside the main routine.

define	LEN_CTRLSTRUCT	180
define	SZ_SECNAME	39		# section name, single section mode
define	SZ_PARNAME	39		# parameter name
define	SZ_TEMPLATE	79		# the original modules template
define	SZ_HELPDB	512		# max chars in helpdb file list

define	H_IN		Memi[$1]	# input file descriptor
define	H_OUT		Memi[$1+1]	# output file descriptor
define	H_OPTION	Memi[$1+2]	# option code (see below)
define	H_TTY		Memi[$1+3]	# TTY device descriptor
define	H_LMARGIN	Memi[$1+4]	# permanent left margin for Lroff
define	H_RMARGIN	Memi[$1+5]	# permanent right margin for Lroff
define	H_RAWIN		Memi[$1+6]	# if YES, do not look at input
define	H_RAWOUT	Memi[$1+7]	# if YES, do not process output
define	H_FILTER_INPUT	Memi[$1+8]	# if YES, filter out part of input
define	H_PAGINATE	Memi[$1+9]	# paginate output?
define	H_MANPAGE	Memi[$1+10]	# manpage style output?
define	H_NLPP		Memi[$1+11]	# number of lines per manual page
define	H_NLINES	Memi[$1+12]	# number of output lines on page
define	H_STATE		Memi[$1+13]	# input state, hinput()
define	H_EOF		Memi[$1+14]	# input should return EOF to Lroff
define	H_QUIT		Memi[$1+15]	# stop program
define	H_LENTL		Memi[$1+16]	# length of the TL template list
define	H_ALLMODULES	Memi[$1+17]	# process all modules matching template
			# (extra space)
define	H_SECNAME	Memc[P2C($1+20)]	# section name
define	H_PARNAME	Memc[P2C($1+60)]	# parameter name
define	H_TEMPLATE	Memc[P2C($1+100)]	# module name template

# The nomore flag is set whenever the user responds negatively to the more?
# query.  A nomore at the beginning of a help block or two nomores in a row
# stop the program.

define	NOMORE		(-1)

# Option codes.  Max_options is used by the get_option keyword recognizer,
# which handles abbreviations.

define	O_HELP		1		# print full help block
define	O_SOURCE	2		# print source code
define	O_SYSDOC	3		# print technical system documentation
define	O_ALLDOC	4		# print all documentation (!source)
define	O_FILES		5		# print file names
define	O_DIR		6		# print directory of help blocks
define	O_SUMMARY	7		# summarize contents of help file
define	MAX_OPTIONS	7

define	O_PARAM		7		# output text for single parameter
define	O_SECTION	8		# output text for single section
define	O_MENU		9		# print package menu

# Type codes for filenames.  Passed to hd_getname to fetch the module name
# or a filename from a help directory.

define	TY_MODNAME	0
define	TY_HLP		1
define	TY_SYS		2
define	TY_SRC		3
define	TY_PKG		4
define	TY_MEN		5
define	TY_UNKNOWN	6

# Help block header structure.  A ".help" directive is decoded into this
# structure.  The line number counter should be zeroed when the structure
# is allocated.

define	LEN_HBSTRUCT	1124
define	MAX_KEYS	50
define	SZ_TYPESTR	9		# help block type string
define	SZ_KEY		19		# max size of a key
define	SZ_SECTION	39		# section label, i.e., (Mar84)
define	SZ_TITLE	63		# block title

define	HB_TYPE		Memi[$1]	# type of block (default TY_HLP)
define	HB_LINENO	Memi[$1+1]	# line number within file
define	HB_NKEYS	Memi[$1+2]	# number of keys
define	HB_TYPESTR	Memc[P2C($1+10)]		# blk type string
define	HB_KEY		Memc[P2C($1+20+($2-1)*20)]	# keys
define	HB_SECTION	Memc[P2C($1+1020)]		# section label
define	HB_TITLE	Memc[P2C($1+1060)]		# block title

# Pagination Control Codes.  The pagination directives BP, TP, KS, and KE
# are ignored when output is directed to the terminal, but are important
# when output is piped to the printer.  When the line input routine HINPUT sees
# one of these directives in the input it places a control code in the
# data stream read by Lroff.  Lroff passes control codes on to the output,
# i.e., to HOUTPUT, where pagination takes place.

define	BREAK_PAGE	1		# .bp
define	TEST_PAGE	2		# .tp n
define	START_KEEP	3		# .ks
define	END_KEEP	4		# .ke
