# SYMTAB definitions.

define	MAX_HASHCHARS	18		# max characters used in hash function
define	SZ_ASCII	128		# max possible character values
define	INC_START	0.50		# used in overflow algorithm
define	INC_GROW	2		# growing factor for increment
define	MAX_INCREMENT	32768		# max sbuf or stab increment
define	MAX_SZKEY	256		# arbitrarily large number

# Symbol table descriptor.

define	LEN_SYMTAB	256
define	MAGIC		0123124B

define	ST_MAGIC	Memi[$1]	# for error checking
define	ST_NAME		Memi[$1+1]	# optional name for symbol table
define	ST_LASTSYMBOL	Memi[$1+2]	# last element entered
define	ST_NSYMBOLS	Memi[$1+3]	# number of symbols in table
			# (extra space)
define	ST_INDEX	Memi[$1+5]	# pointer to buffer of thread indices
define	ST_INDEXLEN	Memi[$1+6]	# length of index
			# (extra space)
define	ST_SBUFP	Memi[$1+10]	# string buffer
define	ST_SBUFLEN	Memi[$1+11]	# current size of string buffer
define	ST_SBUFOP	Memi[$1+12]	# next location in string buffer
define	ST_SBUFINC	Memi[$1+13]	# increment if overflow occurs
define	ST_SBUFNGROW	Memi[$1+14]	# number of reallocs of sbuf
			# (extra space)
define	ST_STABP	Memi[$1+20]	# symbol table
define	ST_STABLEN	Memi[$1+21]	# symbol table length
define	ST_STABOP	Memi[$1+22]	# next location in symbol table
define	ST_STABINC	Memi[$1+23]	# increment if overflow occurs
define	ST_STABNGROW	Memi[$1+24]	# number of reallocs of stab
			# (extra space)
define	ST_ASCII	Memi[($1+30)+$2]

# Symstruct.  STAB contains an array of these, each of which is linked both
# on a thread and on the global lifo list.

define	LEN_SYMSTRUCT	4

define	E_NEXTHASH	Memi[$1]	# next element on thread
define	E_NEXTGLOB	Memi[$1+1]	# next element on global list
define	E_THREAD	Memi[$1+2]	# index of thread in INDEX array
define	E_KEY		Memi[$1+3]	# index of key name

define	E_USERFIELDS	($1+LEN_SYMSTRUCT)
define	E_BASE		($1-LEN_SYMSTRUCT)

# Magic marker structure (for mark/free).

define	LEN_MARKER	2
define	M_SBUFOP	Memi[$1]	# saved string buffer offset
define	M_NSYMBOLS	Memi[$1+1]	# nsymbols in table below marker
