# tbtables.h -- Internal definitions for the table I/O package.

# Software version number.
#
# Version 0 corresponds to STSDAS and TABLES versions 1.2.3 and earlier.
# The row length was restricted to integral multiples of the size of a
# real number.
#
# Version 1 begins with STSDAS and TABLES version 1.3.  Short integer
# datatype was introduced, and character strings were rounded up to a
# multiple of the number of bytes in a char.  The row length is allowed
# to be any integral multiple of SZ_CHAR.
#
# Version 2 allows header parameters to have comments.
# This change was made after TABLES version 1.3.3 was released.
#
# Version 3 allows the character strings in column definitions to have
# one more character, and the end-of-string character may be absent.
#   14-Apr-1998

define	TBL_CURRENT_VERSION	3

# Default maximum number of header parameters.  The current value is TB_MAXPAR.
define	DEFMAXPAR		5

# Default maximum number of columns.  The current value is TB_MAXCOLS.
define	DEFMAXCOLS		5

# This section describes the size information record.
define	LEN_SIZINFO		12		# unit = SZ_INT32
define	SZ_SIZINFO		(LEN_SIZINFO * SZ_INT32)
define	S_NPAR			$1[1]		# Number of header parameters
define	S_MAXPAR		$1[2]		# Max number of header pars
define	S_NROWS			$1[3]		# Number of rows
define	S_ALLROWS		$1[4]		# Number of rows allocated
define	S_NCOLS			$1[5]		# Number of columns defined
define	S_MAXCOLS		$1[6]		# Current max number of columns
define	S_COLUSED		$1[7]		# Chars used by defined columns
define	S_ROWLEN		$1[8]		# Total row length alloc (chars)
define	S_TYPE			$1[9]		# Type (row or column ordered)
define	S_VERSION		$1[10]		# Software version number

# This is the size of the table-descriptor structure.
define	LEN_TBLSTRUCT		(39)

# File descriptor for the table file, or pointer to CFITSIO descriptor for
# FITS files.  Note that by including TB_FILE2 there's space for two words.
define	TB_FILE			Memi[$1]
define	TB_FILE2		Memi[$1+1]	# second half of pointer

# Table file name; this can be an IRAF virtual file name.
define	TB_NAME_PTR		Memi[$1+2]	# pointer to table name string
define	TB_NAME			Memc[TB_NAME_PTR($1)]

# Table file name converted to a host operating system file name.
# This name is needed for CFITSIO, since that uses host OS I/O.
define	TB_OS_FILENAME_PTR	Memi[$1+3]	# pointer to OS file name string
define	TB_OS_FILENAME		Memc[TB_OS_FILENAME_PTR($1)]

# General descriptive information.  (R) means relevant only for row-ordered
# tables, (C) is for column-ordered tables, and (F) is for FITS tables.
# For row-ordered tables, TB_ROWLEN is the allocated row length in SPP chars,
# while for FITS tables, TB_ROWLEN is the value of NAXIS1, the length in bytes.
define	TB_TYPE			Memi[$1+4]	# what type of table
define	TB_SUBTYPE		Memi[$1+5]	# subtype of text or FITS table
define	TB_NPAR			Memi[$1+6]	# number of header paramters
define	TB_MAXPAR		Memi[$1+7]	# max number of header paramters
define	TB_NROWS		Memi[$1+8]	# number of rows
define	TB_ALLROWS		Memi[$1+9]	# (C) allocated number of rows
define	TB_NCOLS		Memi[$1+10]	# number of columns
define	TB_MAXCOLS		Memi[$1+11]	# current max number of columns
define	TB_COLUSED		Memi[$1+12]	# (R) chars used by columns
define	TB_ROWLEN		Memi[$1+13]	# (R,F) row length
define	TB_VERSION		Memi[$1+14]	# Software version number
define	TB_BOD			Meml[$1+15]	# L beg of data (in SZ_CHAR)
define	TB_IOMODE		Memi[$1+16]	# I/O mode

# Flags
define	TB_IS_OPEN		Memb[$1+17]	# Table is open?
define	TB_READONLY		Memb[$1+18]	# I/O mode is read-only?
define	TB_MODIFIED		Memb[$1+19]	# Has table been changed?
define	TB_INDEF_IS_CURRENT	Memb[$1+20]	# (F) TB_INDEF is up-to-date?

# Pointers.  TB_INDEF is used for row-ordered tables and FITS tables.
define	TB_INDEF		Memi[$1+21]	# Pointer to indef record buffer
define	TB_COLPTR		Memi[$1+22]	# Ptr to array of column ptrs

# These are for tables in FITS files.
define	TB_HDU			Memi[$1+23]	# number of HDU in FITS file
define	TB_EXTVER		Memi[$1+24]	# version number
define	TB_OVERWRITE		Memi[$1+25]	# +1 --> yes, 0 --> no
define	TB_CD			Memi[$1+26]	# returned by cd_open()
define	TB_EXTNAME_PTR		Memi[$1+27]	# pointer to EXTNAME
define	TB_EXTNAME		Memc[TB_EXTNAME_PTR($1)]

# These are for row and column selectors.
# TB_ROW_SELECT will be YES if there is a row selector in effect for the
# current table.  TB_ROWSET will in this case not be NULL, and the actual
# row number corresponding to a selected row number will be:
#	rst_rownum (TB_ROWSET(tp), selected_row)
# TB_COLUMN_SELECT will be YES if there is a column selector in effect for
# the current table.  TB_SELCOL(tp,M) will in this case be the selectors
# descriptor of Mth selected column (note:  not the same as column descriptor)

define	TB_ROW_SELECT		Memi[$1+28]	# row selection turned on?
define	TB_NSEL_ROWS		Memi[$1+29]	# number of selected rows
define	TB_ROWSET		Memi[$1+30]	# pointer to row set

define	TB_COLUMN_SELECT	Memi[$1+31]	# column selection turned on?
define	TB_NSEL_COLS		Memi[$1+32]	# number of selected columns
define	TB_MAX_SELCOLS		Memi[$1+33]	# size of TB_SELCOL_PTR array
define	TB_SELCOL_PTR		Memi[$1+34]
define	TB_SELCOL		Memi[TB_SELCOL_PTR($1)+$2-1]

# These are for text tables.
define	TB_COMMENT		Memi[$1+35]	# pointer to comment string
define	TB_SZ_COMMENT		Memi[$1+36]	# size of comment string
define	TB_KEYLIST_PTR		Memi[$1+37]	# pointer to list of keywords
define	TB_KEYWORD		Memi[TB_KEYLIST_PTR($1)+$2-1]  # ptr to keyword

# Table file name; this can be an IRAF virtual file name.
define	TB_SRC_PTR		Memi[$1+38]	# pointer to source name string
define	TB_SRC			Memc[TB_SRC_PTR($1)]


# Array of pointers to column information.  This array can be reallocated
# to allow more columns; the current size at any time is TB_MAXCOLS.
define	TB_COLINFO		Memi[TB_COLPTR($1)+$2-1]



# Column information structures.

# This is the size of the buffer for a column name (i.e. including EOS).
define	FULL_SZ_COLNAME		(SZ_COLNAME+SZ_CHAR)

# This structure is for maintaining a column definition in memory.
# The size is for five integers plus three strings, unit = SZ_STRUCT.
# Note that some of these are only meaningful for stsdas format tables,
# and some others are only meaningful for FITS tables.
# COL_LEN is the number of char taken up by one cell in the table,
# the entire array (if the column contains arrays) at one row & column.
# COL_OFFSET is the offset in char from the beginning of a row to
# the beginning of a cell in that row (although it is also used for
# a column ordered table).
# In a FITS table, the true data type in the table may be an integer type
# (byte, short, or int), but with TSCALi and TZEROi keywords to scale the
# values to floating point.  In this case, COL_DTYPE will be real or
# double, while COL_TDTYPE will be the integer type, and COL_TSCAL and
# COL_TZERO will be something other than 1 and 0 respectively.
# COL_DTYPE is the apparent data type, the type as seen by the user and
# by most of the routines in this interface.

define	LEN_COLSTRUCT	(10 + 3*(FULL_SZ_COLNAME)/SZ_STRUCT)

define	COL_NUMBER		Memi[$1]	# Column number
define	COL_OFFSET		Memi[$1+1]	# Offset from start of row
define	COL_LEN			Memi[$1+2]	# Chars for one cell
define	COL_DTYPE		Memi[$1+3]	# Data type
define	COL_TDTYPE		Memi[$1+4]	# True data type, in FITS table
define	COL_NELEM		Memi[$1+5]	# Length of array
define	COL_TSCAL		Memd[P2D($1+6)]	# TSCAL, if FITS table
define	COL_TZERO		Memd[P2D($1+8)]	# TZERO, if FITS table
define	COL_NAME	Memc[P2C($1+10)]
define	COL_UNITS	Memc[P2C($1+10+  FULL_SZ_COLNAME/SZ_STRUCT)]
define	COL_FMT		Memc[P2C($1+10+2*FULL_SZ_COLNAME/SZ_STRUCT)]

# This structure is a copy of the bytes read from or written to an
# stsdas format table (either row or column ordered).
define	LEN_COLDEF		16		# unit = SZ_STRUCT
define	SZ_COLDEF		(LEN_COLDEF * SZ_STRUCT32)

# Lengths of character strings for column information in an stsdas format
# table.  Note that SZ_COLNAME, etc, defined in tbset.h are larger than these.
# Note:  These have been increased by one character, so there may not be room
# for an EOS.
define	SZ_CD_COLNAME		20	# Size of a column name
define	SZ_CD_COLUNITS		20	# Size of string for units
define	SZ_CD_COLFMT		8	# Size for print format

define	CD_COL_NUMBER		Memi[$1]	# Column number
define	CD_COL_OFFSET		Memi[$1+1]	# Offset from start of row
define	CD_COL_LEN		Memi[$1+2]	# Chars for one cell
define	CD_COL_DTYPE		Memi[$1+3]	# Data type
define	CD_COL_NAME		Memc[P2C($1+4)]		# Column name	20
define	CD_COL_UNITS		Memc[P2C($1+9)]		# Units		20
define	CD_COL_FMT		Memc[P2C($1+14)]	# Print format	8


# Definitions of data types.  These agree with iraf.h at the time of writing.
define	TBL_TY_BOOL	1
define	TBL_TY_CHAR	2
define	TBL_TY_SHORT	3
define	TBL_TY_INT	4
define	TBL_TY_REAL	6
define	TBL_TY_DOUBLE	7

# Undefined double for tables.  This agrees with the pre-IRAF 2.11 INDEFD.
define	TBL_INDEFD	1.6d38
define	TBL_IS_INDEFD	(($1)==TBL_INDEFD)
