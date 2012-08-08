# tbtables.h -- Internal definitions for the table I/O package.

# Software version number.
# Version 0 corresponds to STSDAS and TABLES versions 1.2.3 and earlier.
# The row length was restricted to integral multiples of the size of a
# real number.
# Version 1 begins with STSDAS and TABLES version 1.3.  Short integer
# datatype was introduced, and character strings were rounded up to a
# multiple of the number of bytes in a char.  The row length is allowed
# to be any integral multiple of SZ_CHAR.
# Version 2 allows header parameters to have comments.
# This change was made after TABLES version 1.3.3 was released.
define	TBL_CURRENT_VERSION	2

# Default maximum number of user parameters.  The current value is TB_MAXPAR.
define	DEFMAXPAR		5

# Default maximum number of columns.  The current value is TB_MAXCOLS.
define	DEFMAXCOLS		5

# This section describes the size information record.
define	LEN_SIZINFO		12		# unit = SZ_INT32
define	SZ_SIZINFO		(LEN_SIZINFO * SZ_INT32)
define	S_NPAR			$1[1]		# Number of user parameters
define	S_MAXPAR		$1[2]		# Max number of user parameters
define	S_NROWS			$1[3]		# Number of rows
define	S_ALLROWS		$1[4]		# Number of rows allocated
define	S_NCOLS			$1[5]		# Number of columns defined
define	S_MAXCOLS		$1[6]		# Current max number of columns
define	S_COLUSED		$1[7]		# Chars used by defined columns
define	S_ROWLEN		$1[8]		# Total row length alloc (chars)
define	S_TYPE			$1[9]		# Type (row or column ordered)
define	S_VERSION		$1[10]		# Software version number

# This is the size of the table-descriptor structure.
define	LEN_TBLSTRUCT		(28)

# General descriptive information.  (R) means relevant only for row-ordered
# tables, while (C) means relevant only for column-ordered tables.
define	TB_TYPE			Memi[$1]	# what type of table
define	TB_NPAR			Memi[$1+1]	# number of user paramters
define	TB_MAXPAR		Memi[$1+2]	# max number of user paramters
define	TB_NROWS		Memi[$1+3]	# number of rows
define	TB_ALLROWS		Memi[$1+4]	# (C) allocated number of rows
define	TB_NCOLS		Memi[$1+5]	# number of columns
define	TB_MAXCOLS		Memi[$1+6]	# current max number of columns
define	TB_COLUSED		Memi[$1+7]	# (R) chars used by columns
define	TB_ROWLEN		Memi[$1+8]	# (R) row length = chars alloc
define	TB_VERSION		Memi[$1+9]	# Software version number
define	TB_BOD			Meml[$1+10]	# L beg of data (in SZ_CHAR)
define	TB_IOMODE		Memi[$1+11]	# I/O mode

# Flags
define	TB_IS_OPEN		Memb[$1+12]	# Table is open?
define	TB_READONLY		Memb[$1+13]	# Readonly?
define	TB_MODIFIED		Memb[$1+14]	# Actually been changed?

# File descriptor for the table file
define	TB_FILE			Memi[$1+15]

# Pointers.  TB_INDEF is only used for row-ordered tables.
define	TB_INDEF		Memi[$1+16]	# Pointer to indef record buffer
define	TB_COLPTR		Memi[$1+17]	# Ptr to array of column ptrs

# These are for tables in CDF files or FITS files.
define	TB_F_TYPE		Memi[$1+18]	# CDF, FITS, or ordinary file
define	TB_HDU			Memi[$1+19]	# number of HDU in FITS file
define	TB_EXTVER		Memi[$1+20]	# version number
define	TB_OVERWRITE		Memi[$1+21]	# +1 --> yes, 0 --> no
define	TB_HDUTYPE		Memi[$1+22]	# 1--> ascii; 2 --> binary
define	TB_CD			Memi[$1+23]	# returned by cd_open()
define	TB_EXTNAME_PTR		Memi[$1+24]	# pointer to CDF name or EXTNAME
define	TB_EXTNAME		Memc[TB_EXTNAME_PTR($1)]

# These two are for text tables.
define	TB_COMMENT		Memi[$1+25]	# pointer to comment string
define	TB_SZ_COMMENT		Memi[$1+26]	# size of comment string

# Table name
define	TB_NAME_PTR		Memi[$1+27]	# pointer to table name string
define	TB_NAME			Memc[TB_NAME_PTR($1)]


# Array of pointers to column information.  This array can be reallocated
# to allow more columns; the current size at any time is TB_MAXCOLS.
define	TB_COLINFO		Memi[TB_COLPTR($1)+$2-1]



# Column information structure.
define	LEN_COLSTRUCT		16		# unit = SZ_STRUCT
define	SZ_COLSTRUCT		(LEN_COLSTRUCT * SZ_STRUCT)

define	COL_NUMBER		Memi[$1]	# Column number
define	COL_OFFSET		Memi[$1+1]	# Offset from start of row
define	COL_LEN			Memi[$1+2]	# Chars for data element
define	COL_DTYPE		Memi[$1+3]	# Data type
define	COL_NAME		Memc[P2C($1+4)]		# Column name	19
define	COL_UNITS		Memc[P2C($1+9)]		# Units		19
define	COL_FMT			Memc[P2C($1+14)]	# Print format	7
# Next available field is ($1 + 16).


# Definitions of data types.  These agree with iraf.h.
define	TBL_TY_BOOL	1
define	TBL_TY_CHAR	2
define	TBL_TY_SHORT	3
define	TBL_TY_INT	4
define	TBL_TY_REAL	6
define	TBL_TY_DOUBLE	7

# Undefined double for tables.  This agrees with the pre-IRAF 2.11 INDEFD.
define	TBL_INDEFD	1.6d38
define	TBL_IS_INDEFD	(($1)==TBL_INDEFD)

# These two (which are in tbset.h) are used for the file type TB_F_TYPE
# as well as table type TB_TYPE.
# (moved) define	TBL_TYPE_FITS	14	# FITS table
# (moved) define	TBL_TYPE_CDF	15	# common datafile format
# These two are modifiers for the table type in case it's a FITS table.
# They are the value of TB_HDUTYPE.
define	TBL_FITS_ASCII   1	# FITS ASCII table
define	TBL_FITS_BINARY  2	# FITS BINTABLE
