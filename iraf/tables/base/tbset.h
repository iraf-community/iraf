# tbset -- This header file defines the lengths of character strings
# and defines parameters that can be set by tbpset and/or read by tbpsta.
#
# Phil Hodge,  2-Oct-1987  Add FIO options and parameters for column info.
# Phil Hodge,  2-Apr-1993  Add TBL_ROWLEN_CHAR and TBL_ROWLEN_CHAR_USED.
# Phil Hodge,  8-Apr-1993  Add TBL_VERSION for getting software version number.
# Phil Hodge,  4-Nov-1993  Add TBL_LAST_ROW for access to number of rows. DEL
# Phil Hodge, 15-Sep-1994  Add TBL_TYPE_MI and TBL_COL_DIMENSION.
# Phil Hodge,  1-Jul 1995  Add TBL_TYPE_FITS and TBL_TYPE_CDF.
# Phil Hodge, 14-Apr-1998  Increase the sizes of SZ_COLNAME, SZ_COLUNITS,
#			and SZ_COLFMT to 79 (from 19, 19, and 9 respectively).
# Phil Hodge,  2-Jun-1999  Add table subtype definitions.

# Lengths of character strings for column information in memory.
define	SZ_COLNAME		79	# Size of a column name
define	SZ_COLUNITS	SZ_COLNAME	# Size of string for units
define	SZ_COLFMT	SZ_COLNAME	# Size for print format

# This section defines the sizes and locations (unit = SZ_CHAR) of records
# for user parameters in the table "header".  The keyword takes the first
# eight characters of a parameter record.  The next character is the data
# type, and the tenth through 80th contain the parameter value.
# If SZB_CHAR is something strange like 3 or 6 then SZ_PARREC should be
# increased to 84.

define	SZ_PARREC	80		# size of a parameter record (SZ_CHAR)
define	SZ_KEYWORD	8		# size of a keyword
define	LOCN_DTYPE	9		# location of datatype character
define	START_OF_VALUE	10		# location of start of value

# This section defines parameters that can be set or read.
# Those parameters with (R) beginning the comment are relevant only
# to row-ordered tables, and those with (C) are relevant only to
# column-ordered tables.  TBL_MAXPAR and TBL_MAXCOLS are not hard limits;
# they are for setting or determining the amount of space allocated for
# user parameters and column descriptors respectively.

# These can be set by tbpset and/or read by tbpsta:
define	TBL_ROWLEN		1	# (R) row length to allocate (SZ_REAL)
define	TBL_INCR_ROWLEN		2	# (R) increase row length (SZ_REAL)
define	TBL_ALLROWS		3	# (C) number of rows to allocate
define	TBL_INCR_ALLROWS	4	# (C) increase alloc num of rows
define	TBL_WHTYPE		5	# which type of table?
define	TBL_MAXPAR		6	# maximum number of user param
define	TBL_MAXCOLS		7	# maximum number of columns
define	TBL_ROWLEN_CHAR		8	# (R) row length to allocate (SZ_CHAR)

# The table subtype can be read by tbpsta.  The subtype can be set for
# a text table but not for a FITS table.
define	TBL_SUBTYPE		9	# subtype of text or FITS table

# Table types.
# Note:  TBL_TYPE_MI and TBL_TYPE_CDF are not implemented.
define	TBL_TYPE_MI	10	# machine independent table format
define	TBL_TYPE_S_ROW		11	# row-ordered table format
define	TBL_TYPE_S_COL		12	# column-ordered table format
define	TBL_TYPE_TEXT		13	# text table
define	TBL_TYPE_FITS		14	# FITS table
define	TBL_TYPE_CDF	15	# table in a CDF file

# Table subtypes.
define	TBL_SUBTYPE_UNKNOWN	1000	# subtype not known, or irrelevant
define	TBL_SUBTYPE_SIMPLE	1301	# ordinary text table
define	TBL_SUBTYPE_EXPLICIT	1302	# text table with explicit column def
define	TBL_SUBTYPE_ASCII	1401	# FITS ASCII table extension
define	TBL_SUBTYPE_BINTABLE	1402	# FITS binary table extension
define	TBL_SUBTYPE_IMAGE	1403	# FITS primary header

# These can be read by tbpsta but may not be set:
define	TBL_NROWS		21	# number of rows written to
define	TBL_NCOLS		22	# number of columns defined
define	TBL_ROWLEN_USED		23	# (R) amount of row len used (SZ_REAL)
define	TBL_NPAR		24	# number of user parameters
define	TBL_ROWLEN_CHAR_USED	25	# (R) amount of row len used (SZ_CHAR)
define	TBL_VERSION		26	# version that created the table

# These have to do with the file size and file I/O buffer size.
define	TBL_ADVICE		31	# set RANDOM or SEQUENTIAL
define	TBL_BUFSIZE		32	# get buffer size in char

define	TBL_DATA_SIZE		34	# get size of table data in char

# These are for information about a column.
define	TBL_COL_NAME		41	# column name
define	TBL_COL_UNITS		42	# units for column
define	TBL_COL_FMT		43	# print format for displaying values
define	TBL_COL_DATATYPE	44	# data type (-n for char string)
define	TBL_COL_NUMBER		45	# column number
define	TBL_COL_FMTLEN		46	# length for printing using print fmt
define	TBL_COL_LENDATA		47	# number of elements if it's an array
define	TBL_COL_DIMENSION	48	# dimension of array
