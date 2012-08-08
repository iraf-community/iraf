# These definitions are for FITS BINTABLEs and the FITSIO interface.

# These are values for the ORIGIN keyword to be added to new FITS files.
define	FITS_ORIGIN	"STScI-STSDAS/TABLES"
define	FITS_ORIGIN_CMT	"Tables version 2002-02-22"

# These are the three possible values of hdutype (as returned by fsmahd,
# for example).
define	TBL_FITS_IMAGE   0	# FITS IMAGE extension or primary HDU
define	TBL_FITS_ASCII   1	# FITS ASCII table
define	TBL_FITS_BINARY  2	# FITS BINTABLE

# For defining 2-D char arrays.  (values and names changed 1999 Mar 10 by PEH)
define	SZ_FTTYPE	70	# size of character string for column name
define	SZ_FTFORM	70	# size of character string for column format
define	SZ_FTUNIT	70	# size of character string for column units

# Undefined values for FITS BINTABLE.
define	FITS_INDEFI	(-2147483647)
define	FITS_INDEFS	(-32767)

# Error return codes.
define	FITS_END_OF_FILE	107
define	FITS_KEYWORD_MISSING	202
define	FITS_TNULL_NOT_SET	314
