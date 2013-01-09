include <tbset.h>
include "tbtables.h"
include "tblfits.h"

# tbfrcd -- read all column descriptors
# For a FITS table, this routine reads the information describing all
# columns, and it assigns values to the column descriptors.  Memory
# for the column descriptors is assumed to already have been allocated.
# (This is called by tbuopn.)
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 14-Apr-1998  Use strcpy instead of strpak or tbcftp for
#			column name, units, and print format.
# Phil Hodge,  7-Jun-1999  Use TB_SUBTYPE instead of TB_HDUTYPE.
# Phil Hodge,  5-Aug-1999  Rewrite so that it reads all column info
#		in one call, not just the info for a single column;
#		in tbfcd3, assign COL_NELEM.
# Phil Hodge, 23-Jun-2000  The first character of TSCALi & TZEROi was being
#		truncated.  Assign values to COL_TDTYPE, COL_TSCAL, COL_TZERO;
#		change default values of tscal & tzero to 1. & 0. respectively.

procedure tbfrcd (tp, cp, ncols)

pointer tp		# i: pointer to table descriptor
pointer cp[ARB]		# i: pointers to column descriptors
int	ncols		# i: number of columns in cp array
#--
pointer sp
pointer ttype		# scratch for column name
pointer tform		# scratch for column format
pointer tunit		# scratch for column unit
pointer tdisp		# scratch for display format
pointer tscal, tzero	# parameters for scaling from integer to floating
errchk	tbfcd1, tbfcd2, tbfcd3

begin
	if (ncols < 1)
	    return

	call smark (sp)
	call salloc (ttype, (SZ_FTTYPE+1)*ncols, TY_CHAR)
	call salloc (tform, (SZ_FTFORM+1)*ncols, TY_CHAR)
	call salloc (tunit, (SZ_FTUNIT+1)*ncols, TY_CHAR)
	call salloc (tdisp, (SZ_FTTYPE+1)*ncols, TY_CHAR)
	call salloc (tscal, ncols, TY_DOUBLE)
	call salloc (tzero, ncols, TY_DOUBLE)

	# Initialize these arrays to null or indef.
	call tbfcd1 (Memc[ttype], Memc[tform], Memc[tunit], Memc[tdisp],
		Memd[tscal], Memd[tzero], ncols)

	# Read each keyword in the header, and assign values to these
	# arrays as keywords are found.
	call tbfcd2 (tp,
		Memc[ttype], Memc[tform], Memc[tunit], Memc[tdisp],
		Memd[tscal], Memd[tzero], ncols)

	# Loop over columns, interpret info from these arrays, and
	# assign to column descriptors.
	call tbfcd3 (tp, cp,
		Memc[ttype], Memc[tform], Memc[tunit], Memc[tdisp],
		Memd[tscal], Memd[tzero], ncols)

	call sfree (sp)
end

# This routine initializes the array values to null or INDEFD.

procedure tbfcd1 (ttype, tform, tunit, tdisp,
		tscal, tzero, ncols)

char	ttype[SZ_FTTYPE,ncols]	# o: will be initialized to null
char	tform[SZ_FTFORM,ncols]	# o: will be initialized to null
char	tunit[SZ_FTUNIT,ncols]	# o: will be initialized to null
char	tdisp[SZ_FTTYPE,ncols]	# o: will be initialized to null
double	tscal[ncols]		# o: will be initialized to 1.
double	tzero[ncols]		# o: will be initialized to 0.
int	ncols			# i: size of arrays
#--
int	col

begin
	do col = 1, ncols {
	    ttype[1,col] = EOS
	    tform[1,col] = EOS
	    tunit[1,col] = EOS
	    tdisp[1,col] = EOS
	    tscal[col] = 1.d0
	    tzero[col] = 0.d0
	}
end

# This routine reads each header record, checks whether the keyword is one
# of the those that define a column, and if so, extracts the information
# to the appropriate output array.

procedure tbfcd2 (tp,
		ttype, tform, tunit, tdisp,
		tscal, tzero, ncols)

pointer tp			# i: pointer to table descriptor
char	ttype[SZ_FTTYPE,ncols]	# o: will be assigned if keyword found
char	tform[SZ_FTFORM,ncols]	# o: will be assigned if keyword found
char	tunit[SZ_FTUNIT,ncols]	# o: will be assigned if keyword found
char	tdisp[SZ_FTTYPE,ncols]	# o: will be assigned if keyword found
double	tscal[ncols]		# o: will be assigned if keyword found
double	tzero[ncols]		# o: will be assigned if keyword found
int	ncols			# i: size of arrays
#--
pointer sp
pointer buf			# scratch for header record
pointer value			# scratch for keyword value
pointer comment			# scratch for comment for keyword
double	x			# tscal or tzero
int	parnum			# loop index for keyword number
int	col			# column number, read from keyword name
int	ip, ctoi(), ctod()
int	strncmp(), strlen()
int	status			# = 0 is OK
errchk	tbferr

begin
	status = 0

	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (comment, SZ_FNAME, TY_CHAR)

	# Read each keyword in the header.
	do parnum = 1, TB_NPAR(tp) {

	    # Read the record as a string.
	    call fsgrec (TB_FILE(tp), parnum, Memc[buf], status)
	    if (status != 0)
		call tbferr (status)

	    if (Memc[buf] != 'T')
		next

	    ip = 6		# first character of the column number
	    if (ctoi (Memc[buf], ip, col) < 1)
		next

	    # Reject keywords such as "TTYPE5X".
	    if (Memc[buf+ip-1] != ' ' && Memc[buf+ip-1] != '=')
		next

	    # Extract the value.
	    call fspsvc (Memc[buf], Memc[value], Memc[comment], status)
	    if (status != 0)
		call tbferr (status)

	    # Trim trailing and leading blanks and single quotes.
	    ip = strlen (Memc[value]) - 1	# zero indexed
	    while (Memc[value+ip] == ' ' || Memc[value+ip] == '\'') {
		Memc[value+ip] = EOS
		ip = ip - 1
	    }
	    ip = 0
	    while (Memc[value+ip] == ' ' || Memc[value+ip] == '\'')
		ip = ip + 1

	    # Check to see whether this is one of the keywords that we need,
	    # and if so, copy the value to the output array.
	    if (strncmp (Memc[buf], "TTYPE", 5) == 0) {
		call strcpy (Memc[value+ip], ttype[1,col], SZ_FTTYPE)

	    } else if (strncmp (Memc[buf], "TFORM", 5) == 0) {
		call strcpy (Memc[value+ip], tform[1,col], SZ_FTFORM)

	    } else if (strncmp (Memc[buf], "TUNIT", 5) == 0) {
		call strcpy (Memc[value+ip], tunit[1,col], SZ_FTUNIT)

	    } else if (strncmp (Memc[buf], "TDISP", 5) == 0) {
		call strcpy (Memc[value+ip], tdisp[1,col], SZ_FTTYPE)

	    } else if (strncmp (Memc[buf], "TSCAL", 5) == 0) {
		ip = 1
		if (ctod (Memc[value], ip, x) < 1)
		    call error (1, "can't interpret TSCAL keyword")
		tscal[col] = x

	    } else if (strncmp (Memc[buf], "TZERO", 5) == 0) {
		ip = 1
		if (ctod (Memc[value], ip, x) < 1)
		    call error (1, "can't interpret TZERO keyword")
		tzero[col] = x
	    }
	}

	call sfree (sp)
end

# This routine interprets the contents of the ttype, etc, arrays
# and assigns values to the column descriptors.

procedure tbfcd3 (tp, cp,
		ttype, tform, tunit, tdisp,
		tscal, tzero, ncols)

pointer tp			# i: pointer to table descriptor
pointer cp[ncols]		# i: pointers to column descriptors
char	ttype[SZ_FTTYPE,ncols]	# i: array of column names
char	tform[SZ_FTFORM,ncols]	# i: array that defines data types
char	tunit[SZ_FTUNIT,ncols]	# i: array of column units
char	tdisp[SZ_FTTYPE,ncols]	# i: array of print formats
double	tscal[ncols]		# i: array of tscal values
double	tzero[ncols]		# i: array of tzero values
int	ncols			# i: size of arrays
#--
char	pform[SZ_COLFMT]	# print format for column
int	col			# loop index for column number
errchk	tbftya, tbftyb

begin
	do col = 1, ncols {

	    # If there's no column name, assign a default.
	    if (ttype[1,col] == EOS) {
		call sprintf (ttype[1,col], SZ_FTTYPE, "c%d")
		    call pargi (col)
	    }

	    if (tform[1,col] == EOS)
		call error (1, "TFORM not specified; this keyword is required")

	    # Determine the data type, print format and array length.
	    if (TB_SUBTYPE(tp) == TBL_SUBTYPE_ASCII) {

		call tbftya (tform[1,col], tdisp[1,col],
			tscal[col], tzero[col],
			COL_TDTYPE(cp[col]), COL_DTYPE(cp[col]),
			pform, SZ_COLFMT, COL_LEN(cp[col]))
		COL_NELEM(cp[col]) = 1		# does not support arrays

	    } else if (TB_SUBTYPE(tp) == TBL_SUBTYPE_BINTABLE) {

		call tbftyb (tform[1,col], tdisp[1,col],
			tscal[col], tzero[col],
			COL_TDTYPE(cp[col]), COL_DTYPE(cp[col]),
			pform, SZ_COLFMT,
			COL_NELEM(cp[col]), COL_LEN(cp[col]))

	    } else {

		call error (1, "tbfrcd:  invalid HDU type")
	    }

	    # Assign values to column descriptor.

	    COL_NUMBER(cp[col]) = col
	    COL_OFFSET(cp[col]) = 0			# meaningless

	    COL_TSCAL(cp[col]) = tscal[col]
	    COL_TZERO(cp[col]) = tzero[col]

	    call strcpy (ttype[1,col], COL_NAME(cp[col]), SZ_COLNAME)
	    call strcpy (tunit[1,col], COL_UNITS(cp[col]), SZ_COLUNITS)
	    call strcpy (pform, COL_FMT(cp[col]), SZ_COLFMT)
	}
end
