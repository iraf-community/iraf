include <ctype.h>		# for IS_ALNUM
include <tbset.h>
include "tbtables.h"

# tbtcpy -- copy a table
# The filename extension is taken to imply a table type, which may be
# either row-ordered, FITS, or text (or eventually CDF).
#
# If the output is a FITS file and no EXTNAME was given in the outtable
# string, the name of the input table (in brackets and without directory
# prefix) will be appended to outtable as EXTNAME.  If the input is also
# a FITS file, however, the input name will not be appended to outtable
# if the keyword EXTNAME is present in the input header (i.e. we would
# rather copy EXTNAME from the input header than override it with just
# a file name).
#
# Phil Hodge, 28-Dec-1989  Open before copying to verify that it is a table.
# Phil Hodge, 14-May-1992  Check for text table; call tbtext only if binary.
# Phil Hodge, 11-Jul-1995  Use fcopy, fcopyo or tbrcpy.
# Phil Hodge,  5-Oct-1995  For FITS output, set extname = input name.
# Phil Hodge, 11-Dec-1995  Use tbrcsc instead of tbrcpy.
# Phil Hodge,  8-May-1997  Don't require explicit extension for FITS input.
# Phil Hodge, 14-Aug-1997  Call tbtwer if output table is FITS.
# Phil Hodge, 30-Sep-1997  Use tbpsta instead of TB_NROWS & TB_NCOLS;
#			when appending input table name to be used as the
#			EXTNAME for output, use Memc[in] instead of intable.
# Phil Hodge, 15-Jun-1998  Only use fcopy for text input if the entire file
#			 is to be copied, i.e. no row or column selector.
# Phil Hodge, 16-Apr-1999  Call tbttyp to get file type;
#		remove check for STDOUT, since that's done in tbttyp;
#		remove table type from calling sequence of tbparse;
#		don't try to set type to FITS using tbpset;
#		use root_len to skip over input directory prefix for EXTNAME;
#		change SZ_LINE to SZ_FNAME.

procedure tbtcpy (intable, outtable)

char	intable[ARB]		# i: name of table to be copied to outtable
char	outtable[ARB]		# i: name of new table
#--
pointer sp
pointer in, out			# table names without brackets
pointer iextn, oextn		# EXTNAME
pointer scratch
pointer itp, otp		# pointers to descr for input & output tables
pointer icp, ocp		# pointers to arrays of column descriptors
int	itype, otype		# table types based on extension
int	ihdu, ohdu		# HDU number, if any (ignored)
int	ncols			# number of columns
int	nrows			# number of rows in input table
int	row			# loop index for row number
int	junk
int	dotloc			# location of last '.' in file name
int	root_len		# number of char in input directory name
int	exists			# returned by tbttyp and ignored
int	i
bool	from_stdin		# is intable STDIN?
bool	cat_extname		# should we append input name to use as EXTNAME?

pointer tbtopn(), tbcnum()
int	strlen()
int	fnldir()
int	tbparse(), tbttyp()
int	tbpsta()
bool	streq()
errchk	fcopy, fcopyo, tbtopn, tbtcre, tbhcal, tbrcsc, tbparse, tbttyp, tbtwer

begin
	call smark (sp)
	call salloc (in, SZ_FNAME, TY_CHAR)
	call salloc (out, SZ_FNAME, TY_CHAR)
	call salloc (iextn, SZ_FNAME, TY_CHAR)
	call salloc (oextn, SZ_FNAME, TY_CHAR)
	call salloc (scratch, SZ_FNAME, TY_CHAR)

	# Get the file names and EXTNAMEs or numbers for the tables.
	junk = tbparse (intable, Memc[in], Memc[iextn], SZ_FNAME, ihdu)
	if (tbparse (outtable, Memc[out], Memc[oextn], SZ_FNAME, ohdu) < 1)
	    call error (1, "no output name specified")

	# If the input table is a URL, do a dummy open to ensure we convert
	# it before processing.
#	itp = tbtopn (Memc[in], READ_ONLY, NULL)
#	call tbtclo (itp)

	# Get the table type (based on extension, if any, for output table).
	itype = tbttyp (Memc[in], exists)
	otype = tbttyp (Memc[out], exists)

	# No CDF name for CDF file?
	if (Memc[iextn] == EOS && itype == TBL_TYPE_CDF) {
	    call sprintf (Memc[scratch], SZ_FNAME,
	    "can't copy entire CDF file `%s'; specify which table")
		call pargstr (intable)
	    call error (1, Memc[scratch])
	}

	from_stdin = streq (intable, "STDIN")

	# Open the input table (but if it's STDIN we'll open it later).
	if (!from_stdin) {
	    itp = tbtopn (intable, READ_ONLY, NULL)
	    itype = TB_TYPE(itp)		# actual table type
	}

	# Update output table type, if appropriate.

	if (itype == TBL_TYPE_TEXT) {

	    # Check whether the output file name contains an extension.
	    dotloc = 0				# initial value
	    do i = strlen (Memc[out]), 1, -1 {
		if (Memc[out+i-1] == '.') {	# found it
		    dotloc = i
		    break
		}
		if (!IS_ALNUM(Memc[out+i-1]))	# stop at first special char
		    break
	    }
	    if (dotloc > 0) {
		# Output file name has an extension.  Set the table type
		# to stsdas format (row) if the extension is ".tab".
		if (streq (Memc[out+dotloc], "tab")) {
		    otype = TBL_TYPE_S_ROW
		} else if (otype != TBL_TYPE_FITS) {	# don't change FITS
		    otype = TBL_TYPE_TEXT
		}
	    } else {
		otype = TBL_TYPE_TEXT
	    }
	}

	# If we're copying an entire text file to a text file, use fcopy.
	if (itype == TBL_TYPE_TEXT && otype == TBL_TYPE_TEXT &&
		Memc[iextn] == EOS && !from_stdin) {
	    call tbtclo (itp)
	    call fcopy (intable, outtable)
	    call sfree (sp)
	    return					# done
	}

	# If we're reading from STDIN, now is the time to open it.
	# The reason we didn't open it before is that we weren't sure of
	# the output table type; for text output we already used fcopy above.
	if (from_stdin)
	    itp = tbtopn (intable, READ_ONLY, NULL)

	# If the output is a FITS file, and no EXTNAME was given in the
	# file name, append the input table name (without directory)
	# to use as EXTNAME.  If the input is also a FITS file, however,
	# the EXTNAME from the input (if present) will be used for output.
	#        NOTE that we're clobbering the previous contents of Memc[out].
	call strcpy (outtable, Memc[out], SZ_FNAME)
	if (otype == TBL_TYPE_FITS && Memc[oextn] == EOS) {
	    cat_extname = true				# may be reset below
	    if (itype == TBL_TYPE_FITS) {
		# Don't append anything if EXTNAME is present in input header.
		ifnoerr {
		    call tbhgtt (itp, "EXTNAME", Memc[scratch], SZ_FNAME)
		} then {
		    cat_extname = false
		}
	    }
	    if (cat_extname) {
		root_len = fnldir (Memc[in], Memc[scratch], SZ_FNAME)
		call strcat ("[", Memc[out], SZ_FNAME)
		call strcat (Memc[in+root_len], Memc[out], SZ_FNAME)
		call strcat ("]", Memc[out], SZ_FNAME)
	    }
	}

	# Open the output table.
	otp = tbtopn (Memc[out], NEW_COPY, itp)

	# Override NEW_COPY table type if output type should be row.
	if (itype != otype && otype == TBL_TYPE_S_ROW)
	    call tbpset (otp, TBL_WHTYPE, TBL_TYPE_S_ROW)

	# Create the table file.
	call tbtcre (otp)

	# Copy the contents from input to output.

	if (itype != otype ||
	    itype == TBL_TYPE_TEXT || itype == TBL_TYPE_FITS ||
	    Memc[iextn] != EOS) {

	    ncols = tbpsta (itp, TBL_NCOLS)
	    nrows = tbpsta (itp, TBL_NROWS)

	    call salloc (icp, ncols, TY_POINTER)
	    call salloc (ocp, ncols, TY_POINTER)

	    do i = 1, ncols {
		Memi[icp+i-1] = tbcnum (itp, i)
		Memi[ocp+i-1] = tbcnum (otp, i)
	    }

	    call tbhcal (itp, otp)		# copy all header parameters

	    if (otype == TBL_TYPE_FITS)		# fill out the file with INDEF
		call tbtwer (otp, nrows)

	    do row = 1, nrows		# copy all rows
		call tbrcsc (itp, otp, Memi[icp], Memi[ocp], row, row, ncols)

	} else {			# same type, and neither is FITS

	    # Copy the whole file.
	    call seek (TB_FILE(itp), BOF)
	    call seek (TB_FILE(otp), BOF)
	    call fcopyo (TB_FILE(itp), TB_FILE(otp))
	    call flush (TB_FILE(otp))

	    # Update the size information in the otp struct.
	    call tbtrsi (otp)
	
	}

	call tbtclo (otp)
	call tbtclo (itp)

	call sfree (sp)
end
