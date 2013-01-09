include <ctype.h>		# for IS_WHITE
include <tbset.h>
include "tbtables.h"

# tbfcal -- copy all header parameters for FITS table
# All header parameters are copied from the input to the output table,
# both of which must be open.  This version should be used when either
# the input or output table is in a FITS file.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 13-Nov-1995  Change type of tbfres from bool to int.
# Phil Hodge, 14-Aug-1997  Don't clobber EXTVER if it's already present.

procedure tbfcal (itp, otp)

pointer itp		# i: pointer to descriptor of input table
pointer otp		# i: pointer to descriptor of output table
#--
pointer sp
pointer value		# buffer for header record for parameter
pointer comment		# scratch for comment string
pointer extname		# buffer for copying extname
char	keyword[SZ_KEYWORD]	# parameter name
int	dtype		# data type of parameter
int	i
int	ip

# buffers for copying the value
double	dval
real	rval
int	ival
bool	bval

bool	streq()
int	tbhgti()
int	tbfres()
errchk	tbferr, tbhgnp, tbhgti, tbhgtt

begin
	call smark (sp)
	call salloc (value, SZ_LINE, TY_CHAR)
	call salloc (comment, SZ_FNAME, TY_CHAR)
	call salloc (extname, SZ_LINE, TY_CHAR)

	# Copy each parameter except for the reserved keywords,
	# such as XTENSION, TTYPEn.
	do i = 1, TB_NPAR(itp) {

	    # Get Nth keyword and value from the input table.
	    call tbhgnp (itp, i, keyword, dtype, Memc[value])

	    if (tbfres (keyword) == YES)	# ignore reserved keywords
		next

	    # Don't clobber EXTNAME or EXTVER if they're already present in
	    # the output.
	    if (streq (keyword, "EXTNAME")) {
		ifnoerr (call tbhgtt (otp, "EXTNAME", Memc[extname], SZ_LINE))
		    next
	    }
	    if (streq (keyword, "EXTVER")) {
		ifnoerr (ip = tbhgti (otp, "EXTVER"))	# use ip as scratch
		    next
	    }

	    # Read the value into an appropriate buffer, and add it
	    # to the output table header.
	    switch (dtype) {
	    case TY_REAL:
		dval = INDEFD
		call sscan (Memc[value])
		    call gargd (dval)
		rval = dval
		call tbhadr (otp, keyword, rval)
	    case TY_DOUBLE:
		dval = INDEFD
		call sscan (Memc[value])
		    call gargd (dval)
		call tbhadd (otp, keyword, dval)
	    case TY_INT:
		ival = INDEFI
		call sscan (Memc[value])
		    call gargi (ival)
		call tbhadi (otp, keyword, ival)
	    case TY_CHAR:
		call tbhadt (otp, keyword, Memc[value])
	    case TY_BOOL:
		ip = 0
		while (IS_WHITE(Memc[value+ip]))
		    ip = ip + 1
		if (Memc[value+ip] == 'T' || Memc[value+ip] == 't') {
		    bval = true
		} else if (Memc[value+ip] == 'F' || Memc[value+ip] == 'f') {
		    bval = false
		} else {
		    # Read 1 or 0 for true or false respectively.
		    ival = NO
		    call sscan (Memc[value+ip])
			call gargi (ival)
		    bval = (ival != NO)
		}
		call tbhadb (otp, keyword, bval)
	    default:
		call error (1, "tbhcal:  bad data type")
	    }

	    # Copy the comment from input to output.
	    call tbhgcm (itp, keyword, Memc[comment], SZ_FNAME)
	    call tbhpcm (otp, keyword, Memc[comment])
	}

	call sfree (sp)
end
