include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbhcal -- copy all header parameters
# All header (i.e. user) parameters are copied from the input to the
# output table, both of which must be open.  This would normally be used
# for a recently created output table which would not have any parameters.
# If there are header parameter(s) in the output table (except for history
# or comments) that have the same keyword names as parameters in the input
# table, the records from the input replace those in the output.
#
# Phil Hodge, 31-Aug-1987  Subroutine created.
# Phil Hodge,  9-Mar-1989  Change dtype from char to int.
# Phil Hodge, 30-Jan-1992  Modify for text tables (copy comment buffer).
# Phil Hodge, 22-Apr-1994  Call tbbcmt to append comment buffer.
# Phil Hodge, 11-May-1994  Simplify text table section, and dereference
#			pointer TB_COMMENT in call to tbbcmt.
# Phil Hodge,  6-Mar-1995  Also copy comment.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true for text table.
# Phil Hodge, 14-Jun-1995  Modify for FITS tables.
# Phil Hodge, 30-Jan-1996  Set TB_MODIFIED to true.
# Phil Hodge,  7-Jun-1999  Copy parameters for a text table.

procedure tbhcal (itp, otp)

pointer itp			# i: pointer to descriptor of input table
pointer otp			# i: pointer to descriptor of output table
#--
pointer sp
pointer par			# buffer for header record for parameter
pointer comment			# scratch for comment string
pointer key			# scratch for keyword names
pointer pnum			# scratch for parameter number
char	keyword[SZ_KEYWORD]	# parameter name
int	dtype			# data type of parameter (ignored)
int	parnum			# parameter number
int	j			# loop index for par number in input table
int	k			# loop index for par number in output table
int	numout			# initial number of par in output table
int	allout			# total number of parameters for output table
int	num_noncomment		# number of non-comment parameters
int	key_offset		# an offset for keyword names in scratch array
int	strncmp()
bool	tbhisc()
errchk	tbtchs, tbbcmt, tbfcal, tbhgnp, tbhpnp, tbhanp, tbhrpr, tbhwpr

begin
	if ( ! TB_IS_OPEN(otp) )
	    call error (1, "tbhcal:  output table is not open yet")
	if (TB_READONLY(otp))
	    call error (ER_TBREADONLY,
			"tbhcal:  can't write to table; it's readonly")

	if (TB_TYPE(itp) == TBL_TYPE_FITS || TB_TYPE(otp) == TBL_TYPE_FITS) {
	    call tbfcal (itp, otp)
	    TB_MODIFIED(otp) = true
	    return
	}

	if (TB_TYPE(otp) == TBL_TYPE_TEXT) {
	    # For a text table we also copy the comment buffer, but only
	    # if there's an input comment buffer.
	    if (TB_COMMENT(itp) != NULL)
		call tbbcmt (otp, Memc[TB_COMMENT(itp)])
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)
	call salloc (comment, SZ_PARREC, TY_CHAR)

	# This will be zero if it's a new table.
	numout = TB_NPAR(otp)

	# Do we need more space for header parameters in the output table?
	allout = numout + TB_NPAR(itp)
	if (allout > TB_MAXPAR(otp))
	    call tbtchs (otp, allout+DEFMAXPAR, -1, -1, -1)

	# Are there already some parameters in the output table?
	if (numout > 0) {

	    call salloc (key, numout*SZ_KEYWORD, TY_CHAR)
	    call salloc (pnum, numout, TY_INT)

	    # Make a list of all non-comment keywords in output table.
	    num_noncomment = 0			# initial values
	    key_offset = key
	    do k = 1, numout {
		# Get Nth parameter from output table (we just need
		# the keyword name).
		call tbhgnp (otp, k, keyword, dtype, Memc[par])
		if ( ! tbhisc (keyword) ) {
		    # Not a comment, so add it to the list.
		    num_noncomment = num_noncomment + 1
		    call strcpy (keyword, Memc[key_offset], SZ_KEYWORD)
		    key_offset = key_offset + SZ_KEYWORD
		    Memi[pnum+num_noncomment-1] = k	# param number
		}
	    }
	    # Copy each input parameter.
	    do j = 1, TB_NPAR(itp) {
		# Get Nth parameter and its comment from the input table.
		call tbhgnp (itp, j, keyword, dtype, Memc[par])
		call tbhgcm (itp, keyword, Memc[comment], SZ_PARREC)
		parnum = 0			# initial values
		key_offset = key
		do k = 1, num_noncomment {
		    if (strncmp (keyword, Memc[key_offset], SZ_KEYWORD) == 0) {
			parnum = Memi[pnum+k-1]		# found it
			break
		    } else {
			key_offset = key_offset + SZ_KEYWORD
		    }
		}
		if (parnum > 0)
		    # It's already present in output table; put Nth parameter.
		    call tbhpnp (otp, parnum, keyword, dtype, Memc[par])
		else
		    # Add new parameter; the output parnum is ignored.
		    call tbhanp (otp, keyword, dtype, Memc[par], parnum)

		# Append the comment.
		call tbhpcm (otp, keyword, Memc[comment])
	    }

	} else {

	    # No parameters in output table yet; copy every parameter.
	    do j = 1, TB_NPAR(itp) {
		call tbhrpr (itp, j, Memc[par])
		call tbhwpr (otp, j, Memc[par])
	    }
	    TB_NPAR(otp) = TB_NPAR(itp)
	}

	TB_MODIFIED(otp) = true

	call sfree (sp)
end
