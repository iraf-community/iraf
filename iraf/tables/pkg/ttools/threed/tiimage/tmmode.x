include <tbset.h>
include "tiimage.h"

#  TM_MODE  --  Detect mode of operation.
#
#  There are five possible modes:
#  1 - Output table exists and one column was selected.
#  2 - Output table exists and no valid column was selected.
#  3 - Output table does not exist but template exists and one column was 
#      selected.
#  4 - Output table does not exist but template exists and no valid column 
#      was selected.
#  5 - New table has to be created from scratch.
#
#
#
#  Revision history:
#  ----------------
#  30-Jan-97  -  Task created (I.Busko)
#   8-Apr-02  -  Remove the call to whatfile (P. Hodge)


int procedure tm_mode (output, template, root, rs, cs, cn, cu, cf)

char	output[SZ_PATHNAME]
char	template[SZ_PATHNAME]
char	root[SZ_FNAME]
char	rs[SZ_FNAME]
char	cs[SZ_FNAME]
char	cn[SZ_COLNAME]
char	cu[SZ_COLUNITS]
char	cf[SZ_COLFMT]
#-
int	mode

int	access(), tm_m1()

begin
	# Process output name. Notice that routine access() must be 
	# supplied with only the root name in order to succeed.
	call rdselect (output, root, rs, cs, SZ_PATHNAME)
	if (access (root, READ_WRITE, 0) == YES) {
	    mode = tm_m1 (OUTPUT_TYPE, root,rs,cs,cn,cu,cf)
	    if (mode == MODE_ERROR)
	        call error (1, "Cannot use output file.")

	# If no valid output, try with template name.
	} else {
	    call rdselect (template, root, rs, cs, SZ_PATHNAME)
	    if (access (root, READ_ONLY, 0) == YES) {
	        mode = tm_m1 (TEMPLATE_TYPE, root, rs, cs, cn, cu, cf)
	        if (mode == MODE_ERROR)
	            call error (1, "Cannot use template file.")
	    } else {
	        mode = MODE_SCRATCH
	    }
	}

	return (mode)
end


#  TM_M1  --  Verify status of file and column selector.

int procedure tm_m1 (type, root, rs, cs, cn, cu, cf)

int	type
char	root[SZ_FNAME]
char	rs[SZ_FNAME]
char	cs[SZ_FNAME]
char	cn[SZ_COLNAME]
char	cu[SZ_COLUNITS]
char	cf[SZ_COLFMT]
#-
pointer	tp, cp
int	numcol, ncp

pointer	tbtopn()
int	tbpsta()

begin
	# Open table
	tp = tbtopn (root, READ_ONLY, 0)

	# Get its total number of columns.
	numcol = tbpsta (tp, TBL_NCOLS)

	# Create array of column pointers from column selector. 
	# This is just to get the actual number of selected columns.
	call malloc (cp, numcol, TY_INT)
	call tcs_open (tp, cs, Memi[cp], ncp, numcol)
	call tbtclo (tp)
	call mfree (cp, TY_INT)

	# Decide mode.
	if (type == OUTPUT_TYPE) {
	    if (ncp == 1)
	        return (MODE_OUT_SINGLE)
	    else
	        return (MODE_OUT_ALL)
	} else if (type == TEMPLATE_TYPE) {
	    if (ncp == 1)
	        return (MODE_TEM_SINGLE)
	    else
	        return (MODE_TEM_ALL)
	}
	return (MODE_ERROR)
end
