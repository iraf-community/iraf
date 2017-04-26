include	<xwhen.h>
include	<config.h>
include	<imhdr.h>
include	<mach.h>
include	<error.h>
include	<ctype.h>
include	<fio.h>
include	<fset.h>
include <tbset.h>	# TBtables

define	GT_QUIT		0
define	GT_EXIT		1

# GTEDIT -- Interactive STSDAS Table editor.

procedure t_gtedit()

pointer	input			# Name of input table
pointer	device
pointer	xcolumn			# Name of column for X
pointer	ycolumn			# Name of column for Y
pointer	output
pointer	reject
pointer	scrname
bool	inplace

pointer	x, y, null, size, sp, tp, deleted, tpr
pointer	errmsg, bad_column
int	npix
int	window			# note:  this is apparently not used
int	phu_copied		# set by tbfpri and ignored
int	tgrjmp[LEN_JUMPBUF], epa, old_onint, status
bool	do_delete, do_quit

bool	clgetb()
int	fstati(), scan(), strncmp(), tbpsta()
pointer	tbtopn()
extern	tgr_onint2()
data	window /0/
common	/tgrcom/ tgrjmp

begin
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize curve pointers to NULL, in case ggplot aborts without
	# allocating any buffers.
	x = NULL
	y = NULL
	size = NULL
	npix = NULL

	# Get some Memory
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (xcolumn, SZ_FNAME, TY_CHAR)
	call salloc (ycolumn, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (reject, SZ_FNAME, TY_CHAR)
	call salloc (scrname, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)
	call salloc (bad_column, SZ_COLNAME, TY_CHAR)
	call aclrc (Memc[reject], SZ_FNAME)
	call aclrc (Memc[output], SZ_FNAME)

	call clgstr ("input", Memc[input], SZ_FNAME)

	# Fetch plotting parameters.
	call clgstr ("device", Memc[device], SZ_FNAME)

	# Get column names etc.
	call clgstr ("xcolumn", Memc[xcolumn], SZ_FNAME)
	call clgstr ("ycolumn", Memc[ycolumn], SZ_FNAME)
	inplace = clgetb ("inplace")		# modified by PEH 13-Jul-92

	# Do we need to get the output file name
	if (!inplace) {
	    call clgstr ("output", Memc[output], SZ_FNAME)
	    if (strncmp (Memc[output], "", 1) == 0) {
		call clpstr ("gtedit.output.p_mode", "q")
	        call clgstr ("output", Memc[output], SZ_FNAME)
		call clpstr ("gtedit.output.p_mode", "h")
	    }
	}
	call clgstr ("reject", Memc[reject], SZ_FNAME)

	if (inplace) {
	    # Copy the name of the table to scrname and open it by that name
	    call strcpy (Memc[input], Memc[scrname], SZ_FNAME)
	    tp = tbtopn (Memc[scrname], READ_WRITE, 0)
	} else {
	    # Copy the table to the output and work on the output.
	    # The call to fcopy was replaced by tbtcpy by PEH on 8-Nov-1993.
	    # The call to tbfpri was added by PEH on 8-Apr-1999.
	    call tbfpri (Memc[input], Memc[output], phu_copied)
	    call tbtcpy (Memc[input], Memc[output])
	    tp = tbtopn (Memc[output], READ_WRITE, 0)
	}

	# Number of rows
	npix = tbpsta (tp, TBL_NROWS)
	iferr {
	    call malloc (x, npix, TY_REAL)
	    call malloc (y, npix, TY_REAL)
	    call malloc (size, npix, TY_REAL)
	    call malloc (null, npix, TY_REAL)
	} then
	    call erract (EA_FATAL)


	# Open reject table if required
	tpr = NULL
	if (Memc[reject] != EOS) {
	    tpr = tbtopn (Memc[reject], NEW_COPY, tp)
	    call tbtcre (tpr)
	    call tbhcal (tp, tpr)
	}

	# Install interrupt exception handler.
	call zlocpr (tgr_onint2, epa)
	call xwhen (X_INT, epa, old_onint)

	call zsvjmp (tgrjmp, status)
	if (status == OK) {
	    # Fetch remaining params and draw the plot.
	    call gt_rdxycol (tp, Memc[xcolumn], Memc[ycolumn], x, y, size, 
		null, npix, Memc[bad_column])

	    # Exit if no column
	    if (npix < 0) {
		call sprintf (Memc[errmsg], SZ_LINE, "Cannot find column %s")
		    call pargstr (Memc[bad_column])
		call error (0, Memc[errmsg])
	    }
	    # Now allocate space for the deleted array
	    call salloc (deleted, npix, TY_INT)
	    call aclri (Memi[deleted], npix)

	    call gteplot (Memc[device], Memc[input], tp, tpr, deleted, 
		   Memc[xcolumn], Memc[ycolumn], x, y, size, null, npix, 
		   Memc[input], status)
	}

	if (status == GT_EXIT) {

	    # Actually delete the rows and save rejects (if requested)
	    call printf ("Please confirm update of output table [y/n]: ")
	    call flush (STDOUT)
	    if (scan() == EOF)
	        call gt_dodel (tp, tpr, Memi[deleted], npix)
	    else {
		call gargb (do_delete)
		if (do_delete)
	            call gt_dodel (tp, tpr, Memi[deleted], npix)
	    }
	} else if (status == GT_QUIT) {

	    call printf (
		  "Please confirm quit with NO update of output table [y/n]: ")
	    call flush (STDOUT)
 	    do_quit = false		# bug fix from Doug Tody, 22-Jan-1993
 	    if (scan() != EOF)
 		call gargb (do_quit)
	    if (!do_quit)
	        call gt_dodel (tp, tpr, Memi[deleted], npix)
	} else if (status == ERR)
	    call fseti (STDOUT, F_CANCEL, OK)

	# Close table 
	call tbtclo (tp)
	if (tpr != NULL)
	    call tbtclo (tpr)

	# Return buffer space whether or not an error occurs while plotting.

	call mfree (x, TY_REAL)
	call mfree (y, TY_REAL)
	call mfree (size, TY_REAL)
	call mfree (null, TY_REAL)

	call sfree (sp)

end
