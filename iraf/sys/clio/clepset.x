# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<clio.h>
include	"clpset.h"

# CLEPSET -- Edit a pset.  What exactly this operation implies depends
# upon the CL.  To the application, it means any external operation which
# can modify the pset.

procedure clepset (pp)

pointer	pp			#I pset descriptor

pointer	sp, lbuf
bool	streq()
int	getlline()
errchk	flush, getlline, clc_scan

begin
	call smark (sp)
	call salloc (lbuf, SZ_COMMAND, TY_CHAR)

	# Edit pset and dump edited version back to CLIN.  It is not
	# necessary to write the pset to the CL before editing as the
	# cache is "write-through" and any clputs will already have
	# updated the CL version of the pset as well as the cache version.

	call flush (STDOUT)
	call fprintf (CLOUT, "eparam %s; dparam %s > %s\n")
	    call pargstr (PS_PSETNAME(pp))
	    call pargstr (PS_PSETNAME(pp))
	    call pargstr (IPCOUT)
	call flush (CLOUT)

	# Parse the new "param = value" statements returned by dparam and
	# update the parameter cache.
	
	while (getlline (CLIN, Memc[lbuf], SZ_COMMAND) != EOF)
	    if (streq (Memc[lbuf], IPCDONEMSG))
		break
	    else
		call clc_scan (Memc[lbuf])

	# Delete the old parameter entries.
	call clc_compress()

	call sfree (sp)
end
