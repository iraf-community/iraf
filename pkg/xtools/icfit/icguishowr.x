# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gset.h>
include	<gio.h>
include "icfit.h"
include	"names.h"

define	CMDS "|show|vshow|xyshow|errors|"

define	SHOW		1	# Show information
define	VSHOW		2	# Show verbose information
define	XYSHOW		3	# Show points 
define	ERRORS		4	# Show errors

# IC_GUISHOW -- GUI show.
#
# Note there is currently an interface violation to determine if the graphics
# stream is connected to a GUI.

procedure ic_guishowr (ic, cmd, cv, x, y, wts, npts)

pointer	ic				#I ICFIT pointer
char	cmd[ARB]			#I Command
pointer cv                              #I CURFIT pointer for error listing
real   x[npts], y[npts], wts[npts]     #I Data arrays
int     npts                            #I Number of data points

int	ncmd, deact, fd
pointer	sp, str, msg
int	strdic(), nscan(), stropen(), open()
errchk	stropen, open, ic_fshow, ic_fvshowr, ic_fxyshowr, ic_ferrorsr

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Scan the command.
	call sscan (cmd)
	call gargwrd (Memc[str], SZ_LINE)
	ncmd = strdic (Memc[str], Memc[str], SZ_LINE, CMDS)
	call gargwrd (Memc[str], SZ_LINE)

	iferr {
	    # Setup the output.
	    deact = NO
	    msg = NULL

	    if (nscan() == 1) {
		if (GP_UIFNAME(IC_GP(ic)) != EOS) {
		    call malloc (msg, 100000, TY_CHAR)
		    fd = stropen (Memc[msg], 100000, WRITE_ONLY)
		} else {
		    fd = open ("STDOUT", APPEND, TEXT_FILE)
		    call gdeactivate (IC_GP(ic), AW_CLEAR)
		    deact = YES
		}
	    } else
		fd = open (Memc[str], APPEND, TEXT_FILE)

	    # Write the results to the output.
	    switch (ncmd) {
	    case SHOW:
		call ic_fshow (ic, fd)
	    case VSHOW:
		call ic_fvshowr (ic, cv, x, y, wts, npts, fd)
	    case XYSHOW:
		call ic_fxyshowr (ic, cv, x, y, wts, npts, fd)
	    case ERRORS:
		call ic_fshow (ic, fd)
		call ic_ferrorsr (ic, cv, x, y, wts, npts, fd)
	    }

	    # Flush the output.
	    call close (fd)
	    if (msg != NULL)
		call gmsg (IC_GP(ic), "icshow", Memc[msg])
	} then
	    call erract (EA_WARN)

	if (msg != NULL)
	    call mfree (msg, TY_CHAR)
	if (deact == YES)
	    call greactivate (IC_GP(ic), AW_PAUSE)
	call sfree (sp)
end
