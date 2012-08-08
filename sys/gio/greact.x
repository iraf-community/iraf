# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gio.h>

# GREACTIVATE -- Reactivate the workstation, i.e., for an interactive device
# (graphics terminal) restore the terminal to graphics mode, following a call
# to gdeactivate to do some normal terminal mode text i/o.

procedure greactivate (gp, flags)

pointer	gp			# graphics descriptor
int	flags			# action flags

int	and()
errchk	gki_reactivatews, gactivate

begin
	call flush (STDOUT)
	if (and (GP_GFLAGS(gp), GF_WSOPEN) != 0) {
	    # The workstation is already open - just reactivate it.
	    call gki_reactivatews (GP_FD(gp), flags)
	    if (and (GP_GFLAGS(gp), GF_WSACTIVE) == 0)
		GP_GFLAGS(gp) = GP_GFLAGS(gp) + GF_WSACTIVE
	} else {
	    # Open the workstation (implies an automatic reactivatews).
	    call gactivate (gp, flags)
	}

	if (and (flags, AW_CLEAR) != 0)
	    call gfrinit (gp)
end
