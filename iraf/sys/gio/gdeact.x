# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gset.h>

# GDEACTIVATE -- Deactivate the workstation, i.e., for an interactive device
# (graphics terminal) restore the terminal to text mode.  This is similar to
# closing the workstation will gclose, except that the graphics state is
# retained and graphics i/o may be resumed following a subsequent call to
# greactivate.  These calls are generally no-ops for noninteractive devices.

procedure gdeactivate (gp, flags)

pointer	gp			# graphics descriptor
int	flags			# action flags

int	and()
errchk	gflush
errchk	gki_deactivatews

begin
	if (and (GP_GFLAGS(gp), GF_WSOPEN) != 0) {
	    call gflush (gp)
	    call gki_deactivatews (GP_FD(gp), flags)
	    if (and (GP_GFLAGS(gp), GF_WSACTIVE) != 0)
		GP_GFLAGS(gp) = GP_GFLAGS(gp) - GF_WSACTIVE
	}
end
