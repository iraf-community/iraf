# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<fset.h>
include <gset.h>
include <gio.h>

# GACTIVATE -- Perform the initial activation of the workstation, i.e.,
# connect to the graphics kernel and issue the GKI_OPENWS instruction to
# the kernel to physically open the workstation.

procedure gactivate (gp, flags)

pointer	gp			# graphics descriptor
int	flags			# AW_ bit flags; zero if no flags

pointer	w
int	junk, fd
extern	zardbf()
int	fstati(), grdwcs(), and(), locpr()
errchk	gki_openws, gki_getwcs, gki_reactivatews

begin
	# If WS has already been opened, just make sure it is activated.
	if (and (GP_GFLAGS(gp), GF_WSOPEN) != 0) {
	    if (and (GP_GFLAGS(gp), GF_WSACTIVE) == 0) {
		call gki_reactivatews (GP_FD(gp), flags)
		GP_GFLAGS(gp) = GP_GFLAGS(gp) + GF_WSACTIVE
	    }
	    return
	}

	fd = GP_FD(gp)

	# Physically open and activate the workstation.  NOTE - the flags
	# argument is currently ignored; this should be fixed at some point.

	call gki_openws (fd, GP_DEVNAME(gp), GP_ACMODE(gp))

	# If the device is being opened in APPEND mode retrieve the WCS
	# from either the GIO code in the CL process (if talking to a
	# process the FIO driver will not be the standard binary file
	# driver) or from an auxiliary file if the device output is being
	# spooled in a metafile.

	if (GP_ACMODE(gp) == APPEND) {
	    w = GP_WCSPTR(gp,1)
	    if (fstati (fd, F_DEVICE) != locpr (zardbf))
		call gki_getwcs (fd, Memi[w], LEN_WCSARRAY)
	    else iferr (junk = grdwcs(GP_DEVNAME(gp), Memi[w], LEN_WCSARRAY))
		;
	}

	GP_GFLAGS(gp) = GP_GFLAGS(gp) + (GF_WSOPEN+GF_WSACTIVE)
end
