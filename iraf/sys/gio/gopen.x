# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<knet.h>
include	<gset.h>
include	<gki.h>
include	<gio.h>

# GOPENUI -- Open a graphics stream for output to the named device on file FD.
# If a logical device name is given the actual device name is fetched from the
# environment.   If a UI file is specified the named user interface definition
# file is downloaded to the graphics server.  The device parameters are then
# retrieved from the graphcap entry for the device.  GIO is initialized, and
# if the device is being opened in APPEND mode, the WCS set when the device
# was last read are retrieved from the CL (if output is to a standard stream)
# or from the WCS savefile for the device.

pointer procedure gopenui (device, mode, uifname, fd)

char	device[ARB]		#I logical or physical device name
int	mode			#I access mode: NEW_FILE or APPEND
char	uifname[ARB]		#I user interface specification file
int	fd			#I metacode output file

pointer	gp, tty
int	outfd, stream_type, junk
bool	close_at_end, kf_ok, vdm_device, std_stream
pointer	sp, devname, envname, kfname

bool	streq()
extern	gflush()
pointer	ttygdes()
int	envgets(), envfind(), open(), locpr(), access(), ttygets()
errchk	syserr, syserrs, ttygdes
errchk	greset, gki_openws, calloc

string	stdgraph "stdgraph"
string	stdimage "stdimage"
string	stdplot  "stdplot"
string	vdm 	 "vdm"
string	stdvdm 	 "stdvdm"

begin
	call smark (sp)
	call salloc (devname, SZ_FNAME, TY_CHAR)
	call salloc (envname, SZ_FNAME, TY_CHAR)
	call salloc (kfname,  SZ_FNAME, TY_CHAR)

	call flush (STDOUT)

	# If one of the logical devices STDGRAPH, STDIMAGE, or STDPLOT is
	# named look up the actual device name in the environment.  The
	# standard metafile "device", STDVDM, is implemented as an actual
	# device with an actual graphcap entry, so we do not have to map
	# its name.

	if (streq (device, stdgraph) || streq (device, stdimage) ||
	    streq (device, stdplot)) {
	    if (envgets (device, Memc[devname], SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, device)
	} else
	    call strcpy (device, Memc[devname], SZ_FNAME)

	# The special name "none" indicates that graphics is not supported
	# on this stream for the local site or workstation (e.g., when using
	# a nongraphics terminal).

	if (streq (Memc[devname], "none"))
	    switch (fd) {
	    case STDGRAPH:
		call syserr (SYS_GGNONE)
	    case STDIMAGE:
		call syserr (SYS_GINONE)
	    case STDPLOT:
		call syserr (SYS_GPNONE)
	    default:
		call syserr (SYS_GPNONE)
	    }

	# Fetch the graphcap entry for the device.
	tty = ttygdes (Memc[devname])

	# If the output device is "stdvdm" or "vdm" and the FD supplied by the
	# user is that of a standard stream, open the standard metafile and
	# append output directly to that.  The metafile is always opened in
	# APPEND mode regardless of the mode in which the graphics device is
	# opened.

	outfd = fd
	close_at_end = false
	call gki_redir (fd, -1, junk, stream_type)
	std_stream = (fd == STDGRAPH || fd == STDIMAGE || fd == STDPLOT)
	vdm_device = (streq(device,stdvdm) || streq(device,vdm))

	if (vdm_device && std_stream) {
	    # Get filename of virtual device metafile.
	    call strcpy (stdvdm, Memc[devname], SZ_DEVNAME)
	    if (envfind (stdvdm, Memc[envname], SZ_FNAME) <= 0)
		call strcpy ("uparm$vdm", Memc[envname], SZ_FNAME)

	    # Open VDM for appending.
	    iferr (outfd = open (Memc[envname], APPEND, BINARY_FILE)) {
		call ttycdes (tty)
		call erract (EA_ERROR)
	    }
	    close_at_end = true

	} else if (std_stream && stream_type != TY_INLINE) {
	    # Verify that there is a GIO kernel specified for the device before
	    # trying to open it via PSIOCTRL, since the latter does not return
	    # an error status if it fails to connect a kernel, causing the error
	    # to go undetected until the CL fails to connect a kernel, which
	    # causes an error which cannot be caught in an IFERR in the current
	    # process.  Catching the error here is faster and works with IFERR.
	    # No checking for a kernel is performed if the metacode output is
	    # being directed to a user opened stream.

	    kf_ok = false
	    if (ttygets (tty, "kf", Memc[kfname], SZ_FNAME) > 0)
		if (streq (Memc[kfname], "cl"))
		    kf_ok = true
		else if (access (Memc[kfname], 0,0) == YES)
		    kf_ok = true

	    if (!kf_ok) {
		call ttycdes (tty)
		call syserrs (SYS_GNOKF, Memc[devname])
	    }
	}

	# Allocate and initialize the GIO graphics descriptor.  Initialize
	# GKI (the graphics kernel interface) on the stream, if the stream
	# has not already been directed to a kernel.

	call calloc (gp, LEN_GDES, TY_STRUCT)

	GP_FD(gp) = outfd
	GP_TTY(gp) = tty
	if (close_at_end)
	    GP_GFLAGS(gp) = GF_CLOSEFD

	# Set the access mode; default to NEW_FILE if not specified.
	GP_ACMODE(gp) = mod (mode, AW_DEFER)
	if (GP_ACMODE(gp) == 0)
	    GP_ACMODE(gp) = NEW_FILE

	call greset (gp, GR_RESETALL)
	call gki_init (outfd)
	call strcpy (Memc[devname], GP_DEVNAME(gp), SZ_DEVNAME)
	call strcpy (uifname, GP_UIFNAME(gp), SZ_UIFNAME)

	# Set up info for GEXFLS, called by CLGCUR to flush the graphics
	# output prior to a cursor read.

	call gexfls_set (outfd, gp, locpr(gflush))

	# Activate (physically open) the workstation, unless the defer flag
	# is set, eg., mode = NEW_FILE+AW_DEFER.

	if (mode < AW_DEFER)
	    iferr (call gactivate (gp, 0)) {
		call ttycdes (tty)
		call gexfls_clear (outfd)
		call mfree (gp, TY_STRUCT)
		call erract (EA_ERROR)
	    }

	call sfree (sp)
	return (gp)
end


# GOPEN -- Open a graphics stream for output to the named device on file FD.
# Identical to GOPENUI except that the default UI is used.

pointer procedure gopen (device, mode, fd)

char	device[ARB]		#I logical or physical device name
int	mode			#I access mode: NEW_FILE or APPEND
int	fd			#I metacode output file

pointer	gopenui()

begin
	return (gopenui (device, mode, "", fd))
end
