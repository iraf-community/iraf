# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<gio.h>
include	<gki.h>
include	"gtr.h"
include	"grc.h"

# GRC_OPEN -- Open the workstation.  Most commonly used to reopen the
# workstation for a cursor read after plotting.

pointer procedure grc_open (device, mode, stream, rc)

char	device[ARB]		# device name (optional)
int	mode			# desired access mode
int	stream			# graphics stream
pointer	rc			# rcursor descriptor

pointer	sp, devname, envvar, tr
int	envgets()
bool	streq()
pointer	gtr_init()

include	"gtr.com"
string	stdgraph "stdgraph"
string	stdimage "stdimage"
string	stdplot  "stdplot"
errchk	syserrs, gtr_openws, gki_openws, gtr_init

begin
	call smark (sp)
	call salloc (envvar,  SZ_FNAME, TY_CHAR)
	call salloc (devname, SZ_FNAME, TY_CHAR)

	tr = gtr_init (stream)

	# If the workstation is already connected and the kernel is open
	# issue the openws directive if it has not already been issued.

	if (TR_DEVNAME(tr) != EOS)
	    if (device[1] == EOS || streq (device, TR_DEVNAME(tr))) {
		# Kernel is already physically open on this stream.  Activate
		# it if necessary; record whether or not is was active when
		# we were called, so that we can restore the original state
		# when grc_close() is called.

		if (TR_WSOPEN(tr) <= 0) {
		    call gki_openws (stream, TR_DEVNAME(tr), mode)
		    TR_WSACTIVE(tr) = YES
		    TR_WSACTSAVE(tr) = NO
		} else {
		    TR_WSACTSAVE(tr) = TR_WSACTIVE(tr)
		    call gki_reactivatews (stream, 0)
		    TR_WSACTIVE(tr) = YES
		}

		call gki_fflush (stream)

		TR_WSOPEN(tr) = TR_WSOPEN(tr) + 1
		call sfree (sp)
		return (tr)
	    }

	# If no device name given fetch the device name from the environment.

	if (device[1] == EOS) {
	    switch (stream) {
	    case STDGRAPH:
		call strcpy (stdgraph, Memc[envvar], SZ_FNAME)
	    case STDIMAGE:
		call strcpy (stdimage, Memc[envvar], SZ_FNAME)
	    default:
		call strcpy (stdplot,  Memc[envvar], SZ_FNAME)
	    }

	    # Convert environment variable name into device name.  Indirection
	    # and assumption of the value of "terminal" are allowed.

	    repeat {
		if (envgets (Memc[envvar], Memc[devname], SZ_FNAME) <= 0)
		    call syserrs (SYS_ENVNF, Memc[envvar])
		if (Memc[devname] == '@') {
		    # Indirection in environment variable name.
		    call strcpy (Memc[devname+1], Memc[envvar], SZ_FNAME)
		} else if (streq (Memc[devname], "terminal")) {
		    call strcpy (Memc[devname], Memc[envvar], SZ_FNAME)
		} else
		    break
	    }
	} else
	    call strcpy (device, Memc[devname], SZ_FNAME)

	# Open the workstation (kernel) on stream FD.
	call gtr_openws (Memc[devname], mode, stream, NULL)

	TR_WSOPEN(tr) = TR_WSOPEN(tr) + 1
	TR_WSACTSAVE(tr) = NO
	TR_WSACTIVE(tr) = YES

	call gki_openws (stream, Memc[devname], mode)
	call gki_fflush (stream)

	call sfree (sp)
	return (tr)
end
