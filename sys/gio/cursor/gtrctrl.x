# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<prstat.h>
include	<config.h>
include	<fset.h>
include	<gset.h>
include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_CONTROL -- Execute a graphics control instruction, e.g., connect a
# graphics kernel to a graphics stream and set or get the WCS for a frame.
# The control instructions are GKI encoded instructions transmitted to the
# pseudofile GIOCONTROL.  The PR_PSIO procedure (which processes the pseudofile
# directives from a subprocess) calls us whenever data is sent to this
# special pseudofile.

procedure gtr_control (stream, gki, source_pid)

int	stream			# graphics stream
short	gki[ARB]		# encoded graphics control instruction
int	source_pid		# pid of requesting process

bool	redirected
pointer	tr, sp, devname, gki_out
int	flags, mode, nwords, fd, p_fd
int	prstati(), pr_getredir()
pointer	gtr_init(), coerce()
include	"gtr.com"

begin
	call smark (sp)
	call salloc (devname, SZ_TRDEVNAME, TY_CHAR)

	nwords = gki[GKI_HDR_LENGTH]
	call salloc (gki_out, nwords, TY_SHORT)
	call amovs (gki, Mems[gki_out], nwords)

	tr = gtr_init (stream)
	p_fd = abs (pr_getredir (source_pid, stream))
	redirected = (p_fd >= FIRST_FD && p_fd <= LAST_FD)

	switch (gki[GKI_HDR_OPCODE]) {
	case GKI_OPENWS:
	    mode   = gki[GKI_OPENWS_M]
	    nwords = gki[GKI_OPENWS_N]

	    # Unpack the device name, passed as a short integer array.
	    call achtsc (gki[GKI_OPENWS_D], Memc[devname], nwords)
	    Memc[devname+nwords] = EOS

	    # Connect the kernel.
	    call fseti (stream, F_CANCEL, OK)
	    call gtr_openws (Memc[devname], mode, stream, source_pid)

	    # Count the logical openws.
	    TR_WSOPEN(tr) = TR_WSOPEN(tr) + 1
	    TR_SKIPOPEN(tr) = YES
	    TR_WSACTIVE(tr) = YES
	    TR_WSACTSAVE(tr) = NO

	case GKI_CLOSEWS:
	    # Count the logical closews.
	    TR_WSOPEN(tr) = TR_WSOPEN(tr) - 1
	    TR_WSACTIVE(tr) = NO

	case GKI_DEACTIVATEWS:
	    TR_WSACTIVE(tr) = NO
	    if (TR_INTERACTIVE(tr) == YES && TR_PAGE(tr) == NO) {
		flags = gki[GKI_REACTIVATEWS_F]
		if (and (flags, AW_CLEAR) != 0)
		    Mems[gki_out+GKI_REACTIVATEWS_F-1] = flags - AW_CLEAR
	    }

	case GKI_REACTIVATEWS:
	    TR_WSACTIVE(tr) = YES
	    if (TR_INTERACTIVE(tr) == YES) {
		flags = gki[GKI_REACTIVATEWS_F]
		if (and (flags, AW_PAUSE) != 0)
		    call gtr_waitpage (STDERR, stream)
	    }

	case GKI_SETWCS:
	    nwords = gki[GKI_SETWCS_N]
	    call amovs (gki[GKI_SETWCS_WCS],
		Mems[coerce (TR_WCSPTR(tr,1), TY_STRUCT, TY_SHORT)],
		min (nwords, LEN_WCS * MAX_WCS * SZ_STRUCT / SZ_SHORT))

	case GKI_GETWCS:
	    nwords = gki[GKI_GETWCS_N]
	    fd = prstati (source_pid, PR_OUTFD)

	    call write (fd, Memi[TR_WCSPTR(tr,1)], nwords * SZ_SHORT)
	    call flush (fd)
	}

	# Pass the (possibly modified) instruction on to the kernel.
	# We must NOT call gki_flush or gki_fflush here, as this would
	# result in a reentrant call to prpsio when writing to a subkernel.

	if (!redirected)
	    call gki_write (stream, Mems[gki_out])

	call sfree (sp)
end
