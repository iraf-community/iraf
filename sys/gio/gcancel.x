# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GCANCEL -- Cancel any buffered graphics output (as far as possible).  Should
# be called by interrupt handlers to avoid leaving GIO in a funny state
# following an interrupt.
#
# As far as possible, GKI instructions are built up in internal storage and
# written to the output file in a single write.  This decreases the likliehood
# of leaving a botched instruction in the output stream in response to an
# interrupt.  Do not call FSETI to cancel the file output because that will
# almost certainly guarantee a botched instruction.  Instead, we discard any
# partially built polylines still in GPL storage and append the GKI_CANCEL to
# the output instruction stream.  The cancel instruction is passed on to the
# graphics kernel which eventually calls FSETI to cancel its output file
# buffer (containing device instructions).  If a metacode reader does detect
# a botched instruction it will scan forward for the next BOI to try to resync
# the instruction stream.

procedure gcancel (gp)

pointer	gp			# graphics descriptor
int	and()

begin
	if (and (GP_GFLAGS(gp), GF_WSOPEN) != 0) {
	    call gki_cancel (GP_FD(gp))
	    call gki_fflush (GP_FD(gp))
	}
	call gfrinit (gp)
end
