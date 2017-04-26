# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<xwhen.h>
include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GIOTR -- A graphics filter, called by PR_PSIO (during normal graphics output)
# and RCURSOR (when in cursor mode) to perform the workstation transformation
# on a block of metacode instructions, writing the individual instructions to
# either the inline stdgraph kernel or to an external kernel.  Input is taken
# from the frame buffer for the stream.  All full instructions starting at the
# input pointer IP and ending at the output pointer OP are processed, leaving
# the input pointer positioned to BOI of the last (and incomplete) instruction
# in the frame buffer.  If output is to an inline kernel the kernel is called
# to execute each instruction as it is extracted.  If output is to an external
# kernel instructions are written to the named stream, i.e., into the FIO
# buffer associated with the stream, and later transferred to the kernel in the
# external process when the process requests input from the named stream in an
# XFER directive (see PR_PSIO).

procedure giotr (stream)

int	stream			# graphics stream

pointer	tr, gki
int	jmpbuf[LEN_JUMPBUF], fn
int	mode, xint, status, junk, nwords
common	/gtrvex/ jmpbuf

pointer	gtr_init(), coerce()
extern	giotr_onint(), gtr_delete()
int	gtr_fetch_next_instruction(), locpr()
errchk	gtr_init, gtr_fetch_next_instruction, gki_write
data	status /OK/, xint /NULL/
include	"gtr.com"

begin
	tr = gtr_init (stream)

	# If an interrupt occurs while GIOTR is executing output is cancelled
	# and further processing is disabled until the next frame begins.

	if (xint == NULL)
	    call xwhen (X_INT, locpr(giotr_onint), xint)

	call zsvjmp (jmpbuf, status)
	if (status != OK) {
	    call gki_cancel (stream)
	    call gki_deactivatews (stream, 0)
	}

	# Fetch, optionally transform, and execute each metacode instruction
	# in the frame buffer.

	while (gtr_fetch_next_instruction (tr, gki) != EOF) {
	    switch (Mems[gki+GKI_HDR_OPCODE-1]) {

	    case GKI_OPENWS:
		mode = Mems[gki+GKI_OPENWS_M-1]
		if (mode != APPEND)
		    status = OK

		if (status == OK) {
		    # If the open instruction has already been passed to the
		    # kernel by gtr_control, do not do so again here.

		    if (TR_SKIPOPEN(tr) == YES)
			TR_SKIPOPEN(tr) = NO
		    else
			call gki_write (stream, Mems[gki])

		    # gtr_control does not call gki_escape so always do this.
		    call gki_escape (stream, GKI_OPENWS, 0, 0)

		    # Discard frame buffer contents up to and including the
		    # openws instruction, so that it will only be executed
		    # once.

		    if (Mems[gki+GKI_OPENWS_M-1] == NEW_FILE)
			call gtr_frame (tr, TR_IP(tr), stream)
		}

	    case GKI_CLOSEWS, GKI_DEACTIVATEWS, GKI_REACTIVATEWS:
		# These instructions are passed directly to the kernel via
		# the PSIOCTRL stream at runtime, but are ignored in metacode
		# to avoid unnecessary mode switching of the terminal.
		;

	    case GKI_CANCEL:
		# Cancel any buffered graphics data.
		call gki_write (stream, Mems[gki])
		call gtr_frame (tr, TR_IP(tr), stream)

	    case GKI_FLUSH, GKI_GETCURSOR, GKI_GETCELLARRAY:
		# Do not buffer these instructions.
		call gki_write (stream, Mems[gki])
		call gtr_delete (tr, gki)

	    case GKI_CLEAR:
		# Clear is special because it initializes things.
		if (status != OK) {
		    call gki_reactivatews (stream, 0)
		    status = OK
		}
		# Execute the instruction.
		call gki_write (stream, Mems[gki])
		call gki_escape (stream, GKI_CLEAR, 0, 0)

		# Discard frame buffer contents up to and including the clear.
		call gtr_frame (tr, TR_IP(tr), stream)

	    case GKI_SETWCS:
		call gki_write (stream, Mems[gki])
		nwords = Mems[gki+GKI_SETWCS_N-1]
		call amovs (Mems[gki+GKI_SETWCS_WCS-1],
		    Mems[coerce (TR_WCSPTR(tr,1), TY_STRUCT, TY_SHORT)],
		    min (nwords, LEN_WCS * MAX_WCS * SZ_STRUCT / SZ_SHORT))

	    case GKI_ESCAPE:
		if (status == OK) {
		    fn = Mems[gki+GKI_ESCAPE_FN-1]

		    # Execute the escape instruction.
		    if (wstranset == YES) {
			call sge_wstran (fn, Mems[gki+GKI_ESCAPE_DC-1],
			    vx1,vy1, vx2,vy2)
		    } else
			call gki_write (stream, Mems[gki])

		    # Allow the kernel escape handling code to preserve,
		    # delete, or edit the instruction.

		    call sge_spoolesc (tr, gki, fn, Mems[gki+GKI_ESCAPE_DC-1],
			TR_FRAMEBUF(tr), TR_OP(tr), locpr(gtr_delete))
		}

	    default:
		if (status == OK)
		    if (wstranset == YES) {
			# Perform the workstation transformation and output the
			# transformed instruction, if there is anything left.
			call gtr_wstran (Mems[gki])
		    } else
			call gki_write (stream, Mems[gki])
	    }
	}

	# Clear the frame buffer if spooling is disabled.  This is done by
	# moving the upper part of the buffer to the beginning of the buffer,
	# starting with the word pointed to by the second argument, preserving
	# the partial instruction likely to be found at the end of the buffer.
	# Truncate the buffer if it grows too large by the same technique of
	# shifting data backwards, but in this case without destroying all
	# of the data.

	if (TR_SPOOLDATA(tr) == NO)
	    call gtr_frame (tr, TR_IP(tr), stream)
	else if (TR_OP(tr) - TR_FRAMEBUF(tr) > TR_MAXLENFRAMEBUF(tr))
	    call gtr_truncate (tr, TR_IP(tr))

	# Pop the interrupt handler.
	if (xint != NULL) {
	    call xwhen (X_INT, xint, junk)
	    xint = NULL
	}
end


# GIOTR_ONINT -- Interrupt handler for GIOTR.

procedure giotr_onint (vex, next_handler)

int	vex			# virtual exception
int	next_handler		# next exception handler in chain
int	jmpbuf[LEN_JUMPBUF]
common	/gtrvex/ jmpbuf

begin
	call xer_reset()
	call zdojmp (jmpbuf, vex)
end
