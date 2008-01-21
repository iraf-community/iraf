include	<config.h>
#include	<imhdr.h>
#include	<mach.h>
#include	<error.h>
#include	<ctype.h>
include	<fset.h>	# FIO
#include	<gset.h>	# GIO
#include <tbset.h>	# TBtables
include "sgraph.h"

# sgraph -- STSDAS graphing utility where input may be one or more lists
# (y or x,y), image sections, or table columns.  Multidimensional image
# sections are reduced to a vector by computing the projection about the
# indicated axis.  Many options are available to personalize the plot; see
# the manual page for a full description. 

#  9/20/91 Added some errchks to prevent SegV.  Took out the error
#  handler (!?).  ZGL

#  10/28/91 Tweeked the error handling a little more to fix a problem
#  with nonexistent images.  Also took out a set WCS to 15 when stacking
#  images to fix a bug causing viewport specification to be ignored when
#  stacking curves.  ZGL

#  4/22/92  Tried to straighten out the error bar size question.
#  Everyone should now plot errors so the user-input value is the size of
#  one end of an error bar, from the point coordinate to the end of the
#  bar, i.e., plus or minus sigma.  ZGL

#  5/6/92  Added the color parameter and setting the drawing color
#  (implemented only by psikern at the moment).  This applies to all
#  graphics drawn in a given execution.ZGL

procedure t_sgraph()

char	input[SZ_LINE]
pointer	x[MAX_CURVES], y[MAX_CURVES], size[MAX_CURVES]
int	npix[MAX_CURVES], ncurves

char	device[SZ_FNAME]
int	mode, i, window
#pointer	sgrjmp[LEN_JUMPBUF], epa, old_onint
#int	status

bool	clgetb()
int	fstati()
#extern	sgr_onint()
data	window /0/
#common	/sgrcom/ sgrjmp

begin
	# Initialize curve pointers to NULL, in case ggplot aborts without
	# allocating any buffers.
	do i = 1, MAX_CURVES {
	    x[i] = NULL
	    y[i] = NULL
	    size[i] = NULL
	    npix[i] = NULL
	}

	if (fstati (STDIN, F_REDIR) == YES)
	    call strcpy ("STDIN", input, SZ_FNAME)
	else
	    call clgstr ("input", input, SZ_LINE)

	# Fetch plotting parameters.

	call clgstr ("device", device, SZ_FNAME)
	mode = NEW_FILE
	if (clgetb ("append"))
	    mode = APPEND

	# Install interrupt exception handler.
#	call zlocpr (sgr_onint, epa)
#	call xwhen (X_INT, epa, old_onint)

#	call zsvjmp (sgrjmp, status)
#	if (status == OK) {
#	    # Fetch remaining params and draw the plot.
#	    iferr (call ggplot (device, mode, input, 
#		x, y, size, npix, ncurves))
#		status = ERR
#	}

	call ggplot (device, mode, input, x, y, size, npix, ncurves)

#	if (status == ERR)
#	    call fseti (STDOUT, F_CANCEL, OK)

	# Return buffer space whether or not an error occurs while plotting.

	do i = 1, MAX_CURVES {
	    call mfree (x[i], TY_REAL)
	    call mfree (y[i], TY_REAL)
	    call mfree (size[i], TY_REAL)
	}

#	if (status == ERR)
#	    call erract (EA_ERROR)
end


# TGR_ONINT -- Interrupt handler for the task graph.  Branches back to ZSVJMP
# in the main routine to permit shutdown without an error message.

procedure sgr_onint (vex, next_handler)

int	vex			# Virtual exception
pointer	next_handler		# not used

pointer	sgrjmp[LEN_JUMPBUF]
common	/sgrcom/ sgrjmp

begin
	call xer_reset()
	call zdojmp (sgrjmp, vex)
end
