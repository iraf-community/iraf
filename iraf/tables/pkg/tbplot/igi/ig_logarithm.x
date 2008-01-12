include "igi.h"
include "commands.h"

#  8/20/91 Removed ^Ls. ZGL
#  1/27/93 Fix INDEF tests.

procedure ig_logarithm (cmd, igs)

int	cmd		# Command index
pointer	igs		# igi parameters structure

pointer	igps		# parameters structure

begin
	igps = PLOT_PARMS(igs)

	if (cmd == XLOGARITHM) {
	    if (MG_XDATAP(igps) == NULL) {
		call eprintf ("No X data defined ")
		return
	    }

	    call ii_xlogarithm (igs)

	    if (DEBUG_OUTPUT(igs) == YES)
		call printf ("Take log of X data ")

	} else if (cmd == YLOGARITHM) {
	    if (MG_YDATAP(igps) == NULL) {
		call eprintf ("No Y data defined ")
		return
	    }

	    call ii_ylogarithm (igs)

	    if (DEBUG_OUTPUT(igs) == YES)
		call printf ("Take log of Y data ")
	}

	call lcmdcat (igs, YES)
	call cmdcat  (igs, NO)
end


procedure ii_xlogarithm (igs)

pointer	igs		# igi parameters structure

pointer	igps		# parameters structure

begin
	igps = PLOT_PARMS(igs)

	call iglogr (Memr[MG_XDATAP(igps)], 
	    Memr[MG_XDATAP(igps)], MG_XNPTS(igps))

	MG_XLOG(igps) = YES
end


procedure ii_ylogarithm (igs)

pointer	igs		# igi parameters structure

pointer	igps		# parameters structure

begin
	igps = PLOT_PARMS(igs)

	call iglogr (Memr[MG_YDATAP(igps)], 
	    Memr[MG_YDATAP(igps)], MG_YNPTS(igps))

	MG_YLOG(igps) = YES
end


procedure iglogr (indat, outdat, n)

real	indat[ARB]
real	outdat[ARB]
int	n

int	i

begin
	do i = 1, n {
	    if (IS_INDEFR (indat[i]) || indat[i] <= 0.0)
		outdat[i] = INDEF
	    else
		outdat[i] = log10 (indat[i])
	}
end
