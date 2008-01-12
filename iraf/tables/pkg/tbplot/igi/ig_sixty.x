include "igi.h"
include "commands.h"

# 2/4/91 Changed the default number of digits of precision to 0 instead
# of INDEF.  ZGL

#  8/20/91 Removed ^Ls. ZGL
#  1/27/93 Fix INDEF tests.

procedure ig_sixty (cmd, igs)

int	cmd		# Command index
pointer	igs		# igi structure

int	ndec

int	get_int()

errchk	get_int

begin
	iferr (ndec = get_int (igs))
	    return

	switch (cmd) {
	case XSIXTY:
	    call ii_xsixty (igs, ndec)

	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("X axis sexagesimal labels, %d decimal places ")
		    call pargi (ndec)
	    }

	case YSIXTY:
	    call ii_ysixty (igs, ndec)

	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("Y axis sexagesimal labels, %d decimal places ")
		    call pargi (ndec)
	    }
	}
end


procedure ii_xsixty (igs, ndec)

pointer	igs		# igi structure
int	ndec

pointer	igps

begin
	igps = PLOT_PARMS(igs)

	MG_SEXAGX(igps) = YES

	if (IS_INDEFI (ndec))
	    MG_NDECMX(igps) = 0
	else
	    MG_NDECMX(igps) = ndec
end


procedure ii_ysixty (igs, ndec)

pointer	igs		# igi structure
int	ndec

pointer	igps

begin
	igps = PLOT_PARMS(igs)

	MG_SEXAGY(igps) = YES

	if (IS_INDEFI (ndec))
	    MG_NDECMY(igps) = 0
	else
	    MG_NDECMY(igps) = ndec
end
