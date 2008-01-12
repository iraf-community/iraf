include "igi.h"
include "commands.h"

procedure ig_flip (cmd, igs)

int	cmd
pointer	igs

int	igps
real	wl, wr, wb, wt

begin
	call lcmdcat (igs, YES)
	call cmdcat  (igs, NO)

	igps = PLOT_PARMS(igs)

	call ggwind (GIO_GP(igs), wl, wr, wb, wt)

	if (cmd == XFLIP)
	    call gswind (GIO_GP(igs), wr, wl, wb, wt)
	else if (cmd == YFLIP)
	    call gswind (GIO_GP(igs), wl, wr, wt, wb)

	call ggwind (GIO_GP(igs), 
	    MG_WINDLEFT(igps), MG_WINDRIGHT(igps),
	    MG_WINDBOTTOM(igps), MG_WINDTOP(igps))
end
