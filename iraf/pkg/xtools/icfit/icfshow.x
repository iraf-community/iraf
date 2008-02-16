# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/gtools.h>
include	"icfit.h"

# IC_FSHOW -- Show the values of the parameters.

procedure ic_fshow (ic, fd)

pointer	ic			# ICFIT pointer
int	fd			# Output file

pointer	str, ptr
long	clktime()

begin
	call malloc (str, SZ_LINE, TY_CHAR)

	call cnvtime (clktime(0), Memc[str], SZ_LINE)
	call fprintf (fd, "\n# %s\n")
	    call pargstr (Memc[str])

	if (IC_GT(ic) != NULL) {
	    # The title may contain new lines so we have to put comments
	    # in front of each line.
	    call gt_gets (IC_GT(ic), GTTITLE, Memc[str], SZ_LINE)
	    call putline (fd, "# ")
	    for (ptr=str; Memc[ptr]!=EOS; ptr=ptr+1) {
		call putc (fd, Memc[ptr])
		if (Memc[ptr] == '\n') {
		    call putline (fd, "# ")
		}
	    }
	    call putline (fd, "\n")

	    call gt_gets (IC_GT(ic), GTYUNITS, Memc[str], SZ_LINE)
	    if (Memc[str] != EOS) {
		call fprintf (fd, "# fit units = %s\n")
		    call pargstr (Memc[str])
	    }
	}

	call ic_gstr (ic, "function", Memc[str], SZ_LINE)
	call fprintf (fd, "# function = %s\n")
	    call pargstr (Memc[str])
	call fprintf (fd, "# grow = %g\n")
	    call pargr (IC_GROW(ic))
	call fprintf (fd, "# naverage = %d\n")
	    call pargi (IC_NAVERAGE(ic))
	call fprintf (fd, "# order = %d\n")
	    call pargi (IC_ORDER(ic))
	call fprintf (fd, "# low_reject = %g\n")
	    call pargr (IC_LOW(ic))
	call fprintf (fd, "# high_reject = %g\n")
	    call pargr (IC_HIGH(ic))
	call fprintf (fd, "# niterate = %d\n")
	    call pargi (IC_NITERATE(ic))
	call fprintf (fd, "# sample = %s\n")
	    call pargstr (Memc[IC_SAMPLE(ic)])

	call mfree (str, TY_CHAR)
end
