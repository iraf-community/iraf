# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/gtools.h>
include	"icfit.h"

# IC_SHOW -- Show the values of the parameters.

procedure ic_show (ic, file, gt)

pointer	ic			# ICFIT pointer
char	file[ARB]		# Output file
pointer	gt			# GTOOLS pointer

int	fd
pointer	str

int	open()
long	clktime()
errchk	open()

begin
	fd = open (file, APPEND, TEXT_FILE)
	call malloc (str, SZ_LINE, TY_CHAR)

	call cnvtime (clktime(0), Memc[str], SZ_LINE)
	call fprintf (fd, "\n%s\n")
	    call pargstr (Memc[str])
	call gt_gets (gt, GTTITLE, Memc[str], SZ_LINE)
	call fprintf (fd, "%s\n")
	    call pargstr (Memc[str])
	call gt_gets (gt, GTYUNITS, Memc[str], SZ_LINE)
	if (Memc[str] != EOS) {
	    call fprintf (fd, "fit units = %s\n")
	        call pargstr (Memc[str])
	}
	call ic_gstr (ic, "function", Memc[str], SZ_LINE)
	call fprintf (fd, "function = %s\n")
	    call pargstr (Memc[str])
	call fprintf (fd, "grow = %g\n")
	    call pargr (IC_GROW(ic))
	call fprintf (fd, "naverage = %d\n")
	    call pargi (IC_NAVERAGE(ic))
	call fprintf (fd, "order = %d\n")
	    call pargi (IC_ORDER(ic))
	call fprintf (fd, "low_reject = %g\n")
	    call pargr (IC_LOW(ic))
	call fprintf (fd, "high_reject = %g\n")
	    call pargr (IC_HIGH(ic))
	call fprintf (fd, "niterate = %d\n")
	    call pargi (IC_NITERATE(ic))
	call fprintf (fd, "sample = %s\n")
	    call pargstr (Memc[IC_SAMPLE(ic)])

	call mfree (str, TY_CHAR)
	call close (fd)
end
