include	"../identify.h"


# AID_LOG -- Log final solution.

procedure aid_log (id, fd, hdr)

pointer	id		#I ID object
int	fd		#I Log file descriptor
int	hdr		#U Print header?

double	wc, dw, id_fitpt(), id_rms()
pointer	str
bool	fp_equald()

begin
	if (fd == NULL)
	    return

	if (fd == STDOUT && ID_GP(id) != NULL)
	    call gdeactivate (ID_GP(id), 0)

	if (hdr == YES) {
	    call malloc (str, SZ_LINE, TY_CHAR)
	    call sysid (Memc[str], SZ_LINE)
	    call fprintf (fd, "\nAUTOIDENTIFY: %s\n")
		call pargstr (Memc[str])
	    call mfree (str, TY_CHAR)

	    call fprintf (fd, "  %-20s  %10s %10s %10s %10s\n")
		call pargstr ("Spectrum")
		call pargstr ("# Found")
		call pargstr ("Midpoint")
		call pargstr ("Dispersion")
		call pargstr ("RMS")

	    hdr = NO
	}

	call fprintf (fd, "  %s%s%24t ")
	    call pargstr (ID_IMAGE(id))
	    call pargstr (ID_SECTION(id))
	if (ID_CV(id) == NULL)
	    call fprintf (fd, " No solution found\n")
	else {
	    wc = id_fitpt (id, (ID_NPTS(id) + 1D0) / 2D0)
	    dw = wc - id_fitpt (id, (ID_NPTS(id) - 1D0) / 2D0)
	    if (!fp_equald (dw, 0D0)) {
		call fprintf (fd, "%10d %10.*g %10.3g %10.3g\n")
		    call pargi (ID_NFEATURES(id))
		    call pargi (int (log10 (abs (wc / dw)) + 3))
		    call pargd (wc)
		    call pargd (dw)
		    call pargd (id_rms(id))
	    }
	}
end
