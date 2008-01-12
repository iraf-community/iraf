include	<time.h>
include	"ecidentify.h"

# EC_LOG -- Write log

procedure ec_log (ec, file)

pointer	ec			# ID pointer
char	file[ARB]		# Log file

char	str[SZ_TIME]
int	i, fd, nrms
double	resid, rms

int	open()
long	clktime()
errchk	open()

begin
	if (EC_NFEATURES(ec) == 0)
	    return

	fd = open (file, APPEND, TEXT_FILE)

	call cnvtime (clktime (0), str, SZ_TIME)
	call fprintf (fd, "\n%s\n")
	    call pargstr (str)
	call fprintf (fd, "Features identified in image %s.\n")
	    call pargstr (Memc[EC_IMAGE(ec)])

	call fprintf (fd, "      %3s %4s %5s %8s %10s %10s %10s %6s %6d\n")
	    call pargstr ("Ap")
	    call pargstr ("Line")
	    call pargstr ("Order")
	    call pargstr ("Pixel")
	    call pargstr ("Fit")
	    call pargstr ("User")
	    call pargstr ("Residual")
	    call pargstr ("Fwidth")
	    call pargstr ("Reject")

	rms = 0.
	nrms = 0
	do i = 1, EC_NFEATURES(ec) {
	    call fprintf (fd,
		"%5d %3d %4d %5d %8.2f %10.8g %10.8g %10.8g %6.2f %6b\n")
		call pargi (i)
		call pargi (APN(ec,i))
		call pargi (LINE(ec,i))
		call pargi (ORDER(ec,i))
		call pargd (PIX(ec,i))
		call pargd (FIT(ec,i))
		call pargd (USER(ec,i))
		if (IS_INDEFD (USER(ec,i)))
		    call pargd (USER(ec,i))
		else {
		    resid = FIT(ec,i) - USER(ec,i)
		    call pargd (resid)
		    if (FTYPE(ec,i) > 0) {
			rms = rms + resid ** 2
			nrms = nrms + 1
		    }
		}
		call pargr (FWIDTH(ec,i))
		if (FTYPE(ec,i) > 0)
		    call pargb (false)
		else
		    call pargb (true)
	}

	if (nrms > 1) {
	    call fprintf (fd, "RMS = %0.8g\n")
		call pargd (sqrt (rms / nrms))
	}

	call close (fd)
end
