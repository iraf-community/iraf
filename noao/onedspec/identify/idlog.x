include	<time.h>
include	"identify.h"

# ID_LOG -- Write log

procedure id_log (id, file)

pointer	id			# ID pointer
char	file[ARB]		# Log file

char	str[SZ_TIME]
int	i, fd, nrms
double	resid, rms

int	open()
long	clktime()
errchk	open()

begin
	if (ID_NFEATURES(id) == 0)
	    return

	fd = open (file, APPEND, TEXT_FILE)

	call cnvtime (clktime (0), str, SZ_TIME)
	call fprintf (fd, "\n%s\n")
	    call pargstr (str)
	call fprintf (fd, "Features identified in image %s.\n")
	    call pargstr (ID_IMAGE(id))

	call fprintf (fd, "   %8s %10s %10s %10s %6s %2s %s\n")
	    call pargstr ("Pixel")
	    call pargstr ("Fit")
	    call pargstr ("User")
	    call pargstr ("Residual")
	    call pargstr ("Fwidth")
	    call pargstr ("Wt")
	    call pargstr ("Label")

	rms = 0.
	nrms = 0
	do i = 1, ID_NFEATURES(id) {
	    call fprintf (fd, "%2d %8.2f %10.8g %10.8g %10.8g %6.2f %2d %s\n")
		call pargi (i)
		call pargd (PIX(id,i))
		call pargd (FIT(id,i))
		call pargd (USER(id,i))
		if (IS_INDEFD (USER(id,i)))
		    call pargd (USER(id,i))
		else {
		    resid = FIT(id,i) - USER(id,i)
		    call pargd (resid)
		    if (WTS(id,i) > 0.) {
		        rms = rms + resid ** 2
		        nrms = nrms + 1
		    }
		}
		call pargr (FWIDTH(id,i))
		call pargd (WTS(id,i))
		if (Memi[ID_LABEL(id)+i-1] != NULL)
		    call pargstr (Memc[Memi[ID_LABEL(id)+i-1]])
		else
		    call pargstr ("")
	}

	if (nrms > 1) {
	    call fprintf (fd, "RMS = %0.8g\n")
		call pargd (sqrt (rms / nrms))
	}

	call close (fd)
end
