include	<smw.h>
include	<time.h>
include	"identify.h"


# ID_LOG -- Write log

procedure id_log (id, file, logfd)

pointer	id			# ID pointer
char	file[ARB]		# Log file
int	logfd			# Log fd

char	str[SZ_TIME]
int	i, fd, nrms
double	z, zrms, zerr, zhelio, resid, rms, v, verr, vhelio, hjd

int	open()
double	id_zshiftd(), id_zval()
long	clktime()
errchk	open, id_velocity, id_vhelio

begin
	if (ID_NFEATURES(id) == 0)
	    return

	if (logfd == NULL)
	    fd = open (file, APPEND, TEXT_FILE)
	else
	    fd = logfd

	if (ID_TASK(id) == IDENTIFY) {

	    call cnvtime (clktime (0), str, SZ_TIME)
	    call fprintf (fd, "\n%s\n")
		call pargstr (str)
	    call fprintf (fd, "Features identified in image %s%s: %s\n")
		call pargstr (Memc[ID_IMAGE(id)])
		call pargstr (Memc[ID_SECTION(id)])
		call pargstr (TITLE(ID_SH(id)))

	    call fprintf (fd, "   %8s %10s %10s %10s %5s %s\n")
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
		call fprintf (fd,
		    "%2d %8.2f %10.8g %10.8g %10.4g %4f %s\n")
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
		    call pargd (WTS(id,i))
		    if (Memi[ID_LABEL(id)+i-1] != NULL)
			call pargstr (Memc[Memi[ID_LABEL(id)+i-1]])
		    else
			call pargstr ("")
	    }

	    if (nrms > 1) {
		call fprintf (fd, "RMS = %0.6g\n")
		    call pargd (sqrt (rms / nrms))
	    }

	} else {
	    call id_velocity (id, NO)
	    z = ID_REDSHIFT(id)
	    zrms = ID_RMSRED(id)
	    zhelio = ID_ZHELIO(id)
	    v = ID_REDSHIFT(id) * VLIGHT
	    call id_vhelio (IM(ID_SH(id)), vhelio, hjd, fd)

	    call cnvtime (clktime (0), str, SZ_TIME)
	    call fprintf (fd, "\n%s\n")
		call pargstr (str)
	    call fprintf (fd, "Features identified in image %s%s: %s\n")
		call pargstr (Memc[ID_IMAGE(id)])
		call pargstr (Memc[ID_SECTION(id)])
		call pargstr (TITLE(ID_SH(id)))

	    call fprintf (fd, "%10s %10s %10s %10s %10s %5s %s\n")
		call pargstr ("Measured")
		call pargstr ("User")
		call pargstr ("Residual")
		call pargstr ("Velocity")
		call pargstr ("Residual")
		call pargstr ("Wt")
		call pargstr ("Label")

	    rms = 0.
	    nrms = 0
	    do i = 1, ID_NFEATURES(id) {
		call fprintf (fd,
		    "%10.8g %10.8g %10.4g %10.8g %10.4g %4f %s\n")
		    call pargd (id_zshiftd (id, FIT(id,i), 0))
		    if (IS_INDEFD (USER(id,i))) {
			call pargd (INDEFD)
			call pargd (INDEFD)
			call pargd (INDEFD)
			call pargd (INDEFD)
		    } else {
			call pargd (USER(id,i))
			resid = id_zshiftd (id, FIT(id,i), 0) - USER(id,i)
			call pargd (resid)
			if (WTS(id,i) > 0.) {
			    rms = rms + resid ** 2
			    nrms = nrms + 1
			}
			verr = id_zval (id, FIT(id,i), USER(id,i)) * VLIGHT
			call pargd (verr + vhelio)
			call pargd (verr - v)
		    }
		    call pargd (WTS(id,i))
		    if (Memi[ID_LABEL(id)+i-1] != NULL)
			call pargstr (Memc[Memi[ID_LABEL(id)+i-1]])
		    else
			call pargstr ("")
	    }

	    if (nrms > 1) {
		call fprintf (fd, "Wavelength RMS = %0.6g\n")
		    call pargd (sqrt (rms / nrms))
		call fprintf (fd, "Velocity RMS = %8.5g\n")
		    call pargd (zrms * VLIGHT)
	    }

	    zerr = zrms
	    if (nrms > 1)
		zerr = zerr / sqrt (nrms - 1.)
	    v = z * VLIGHT
	    verr = zerr * VLIGHT

	    call fprintf (fd, "\n")
	    call fprintf (fd, "%s %3d : Zobs     = %10.5g,    ")
		call pargstr (Memc[ID_IMAGE(id)])
		call pargi (ID_AP(id,1))
		call pargd (z)
	    call fprintf (fd, "Mean err = %10.5g,    Lines = %3d\n")
		call pargd (zerr)
		call pargi (nrms)
	    call fprintf (fd, "%s %3d : Vobs     = %8.5g km/s, ")
		call pargstr (Memc[ID_IMAGE(id)])
		call pargi (ID_AP(id,1))
		call pargd (v)
	    call fprintf (fd, "Mean err = %8.5g km/s, Lines = %3d\n")
		call pargd (verr)
		call pargi (nrms)
	    if (zhelio != 0D0) {
		call fprintf (fd, "%s %3d : Zhelio   = %10.5g,    ")
		    call pargstr (Memc[ID_IMAGE(id)])
		    call pargi (ID_AP(id,1))
		    call pargd (z + zhelio)
		call fprintf (fd, "Mean err = %8.5g km/s, Lines = %3d\n")
		    call pargd (zerr)
		    call pargi (nrms)
		call fprintf (fd, "%s %3d : Vhelio   = %8.5g km/s, ")
		    call pargstr (Memc[ID_IMAGE(id)])
		    call pargi (ID_AP(id,1))
		    call pargd (v + vhelio)
		call fprintf (fd, "Mean err = %8.5g km/s, Lines = %3d\n")
		    call pargd (verr)
		    call pargi (nrms)
		call fprintf (fd, "%s %3d : HJD      = %g\n")
		    call pargstr (Memc[ID_IMAGE(id)])
		    call pargi (ID_AP(id,1))
		    call pargd (hjd)
	    }
	    call fprintf (fd, "\n")
	}

	if (logfd == NULL)
	    call close (fd)
end
