include <error.h>
include "daoedit.h"

# DP_ECOLON -- Show/set the DAOPHOT algorithm parameters.

procedure dp_ecolon (cmdstr, gd, redraw)

char	cmdstr[ARB]	# input colon command
int	gd		# pointer to the graphics stream
int	redraw		# redraw the radial profile plot

bool	bval
int	cmd, pset, ival
pointer	sp, incmd, param, value
real	rval
bool	clgetb()
int	strdic(), nscan(), clgeti()
real	clgetr()
string	datapars	"datapars"
string	centerpars	"centerpars"
string	fitskypars	"fitskypars"
string	photpars	"photpars"
string	daopars		"daopars"
string	findpars	"findpars"

begin
	call smark (sp)
	call salloc (incmd, SZ_LINE, TY_CHAR)
	call salloc (param, SZ_LINE, TY_CHAR)
	call salloc (value, SZ_LINE, TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	call gargwrd (Memc[incmd], SZ_LINE)
	if (Memc[incmd] == EOS) {
	    call sfree (sp)
	    return
	}

	cmd = strdic (Memc[incmd], Memc[incmd], SZ_LINE, CMD_LIST)
	switch (cmd) {
	case CMD_LPAR:
	    call gargwrd (Memc[incmd], SZ_LINE)
	    pset = strdic (Memc[incmd], Memc[incmd], SZ_LINE, PSET_LIST)
	    if (pset != 0)
		call gdeactivate (gd, 0)
	    switch (pset) {
	    case PSET_DATAPARS:
		call clcmdw ("lparam datapars")
	    case PSET_CENTERPARS:
		call clcmdw ("lparam centerpars")
	    case PSET_FITSKYPARS:
		call clcmdw ("lparam fitskypars")
	    case PSET_PHOTPARS:
		call clcmdw ("lparam photpars")
	    case PSET_DAOPARS:
		call clcmdw ("lparam daopars")
	    case PSET_FINDPARS:
		call clcmdw ("lparam findpars")
	    default:
	        call printf ("Unknown parameter set\7\n")
	    }

	case CMD_EPAR:
	    call gargwrd (Memc[incmd], SZ_LINE)
	    pset = strdic (Memc[incmd], Memc[incmd], SZ_LINE, PSET_LIST)
	    if (pset != 0)
		call gdeactivate (gd, 0)
	    switch (pset) {
	    case PSET_DATAPARS:
		call clcmdw ("eparam datapars")
	    case PSET_CENTERPARS:
		call clcmdw ("eparam centerpars")
	    case PSET_FITSKYPARS:
		call clcmdw ("eparam fitskypars")
	    case PSET_PHOTPARS:
		call clcmdw ("eparam photpars")
	    case PSET_DAOPARS:
		call clcmdw ("eparam daopars")
	    case PSET_FINDPARS:
		call clcmdw ("eparam findpars")
	    default:
	        call printf ("Unknown parameter set\7\n")
	    }
	    if (pset != 0)
		redraw = YES

	case CMD_UNLEARN:
	    call gargwrd (Memc[incmd], SZ_LINE)
	    pset = strdic (Memc[incmd], Memc[incmd], SZ_LINE, PSET_LIST)
	    switch (pset) {
	    case PSET_DATAPARS:
		call clcmdw ("unlearn datapars")
	    case PSET_CENTERPARS:
		call clcmdw ("unlearn centerpars")
	    case PSET_FITSKYPARS:
		call clcmdw ("unlearn fitskypars")
	    case PSET_PHOTPARS:
		call clcmdw ("unlearn photpars")
	    case PSET_DAOPARS:
		call clcmdw ("unlearn daopars")
	    case PSET_FINDPARS:
		call clcmdw ("unlearn findpars")
	    default:
	        call printf ("Unknown parameter set\7\n")
	    }
	    if (pset != 0)
		redraw = YES

	case CMD_SCALE:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (datapars)
		call pargstr (Memc[incmd])
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (Memc[incmd])
		    call pargr (clgetr (Memc[param]))
	    } else {
		call clputr (Memc[param], rval)
		redraw = YES
	    }

	case CMD_FWHMPSF, CMD_SIGMA, CMD_DATAMIN, CMD_DATAMAX,
	    CMD_READNOISE, CMD_EPADU, CMD_ITIME, CMD_XAIRMASS:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (datapars)
		call pargstr (Memc[incmd])
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (Memc[incmd])
		    call pargr (clgetr (Memc[param]))
	    } else
		call clputr (Memc[param], rval)

	case CMD_EMISSION:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (datapars)
		call pargstr (Memc[incmd])
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("%s = %b\n")
		    call pargstr (Memc[incmd])
		    call pargb (clgetb (Memc[param]))
	    } else
		call clputb (Memc[param], bval)

	case CMD_NOISE:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (datapars)
		call pargstr (Memc[incmd])
	    call clgstr (Memc[param], Memc[value], SZ_LINE)
	    call printf ("%s = %s\n")
		call pargstr (Memc[incmd])
		call pargstr (Memc[value])

	case CMD_CCDREAD, CMD_GAIN, CMD_EXPOSURE, CMD_AIRMASS, CMD_FILTER,
	    CMD_OBSTIME, CMD_IFILTER, CMD_OTIME:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (datapars)
		call pargstr (Memc[incmd])
	    call gargwrd (Memc[value], SZ_LINE)
	    if (nscan() == 1) {
		call clgstr (Memc[param], Memc[value], SZ_LINE)
		call printf ("%s = \"%s\"\n")
		    call pargstr (Memc[incmd])
		    call pargstr (Memc[value])
	    } else
		call clpstr (Memc[param], Memc[value])

	case CMD_CALGORITHM:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (centerpars)
		call pargstr (Memc[incmd])
	    call gargwrd (Memc[value], SZ_LINE)
	    if (nscan() == 1) {
		call clgstr (Memc[param], Memc[value], SZ_LINE)
		call printf ("%s = \"%s\"\n")
		    call pargstr (Memc[incmd])
		    call pargstr (Memc[value])
	    } else {
		call clpstr (Memc[param], Memc[value])
	    }

	case CMD_CBOX:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (centerpars)
		call pargstr (Memc[incmd])
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (Memc[incmd])
		    call pargr (clgetr (Memc[param]))
	    } else {
		call clputr (Memc[param], rval)
		redraw = YES
	    }

	case CMD_CTHRESHOLD, CMD_MAXSHIFT, CMD_MINSNRATIO, CMD_RCLEAN,
	    CMD_RCLIP, CMD_KCLEAN:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (centerpars)
		call pargstr (Memc[incmd])
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (Memc[incmd])
		    call pargr (clgetr (Memc[param]))
	    } else
		call clputr (Memc[param], rval)

	case CMD_CMAXITER:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (centerpars)
		call pargstr (Memc[incmd])
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("%s = %d\n")
		    call pargstr (Memc[incmd])
		    call pargi (clgeti (Memc[param]))
	    } else
		call clputi (Memc[param], ival)

	case CMD_CLEAN, CMD_MKCENTER:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (centerpars)
		call pargstr (Memc[incmd])
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("%s = %b\n")
		    call pargstr (Memc[incmd])
		    call pargb (clgetb (Memc[param]))
	    } else
		call clputb (Memc[param], bval)

	case CMD_SALGORITHM:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (fitskypars)
		call pargstr (Memc[incmd])
	    call gargwrd (Memc[value], SZ_LINE)
	    if (nscan() == 1) {
		call clgstr (Memc[param], Memc[value], SZ_LINE)
		call printf ("%s = \"%s\"\n")
		    call pargstr (Memc[incmd])
		    call pargstr (Memc[value])
	    } else {
		call clpstr (Memc[param], Memc[value])
		call clgstr (Memc[param], Memc[value], SZ_LINE)
	    }

	case CMD_ANNULUS, CMD_DANNULUS:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (fitskypars)
		call pargstr (Memc[incmd])
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (Memc[incmd])
		    call pargr (clgetr (Memc[param]))
	    } else {
		call clputr (Memc[param], rval)
		redraw = YES
	    }

	case CMD_SLOCLIP, CMD_SHICLIP, CMD_SLOREJECT, CMD_SHIREJECT,
	    CMD_KHIST, CMD_BINSIZE, CMD_SKYVALUE, CMD_RGROW:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (fitskypars)
		call pargstr (Memc[incmd])
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (Memc[incmd])
		    call pargr (clgetr (Memc[param]))
	    } else
		call clputr (Memc[param], rval)

	case CMD_SMAXITER, CMD_SNREJECT:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (fitskypars)
		call pargstr (Memc[incmd])
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("%s = %d\n")
		    call pargstr (Memc[incmd])
		    call pargi (clgeti (Memc[param]))
	    } else
		call clputi (Memc[param], ival)

	case CMD_SMOOTH, CMD_MKSKY:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (fitskypars)
		call pargstr (Memc[incmd])
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("%s = %b\n")
		    call pargstr (Memc[incmd])
		    call pargb (clgetb (Memc[param]))
	    } else
		call clputb (Memc[param], bval)

	case CMD_WEIGHTING:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (photpars)
		call pargstr (Memc[incmd])
	    call clgstr (Memc[param], Memc[value], SZ_LINE)
	    call printf ("%s = %s\n")
		call pargstr (Memc[incmd])
		call pargstr (Memc[value])

	case CMD_APERTURES:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (photpars)
		call pargstr (Memc[incmd])
	    call gargwrd (Memc[value], SZ_LINE)
	    if (nscan() == 1) {
		call clgstr (Memc[param], Memc[value], SZ_LINE)
		call printf ("%s = \"%s\"\n")
		    call pargstr (Memc[incmd])
		    call pargstr (Memc[value])
	    } else {
		call clpstr (Memc[param], Memc[value])
		redraw = YES
	    }

	case CMD_ZMAG:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (photpars)
		call pargstr (Memc[incmd])
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (Memc[incmd])
		    call pargr (clgetr (Memc[param]))
	    } else {
		call clputr (Memc[param], rval)
	    }

	case CMD_MKAPERT:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (photpars)
		call pargstr (Memc[incmd])
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("%s = %b\n")
		    call pargstr (Memc[incmd])
		    call pargb (clgetb (Memc[param]))
	    } else
		call clputb (Memc[param], bval)

	case CMD_FUNCTION:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (daopars)
		call pargstr (Memc[incmd])
	    call clgstr (Memc[param], Memc[value], SZ_LINE)
	    call printf ("%s = %s\n")
		call pargstr (Memc[incmd])
		call pargstr (Memc[value])

	case CMD_RECENTER, CMD_FITSKY, CMD_GROUPSKY, CMD_SATURATED:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (daopars)
		call pargstr (Memc[incmd])
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("%s = %b\n")
		    call pargstr (Memc[incmd])
		    call pargb (clgetb (Memc[param]))
	    } else
		call clputb (Memc[param], bval)

	case CMD_PSFRAD, CMD_FITRAD:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (daopars)
		call pargstr (Memc[incmd])
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (Memc[incmd])
		    call pargr (clgetr (Memc[param]))
	    } else
		call clputr (Memc[param], rval)

	case CMD_MATCHRAD, CMD_SANNULUS, CMD_WSANNULUS, CMD_FLATERR,
	    CMD_PROFERR, CMD_CRITOVERLAP, CMD_CLIPRANGE:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (daopars)
		call pargstr (Memc[incmd])
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (Memc[incmd])
		    call pargr (clgetr (Memc[param]))
	    } else
		call clputr (Memc[param], rval)

	case CMD_VARORDER, CMD_NCLEAN, CMD_MAXITER, CMD_MAXGROUP,
	    CMD_MAXNSTAR, CMD_CLIPEXP:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (daopars)
		call pargstr (Memc[incmd])
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("%s = %d\n")
		    call pargstr (Memc[incmd])
		    call pargi (clgeti (Memc[param]))
	    } else
		call clputi (Memc[param], ival)

	case CMD_THRESHOLD, CMD_NSIGMA, CMD_RATIO, CMD_THETA, CMD_SHARPLO,
	    CMD_SHARPHI, CMD_ROUNDLO, CMD_ROUNDHI:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (findpars)
		call pargstr (Memc[incmd])
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (Memc[incmd])
		    call pargr (clgetr (Memc[param]))
	    } else
		call clputr (Memc[param], rval)

	case CMD_MKDETECTIONS:
	    call sprintf (Memc[param], SZ_LINE, "%s.%s")
		call pargstr (findpars)
		call pargstr (Memc[incmd])
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("%s = %b\n")
		    call pargstr (Memc[incmd])
		    call pargb (clgetb (Memc[param]))
	    } else
		call clputb (Memc[param], bval)

	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
