include <error.h>
include "starlist.h"

# ST_COLON -- Process colon commands for the STARLIST task.

procedure st_colon (gd, st, sf, lf, cmdstr, newspace, newlum, newplot)

pointer	gd		# pointer to the graphics stream
pointer	st		# pointer to starlist structure
int	sf		# spatial density function file descriptor
int	lf		# luminosity function file descriptor
char	cmdstr[ARB]	# input command string
int	newspace	# new spatial distribution function
int	newlum		# new luminosity function
int	newplot		# new plot

int	ival, ncmd, stat
pointer	sp, cmd, str
long	lval
real	rval
bool	streq()
int	strdic(), nscan(), open()

string	lumfuncs LUMFUNCS

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS)
	    return

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, STCMDS)
	switch (ncmd) {

	case STCMD_SHOW:
	    call gdeactivate (gd, 0)
	    call st_show (st)
	    call greactivate (gd, 0)

	case STCMD_SPATIAL:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan () == 1) {
	        call printf ("spatial = %s\n")
		    call pargstr (ST_SPSTRING(st))
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, SPFUNCS)
		if (stat > 0) {
		    ST_SPATIAL(st) = stat
		    call strcpy (Memc[cmd], ST_SPSTRING(st), SZ_FNAME)
		}
		newspace = YES
	    }

	case STCMD_XCENTER:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("xcenter = %g pixels\n")
		    call pargr (ST_XC(st))
	    } else {
		ST_XC(st) = rval
		newspace = YES
	    }

	case STCMD_YCENTER:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("ycenter = %g pixels\n")
		    call pargr (ST_YC(st))
	    } else {
		ST_YC(st) = rval
		newspace = YES
	    }

	case STCMD_CORE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("core = %g pixels\n")
		    call pargr (ST_CORE(st))
	    } else {
		ST_CORE(st) = rval
		newspace = YES
	    }

	case STCMD_BASE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("base = %g pixels\n")
		    call pargr (ST_BASE(st))
	    } else {
		ST_BASE(st) = rval
		newspace = YES
	    }

	case STCMD_XMIN:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("xmin = %g pixels\n")
		    call pargr (ST_XMIN(st))
	    } else {
		ST_XMIN(st) = rval
		newspace = YES
	    }

	case STCMD_XMAX:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("xmax = %g pixels\n")
		    call pargr (ST_XMAX(st))
	    } else {
		ST_XMAX(st) = rval
		newspace = YES
	    }

	case STCMD_YMIN:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("ymin = %g pixels\n")
		    call pargr (ST_YMIN(st))
	    } else {
		ST_YMIN(st) = rval
		newspace = YES
	    }

	case STCMD_YMAX:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("ymax = %g pixels\n")
		    call pargr (ST_YMAX(st))
	    } else {
		ST_YMAX(st) = rval
		newspace = YES
	    }

	case STCMD_SFILE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], ST_SFILE(st))) {
		call printf ("sfile: %s\n")
		    call pargstr (ST_SFILE(st))
	    } else {
		if (sf != NULL) {
		    call close (sf)
		    sf = NULL
		}
		iferr {
		    sf = open (Memc[cmd], READ_ONLY, TEXT_FILE)
		} then {
		    sf = NULL
		    call erract (EA_WARN)
		    call strcpy ("", ST_SFILE(st), SZ_FNAME)
		    call printf (
		        "Spatial distribution function file is undefined.\n")
		} else {
		    call strcpy (Memc[cmd], ST_SFILE(st), SZ_FNAME)
		    newspace = YES
		}
	    }

	case STCMD_LUMINOSITY:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan () == 1) {
	        call printf ("luminosity = %s\n")
		    call pargstr (ST_LFSTRING(st))
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, LUMFUNCS)
		if (stat > 0) {
		    ST_LUMINOSITY (st) = stat
		    call strcpy (Memc[cmd], ST_LFSTRING(st), SZ_FNAME)
		}
		newlum = YES
	    }

	case  STCMD_POWER:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("power = %g\n")
		    call pargr (ST_POWER(st))
	    } else {
		ST_POWER(st) = rval
		newlum = YES
	    }

	case  STCMD_MZERO:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("mzero = %g\n")
		    call pargr (ST_MZERO(st))
	    } else {
		ST_MZERO(st) = rval
		newlum = YES
	    }

	case  STCMD_ALPHA:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("alpha = %g\n")
		    call pargr (ST_ALPHA(st))
	    } else {
		ST_ALPHA(st) = rval
		newlum = YES
	    }

	case  STCMD_BETA:
	    call gargr (rval)
	    if (nscan() == 1) {
	        call printf ("beta = %g\n")
		    call pargr (ST_BETA(st))
	    } else {
		ST_BETA(st) = rval
		newlum = YES
	    }

	case STCMD_DELTA:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("delta = %g\n")
		    call pargr (ST_DELTA(st))
	    } else {
		ST_DELTA(st) = rval
		newlum = YES
	    }

	case STCMD_MSTAR:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("mstar = %g\n")
		    call pargr (ST_MSTAR(st))
	    } else {
		ST_MSTAR(st) = rval
		newlum = YES
	    }

	case STCMD_MINMAG:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("minmag = %g\n")
		    call pargr (ST_MINMAG(st))
	    } else {
		ST_MINMAG(st) = rval
		newlum = YES
	    }

	case STCMD_MAXMAG:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("maxmag = %g\n")
		    call pargr (ST_MAXMAG(st))
	    } else {
		ST_MAXMAG(st) = rval
		newlum = YES
	    }

	case STCMD_LFILE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], ST_LFILE(st))) {
		call printf ("lfile: %s\n")
		    call pargstr (ST_LFILE(st))
	    } else {
		if (lf != NULL) {
		    call close (lf)
		    lf = NULL
		}
		iferr {
		    lf = open (Memc[cmd], READ_ONLY, TEXT_FILE)
		} then {
		    lf = NULL
		    call erract (EA_WARN)
		    call strcpy ("", ST_LFILE(st), SZ_FNAME)
		    call printf (
		        "Luminosity function file is undefined.\n")
		} else {
		    call strcpy (Memc[cmd], ST_LFILE(st), SZ_FNAME)
		    newlum = YES
		}
	    }

	case STCMD_NSTARS:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nstars = %d\n")
		    call pargi (ST_NSTARS(st))
	    } else {
		ST_NSTARS(st) = ival
		newlum = YES
		newspace = YES
	    }

	case STCMD_NSSAMPLE:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nssample = %d\n")
		    call pargi (ST_NSSAMPLE(st))
	    } else {
		ST_NSSAMPLE(st) = ival
		newspace = YES
	    }

	case STCMD_SORDER:
	    call gargl (lval)
	    if (nscan() == 1) {
		call printf ("sorder = %d\n")
		    call pargl (ST_SORDER(st))
	    } else {
		ST_SORDER(st) = lval
		newspace = YES
	    }

	case STCMD_SSEED:
	    call gargl (lval)
	    if (nscan() == 1) {
		call printf ("sseed = %d\n")
		    call pargl (ST_SSEED(st))
	    } else {
		ST_SSEED(st) = lval
		newspace = YES
	    }

	case STCMD_NLSAMPLE:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nlsample = %d\n")
		    call pargi (ST_NLSAMPLE(st))
	    } else {
		ST_NLSAMPLE(st) = ival
		newlum = YES
	    }

	case STCMD_LORDER:
	    call gargl (lval)
	    if (nscan() == 1) {
		call printf ("lorder = %d\n")
		    call pargl (ST_LORDER(st))
	    } else {
		ST_LORDER(st) = lval
		newlum = YES
	    }

	case STCMD_LSEED:
	    call gargl (lval)
	    if (nscan() == 1) {
		call printf ("lseed = %d\n")
		    call pargl (ST_LSEED(st))
	    } else {
		ST_LSEED(st) = lval
		newlum = YES
	    }

	case STCMD_RBINSIZE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("rbinsize = %g\n")
		    call pargr (ST_RBINSIZE(st))
	    } else {
		ST_RBINSIZE(st) = rval
		newplot = YES
	    }

	case STCMD_MBINSIZE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("mbinsize = %g\n")
		    call pargr (ST_MBINSIZE(st))
	    } else {
		ST_MBINSIZE(st) = rval
		newplot = YES
	    }

	default:
	    call printf ("\7\n")
	}

	call sfree (sp)
end


# ST_GCOLON -- Process colon commands for the GALAXIES task.

procedure st_gcolon (gd, st, sf, lf, cmdstr, newspace, newlum, newmix, newaxis,
	newround, newphi, newplot)

pointer	gd		# pointer to the graphics stream
pointer	st		# pointer to starlist structure
int	sf		# spatial distribution function file descriptor
int	lf		# luminosity function file descriptor
char	cmdstr[ARB]	# command string
int	newspace	# new spatial distribution function
int	newlum		# new luminosity function
int	newmix		# new E/S galaxy mixture
int	newaxis		# new axis parameter
int	newround	# compute new roundness parameters
int	newphi		# compute new position angles
int	newplot		# make a newplot

int	ival, ncmd, stat
pointer	sp, cmd, str
long	lval
real	rval
bool	streq()
int	strdic(), nscan(), open()

string	lumfuncs LUMFUNCS

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS)
	    return

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, STCMDS)
	switch (ncmd) {
	case STCMD_SHOW:
	    call gdeactivate (gd, 0)
	    call st_gshow (st)
	    call greactivate (gd, 0)

	case STCMD_SPATIAL:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan () == 1) {
	        call printf ("spatial = %s\n")
		    call pargstr (ST_SPSTRING(st))
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, SPFUNCS)
		if (stat > 0) {
		    ST_SPATIAL(st) = stat
		    call strcpy (Memc[cmd], ST_SPSTRING(st), SZ_FNAME)
		}
		newspace = YES
	    }

	case STCMD_XCENTER:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("xcenter = %g pixels\n")
		    call pargr (ST_XC(st))
	    } else {
		ST_XC(st) = rval
		newspace = YES
	    }

	case STCMD_YCENTER:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("ycenter = %g pixels\n")
		    call pargr (ST_YC(st))
	    } else {
		ST_YC(st) = rval
		newspace = YES
	    }

	case STCMD_CORE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("core = %g pixels\n")
		    call pargr (ST_CORE(st))
	    } else {
		ST_CORE(st) = rval
		newspace = YES
	    }

	case STCMD_BASE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("base = %g pixels\n")
		    call pargr (ST_BASE(st))
	    } else {
		ST_BASE(st) = rval
		newspace = YES
	    }

	case STCMD_XMIN:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("xmin = %g pixels\n")
		    call pargr (ST_XMIN(st))
	    } else {
		ST_XMIN(st) = rval
		newspace = YES
	    }

	case STCMD_XMAX:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("xmax = %g pixels\n")
		    call pargr (ST_XMAX(st))
	    } else {
		ST_XMAX(st) = rval
		newspace = YES
	    }

	case STCMD_YMIN:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("ymin = %g pixels\n")
		    call pargr (ST_YMIN(st))
	    } else {
		ST_YMIN(st) = rval
		newspace = YES
	    }

	case STCMD_YMAX:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("ymax = %g pixels\n")
		    call pargr (ST_YMAX(st))
	    } else {
		ST_YMAX(st) = rval
		newspace = YES
	    }

	case STCMD_SFILE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], ST_SFILE(st))) {
		call printf ("sfile: %s\n")
		    call pargstr (ST_SFILE(st))
	    } else {
		if (sf != NULL) {
		    call close (sf)
		    sf = NULL
		}
		iferr {
		    sf = open (Memc[cmd], READ_ONLY, TEXT_FILE)
		} then {
		    sf = NULL
		    call erract (EA_WARN)
		    call strcpy ("", ST_SFILE(st), SZ_FNAME)
		    call printf (
		        "Spatial distribution function file is undefined.\n")
		} else {
		    call strcpy (Memc[cmd], ST_SFILE(st), SZ_FNAME)
		    newspace = YES
		}
	    }

	case STCMD_LUMINOSITY:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan () == 1) {
	        call printf ("luminosity = %s\n")
		    call pargstr (ST_LFSTRING(st))
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, GLUMFUNCS)
		if (stat > 0) {
		    ST_LUMINOSITY (st) = stat
		    call strcpy (Memc[cmd], ST_LFSTRING(st), SZ_FNAME)
		}
		newlum = YES
	    }

	case  STCMD_POWER:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("power = %g\n")
		    call pargr (ST_POWER(st))
	    } else {
		ST_POWER(st) = rval
		newlum = YES
	    }

	case  STCMD_MZERO:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("mzero = %g\n")
		    call pargr (ST_MZERO(st))
	    } else {
		ST_MZERO(st) = rval
		newlum = YES
	    }

	case  STCMD_ALPHA:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("alpha = %g\n")
		    call pargr (ST_ALPHA(st))
	    } else {
		ST_ALPHA(st) = rval
		newlum = YES
	    }

	case STCMD_MSTAR:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("mstar = %g\n")
		    call pargr (ST_MSTAR(st))
	    } else {
		ST_MSTAR(st) = rval
		newlum = YES
	    }

	case STCMD_MINMAG:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("minmag = %g\n")
		    call pargr (ST_MINMAG(st))
	    } else {
		ST_MINMAG(st) = rval
		newlum = YES
	    }

	case STCMD_MAXMAG:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("maxmag = %g\n")
		    call pargr (ST_MAXMAG(st))
	    } else {
		ST_MAXMAG(st) = rval
		newlum = YES
	    }

	case STCMD_LFILE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], ST_LFILE(st))) {
		call printf ("lfile: %s\n")
		    call pargstr (ST_LFILE(st))
	    } else {
		if (lf != NULL) {
		    call close (lf)
		    lf = NULL
		}
		iferr {
		    lf = open (Memc[cmd], READ_ONLY, TEXT_FILE)
		} then {
		    lf = NULL
		    call erract (EA_WARN)
		    call strcpy ("", ST_LFILE(st), SZ_FNAME)
		    call printf (
		        "Luminosity function file is undefined.\n")
		} else {
		    call strcpy (Memc[cmd], ST_LFILE(st), SZ_FNAME)
		    newlum = YES
		}
	    }


	case STCMD_EGALMIX:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("egalmix = %g\n")
		    call pargr (ST_EGALMIX(st))
	    } else {
		ST_EGALMIX(st) = rval
		newmix = YES
	    }

	case STCMD_ERADIUS:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("eradius = %g\n")
		    call pargr (ST_ERADIUS(st))
	    } else {
		ST_ERADIUS(st) = rval
		newaxis = YES
	    }

	case STCMD_SRADIUS:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("sradius = %g\n")
		    call pargr (ST_SRADIUS(st))
	    } else {
		ST_SRADIUS(st) = rval
		newaxis = YES
	    }

	case STCMD_AR:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("ar = %g\n")
		    call pargr (ST_AR(st))
	    } else {
		ST_AR(st) = rval
		newround = YES
	    }

	case STCMD_Z:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("z = %g\n")
		    call pargr (ST_Z(st))
	    } else {
		ST_Z(st) = rval
		newaxis = YES
	    }

	case STCMD_ABSORPTION:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("absorption = %g\n")
		    call pargr (ST_ABSORPTION(st))
	    } else {
		ST_ABSORPTION(st) = rval
		newround = YES
	    }

	case STCMD_NGALS:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("ngals = %d\n")
		    call pargi (ST_NSTARS(st))
	    } else {
		ST_NSTARS(st) = ival
		newlum = YES
		newspace = YES
		newmix = YES
		newaxis = YES
		newround = YES
		newphi = YES
	    }

	case STCMD_NSSAMPLE:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nssample = %d\n")
		    call pargi (ST_NSSAMPLE(st))
	    } else {
		ST_NSSAMPLE(st) = ival
		newspace = YES
	    }

	case STCMD_SORDER:
	    call gargl (lval)
	    if (nscan() == 1) {
		call printf ("sorder = %d\n")
		    call pargl (ST_SORDER(st))
	    } else {
		ST_SORDER(st) = lval
		newspace = YES
	    }

	case STCMD_SSEED:
	    call gargl (lval)
	    if (nscan() == 1) {
		call printf ("sseed = %d\n")
		    call pargl (ST_SSEED(st))
	    } else {
		ST_SSEED(st) = lval
		newspace = YES
	    }

	case STCMD_NLSAMPLE:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nlsample = %d\n")
		    call pargi (ST_NLSAMPLE(st))
	    } else {
		ST_NLSAMPLE(st) = ival
		newlum = YES
	    }

	case STCMD_LORDER:
	    call gargl (lval)
	    if (nscan() == 1) {
		call printf ("lorder = %d\n")
		    call pargl (ST_LORDER(st))
	    } else {
		ST_LORDER(st) = lval
		newlum = YES
	    }

	case STCMD_LSEED:
	    call gargl (lval)
	    if (nscan() == 1) {
		call printf ("lseed = %d\n")
		    call pargl (ST_LSEED(st))
	    } else {
		ST_LSEED(st) = lval
		newlum = YES
	    }

	case STCMD_RBINSIZE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("rbinsize = %g\n")
		    call pargr (ST_RBINSIZE(st))
	    } else {
		ST_RBINSIZE(st) = rval
		newplot = YES
	    }

	case STCMD_MBINSIZE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("mbinsize = %g\n")
		    call pargr (ST_MBINSIZE(st))
	    } else {
		ST_MBINSIZE(st) = rval
		newplot = YES
	    }

	case STCMD_DBINSIZE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("dbinsize = %g\n")
		    call pargr (ST_DBINSIZE(st))
	    } else {
		ST_DBINSIZE(st) = rval
		newplot = YES
	    }

	case STCMD_EBINSIZE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("ebinsize = %g\n")
		    call pargr (ST_EBINSIZE(st))
	    } else {
		ST_EBINSIZE(st) = rval
		newplot = YES
	    }

	case STCMD_PBINSIZE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("pbinsize = %g\n")
		    call pargr (ST_PBINSIZE(st))
	    } else {
		ST_PBINSIZE(st) = rval
		newplot = YES
	    }

	default:
	    call printf ("\7\n")
	}

	call sfree (sp)
end
