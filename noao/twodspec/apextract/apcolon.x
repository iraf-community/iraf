include	<gset.h>
include <imhdr.h>
include	<error.h>
include	<pkg/xtanswer.h>

# List of colon commands.
define	CMDS "|show|apertures|center|lower|upper|read|write|image|line|nsum|\
	|width|radius|threshold|apfind|aptrace|apsum|apstrip|apio|"

define	SHOW		1	# Show parameter values
define	APERTURES	2	# Show list of apertures
define	CENTER		3	# Set aperture center
define	LOWER		4	# Set aperture lower limit
define	UPPER		5	# Set aperture upper limit
define	READ		6	# Read aperture database entry
define	WRITE		7	# Write aperture database entry
define	IMAGE		8	# Set new image to edit
define	LINE		9	# Set image line to display
define	NSUM		10	# Set number of image lines to sum for display
# newline		11
define	WIDTH		12	# Set centering width
define	RADIUS		13	# Set centering radius
define	THRESHOLD	14	# Set centering threshold
define	APFIND		15	# Set parameters for APFIND
define	APTRACE		16	# Set parameters for APTRACE
define	APSUM		17	# Set parameters for APSUM
define	APSTRIP		18	# Set parameters for APSTRIP
define	APIO		19	# Set parameters for APIO


# AP_COLON -- Process colon commands.  The colon commands may be abbreviated.
# Optional arguments determine either the output or the value of a parameter.
# Changes are signaled to the calling task with the flags NEWGRAPH, NEWIM,
# and NEWDATA.  This task does CLIO including CLCMDW commands.

procedure ap_colon (cmd, im, gp, aps, naps, current, image, line, nsum,
    all, newgraph, newim, newdata)

char	cmd[ARB]		# Colon command
pointer	im			# IMIO pointer
pointer	gp			# GIO pointer
pointer	aps[ARB]		# Aperture pointers
int	naps			# Number of apertures
int	current			# Current aperture
char	image[SZ_FNAME]		# Image name
int	line			# Dispersion line
int	nsum			# Number of lines to sum
int	all			# All switch
int	newgraph		# New graph flag
int	newim			# New image flag
int	newdata			# New data flag

char	wrd[SZ_LINE]
int	i, ival, apid, apbeam
real	center, low, high, rval

bool	strne()
real	clgetr()
int	scan(), nscan(), strdic(), imaccess()
errchk	ap_apertures, ap_show, ap_dbread, ap_dbwrite, ap_openio

begin
	# Scan the command string for the first word which may be abbreviated.
	call sscan (cmd)
	call gargwrd (wrd, SZ_LINE)
	i = strdic (wrd, wrd, SZ_LINE, CMDS)

	switch (i) {
	case SHOW: # :show - Show parameters
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call gdeactivate (gp, AW_CLEAR)
	        call ap_show ("STDOUT", image, line, nsum)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr (call ap_show (cmd, image, line, nsum))
		    call erract (EA_WARN)
	    }

	case APERTURES:	# :apertures - Show aperture list
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call gdeactivate (gp, AW_CLEAR)
	        call ap_apertures ("STDOUT", aps, naps)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr (call ap_apertures (cmd, aps, naps))
		    call erract (EA_WARN)
	    }

	case CENTER: # :center - Set aperture center
	    if (current == 0)
		return
	    call gargr (rval)
	    if (nscan() == 1) {
	        call ap_values (current, aps, line, apid, apbeam, center, low,
		    high)
		call printf ("center %g (return to continue)")
		    call pargr (center)
		call flush (STDOUT)
		i = scan ()
	    } else {
	        call ap_values (current, aps, line, apid, apbeam, center, low,
		    high)
		call ap_update (gp, aps[current], line, apid, apbeam, rval,
		    low, high)
	    }

	case LOWER: # :lower - Set lower aperture limit
	    if (current == 0)
		return
	    call gargr (rval)
	    if (nscan() == 1) {
	        call ap_values (current, aps, line, apid, apbeam, center, low,
		    high)
		call printf ("low %g (return to continue)")
		    call pargr (low)
		call flush (STDOUT)
		i = scan()
	    } else if (all == NO) {
		call ap_values (current, aps, line, apid, apbeam, center, low,
		    high)
		call ap_update (gp, aps[current], line, apid, apbeam, center,
		    rval, high)
	    } else {
		do i = 1, naps {
		    call ap_values (i, aps, line, apid, apbeam, center, low,
			high)
		    call ap_update (gp, aps[i], line, apid, apbeam, center,
			rval, high)
		}
	    }

	case UPPER: # :upper - Set upper aperture limit
	    if (current == 0)
		return
	    call gargr (rval)
	    if (nscan() == 1) {
	        call ap_values (current, aps, line, apid, apbeam, center, low,
		    high)
		call printf ("high %g (return to continue)")
		    call pargr (high)
		call flush (STDOUT)
		i = scan()
	    } else if (all == NO) {
		call ap_values (current, aps, line, apid, apbeam, center, low,
		    high)
		call ap_update (gp, aps[current], line, apid, apbeam, center,
		    low, rval)
	    } else {
		do i = 1, naps {
		    call ap_values (i, aps, line, apid, apbeam, center, low,
			high)
		    call ap_update (gp, aps[i], line, apid, apbeam, center,
			low, rval)
		}
	    }

	case READ: # :read - Read database entry
	    iferr {
	        for (i = 1; i <= naps; i = i + 1)
		    call ap_free (aps[i])

	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1)
		    call ap_dbread (image, aps, naps)
	        else {
		    call xt_stripwhite (cmd)
		    if (cmd[1] == EOS)
		        call ap_dbread (image, aps, naps)
		    else
		        call ap_dbread (cmd, aps, naps)
		}
	    } then
		call erract (EA_WARN)

	    current = max (1, naps)
	    newgraph = YES

	case WRITE: # :write - Write database entry
	    iferr {
	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1)
		    call ap_dbwrite (image, ALWAYSYES, aps, naps)
	        else {
		    call xt_stripwhite (cmd)
		    if (cmd[1] == EOS)
		        call ap_dbwrite (image, ALWAYSYES, aps, naps)
		    else
		        call ap_dbwrite (cmd, ALWAYSYES, aps, naps)
		}
	    } then
		call erract (EA_WARN)

	case IMAGE: # :image - Define a new image
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call printf ("image %s (return to continue)")
		    call pargstr (image)
		call flush (STDOUT)
		i = scan()
	    } else {
		call xt_stripwhite (cmd)
		if ((cmd[1] != EOS) && (strne (cmd, image))) {
		    if (imaccess (cmd, READ_ONLY) == YES)
		        newim = YES
		    else {
			call eprintf (
			    "WARNING: Can't read image %s (return to continue)")
			    call pargstr (cmd)
			call flush (STDOUT)
			i = scan()
		    }
		}
	    }

	case LINE: # :line - Image line or column
	    call gargi (ival)
	    if (nscan() < 2) {
		call printf ("line %d (return to continue)")
		    call pargi (line)
		call flush (STDOUT)
		i = scan()
	    } else if (ival != line) {
		call strcpy (image, cmd, SZ_LINE)
		line = ival
		newdata = YES
	    }

	case NSUM: # :nsum - Number of image lines or columns to sum
	    call gargi (ival)
	    if (nscan() < 2) {
		call printf ("nsum %d (return to continue)")
		    call pargi (nsum)
		call flush (STDOUT)
		i = scan()
	    } else if (ival != nsum) {
		call strcpy (image, cmd, SZ_LINE)
		nsum = ival
		newdata = YES
	    }

	case WIDTH: # :width - Width of profile
	    call gargr (rval)
	    if (nscan() < 2) {
		call printf ("width %g (return to continue)")
		    call pargr (clgetr ("apedit.width"))
		call flush (STDOUT)
		i = scan()
	    } else
	        call clputr ("apedit.width", rval)

	case RADIUS: # :radius - Centering radius
	    call gargr (rval)
	    if (nscan() < 2) {
		call printf ("radius %g (return to continue)")
		    call pargr (clgetr ("apedit.radius"))
		call flush (STDOUT)
		i = scan()
	    } else
	        call clputr ("apedit.radius", rval)

	case THRESHOLD: # :threshold - Centering threshold
	    call gargr (rval)
	    if (nscan() < 2) {
		call printf ("threshold %g (return to continue)")
		    call pargr (clgetr ("apedit.threshold"))
		call flush (STDOUT)
		i = scan()
	    } else
	        call clputr ("apedit.threshold", rval)

	case APFIND, APTRACE, APSUM, APSTRIP, APIO:
	    call gdeactivate (gp, 0)
	    switch (i) {
	    case APFIND: # :apfind - Set parameters for APFIND
	        call clcmdw ("eparam apfind")
	    case APTRACE: # :aptrace - Set aptrace parameters
	        call clcmdw ("eparam aptrace")
	        call tr_done()
	        call tr_init()
	    case APSUM: # :apsum - Set apsum parameters
	        call clcmdw ("eparam apsum")
	    case APSTRIP: # :apstrip - Set apstrip parameters
	        call clcmdw ("eparam apstrip")
	    case APIO: # :apio - Set apio parameters
	        call clcmdw ("eparam apio")
	    }
	    newgraph = YES
	    call greactivate (gp, 0)
	}
end
