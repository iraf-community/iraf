include	<gset.h>
include <imhdr.h>
include	<error.h>
include	"apertures.h"

# List of colon commands.
define	CMDS "|show|parameters|database|logfile|plotfile|read|write|image\
	|line|nsum|center|lower|upper|title\
	|extras,b|apidtable,s|b_function,s|b_order,i|b_sample,s\
	|b_naverage,i|b_niterate,i|b_low_reject,r|b_high_reject,r|b_grow,r\
	|minsep,r|maxsep,r|order,s|apertures,s|npeaks,r|shift,b|llimit,r\
	|ulimit,r|ylevel,r|peak,b|bkg,b|r_grow,r|avglimits,b|width,r|radius,r\
	|threshold,r|t_nsum,i|t_step,i|t_width,r|t_function,s|t_order,i\
	|t_sample,s|t_naverage,i|t_niterate,i|t_low_reject,r|t_high_reject,r\
	|t_grow,r|nsubaps,i|background,s|skybox,i|clean,b|saturation,r\
	|weights,s|readnoise,s|gain,s|lsigma,r|usigma,r|t_nlost,i|"

define	SHOW		1	# Show apertures
define	PARAMS		2	# Show parameters
define	DATABASE	3	# Database
define	LOGFILE		4	# Logfile
define	PLOTFILE	5	# Plotfile
define	READ		6	# Read aperture database entry
define	WRITE		7	# Write aperture database entry
define	IMAGE		8	# Image being edited
define	LINE		9	# Set image line to display
define	NSUM		10	# Set number of image lines to sum for display
define	CENTER		11	# Set aperture center
define	LOWER		12	# Set aperture lower limit
define	UPPER		13	# Set aperture upper limit
define	APTITLE		14	# Set aperture title


# AP_COLON -- Process colon commands.  The colon commands may be abbreviated.
# Optional arguments determine either the output or the value of a parameter.
# Changes are signaled to the calling task with the flags NEWGRAPH, NEWIM,
# and NEWDATA.  This task does CLIO including CLCMDW commands.

procedure ap_colon (cmd, im, gp, apdef, aps, naps, current, image, line,
    nsum, all, newgraph, newim, newdata, statline)

char	cmd[ARB]		# Colon command
pointer	im			# IMIO pointer
pointer	gp			# GIO pointer
pointer	apdef			# Default aperture
pointer	aps			# Aperture pointers
int	naps			# Number of apertures
int	current			# Current aperture
char	image[SZ_FNAME]		# Image name
int	line			# Dispersion line
int	nsum			# Number of lines to sum
int	all			# All switch
int	newgraph		# New graph flag
int	newim			# New image flag
int	newdata			# New data flag
int	statline		# Status line used?

bool	bval
int	i, j, ival, apid, apbeam
real	center, low, high, rval
pointer	sp, wrd, str

bool	strne(), apgetb()
real	apgetr()
int	nscan(), strdic(), imaccess(), apgeti(), stridxs()
errchk	ap_apertures, ap_show, ap_params, ap_dbread, ap_dbwrite, ap_openio

define	done_	99

begin
	call smark (sp)
	call salloc (wrd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Scan the command string for the first word which may be abbreviated.
	call sscan (cmd)
	call gargwrd (Memc[wrd], SZ_LINE)
	i = strdic (Memc[wrd], Memc[wrd], SZ_LINE, CMDS)
	if (i == 0) {
	    call printf ("Unrecognized or ambiguous command\007")
	    statline = YES
	    call sfree (sp)
	    return
	}
	j = stridxs (",", Memc[wrd])

	if (j == 0) {
	    switch (i) {
	    case SHOW:	# :show - Show aperture list
	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1) {
		    call gdeactivate (gp, AW_CLEAR)
	            call ap_show ("STDOUT", Memi[aps], naps)
		    call greactivate (gp, AW_PAUSE)
	        } else {
		    iferr (call ap_show (cmd, Memi[aps], naps)) {
		        call erract (EA_WARN)
			statline = YES
		    }
	        }
	    case PARAMS: # :parameters - Show parameters
	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1) {
		    call mktemp ("junk", cmd, SZ_LINE) 
	            iferr (call ap_params (cmd, image, line, nsum)) {
		        call gdeactivate (gp, AW_CLEAR)
	                call ap_params ("STDOUT", image, line, nsum)
		        call greactivate (gp, AW_PAUSE)
		    } else {
		        call gpagefile (gp, cmd, ":parameters")
		        call delete (cmd)
		    }
	        } else {
		    iferr (call ap_params (cmd, image, line, nsum)) {
		        call erract (EA_WARN)
			statline = YES
		    }
	        }
	    case DATABASE: # :database - Database name
	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1) {
		    call clgstr ("database", cmd, SZ_LINE)
		    call printf ("database %s")
		        call pargstr (cmd)
		    statline = YES
	        } else
		    call clpstr ("database", cmd)
	    case LOGFILE: # :logfile - Logfile name
	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1) {
		    call clgstr ("logfile", cmd, SZ_LINE)
		    call printf ("logfile %s")
		        call pargstr (cmd)
		    statline = YES
	        } else
		    call clpstr ("logfile", cmd)
	    case PLOTFILE: # :plotfile - Plotfile name
	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1) {
		    call clgstr ("plotfile", cmd, SZ_LINE)
		    call printf ("plotfile %s")
		        call pargstr (cmd)
		    statline = YES
	        } else
		    call clpstr ("plotfile", cmd)
	    case READ: # :read - Read database entry
	        iferr {
	            call gargwrd (cmd, SZ_LINE)
	            if (nscan() == 1)
		        call ap_dbread (image, aps, naps)
	            else {
		        call xt_stripwhite (cmd)
		        if (cmd[1] == EOS)
		            call ap_dbread (image, aps, naps)
		        else {
		            call ap_dbread (cmd, aps, naps)
			    call appstr ("ansdbwrite1", "yes")
		        }
		    }
	        } then {
		    call erract (EA_WARN)
		    statline = YES
		}
		current = min (1, naps)
	        newgraph = YES
	    case WRITE: # :write - Write database entry
	        iferr {
	            call gargwrd (cmd, SZ_LINE)
	            if (nscan() == 1)
		        call ap_dbwrite (image, aps, naps)
	            else {
		        call xt_stripwhite (cmd)
		        if (cmd[1] == EOS)
		            call ap_dbwrite (image, aps, naps)
		        else {
		            call ap_dbwrite (cmd, aps, naps)
			    call appstr ("ansdbwrite1", "yes")
		        }
		    }
	        } then {
		    call erract (EA_WARN)
		    statline = YES
		}
	    case IMAGE: # :image - Define a new image
	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1) {
		    call printf ("image %s")
		        call pargstr (image)
		    statline = YES
	        } else {
		    call xt_stripwhite (cmd)
		    if ((cmd[1] != EOS) && (strne (cmd, image))) {
		        if (imaccess (cmd, READ_ONLY) == YES)
		            newim = YES
		        else {
			    call eprintf (
			        "WARNING: Can't read image %s")
			        call pargstr (cmd)
			    statline = YES
		        }
		    }
	        }
	    case LINE: # :line - Image line or column
	        call gargi (ival)
	        if (nscan() < 2) {
		    call printf ("line %d")
		        call pargi (line)
		    statline = YES
	        } else if (ival != line) {
		    call strcpy (image, cmd, SZ_LINE)
		    line = ival
		    newdata = YES
	        }
	    case NSUM: # :nsum - Number of image lines or columns to sum
	        call gargi (ival)
	        if (nscan() < 2) {
		    call printf ("nsum %d")
		        call pargi (nsum)
		    statline = YES
	        } else if (ival != nsum) {
		    call strcpy (image, cmd, SZ_LINE)
		    nsum = ival
		    newdata = YES
	        }
	    case CENTER: # :center - Set aperture center
	        if (current == 0)
		    goto done_
	        call gargr (rval)
	        if (nscan() == 1) {
	            call ap_values (current, Memi[aps], line, apid,
			apbeam, center, low, high)
		    call printf ("center %g")
		        call pargr (center)
		    statline = YES
	        } else if (all == NO) {
	            call ap_values (current, Memi[aps], line, apid,
			apbeam, center, low, high)
		    iferr (call ap_update (gp, Memi[aps+current-1], line, apid,
			apbeam, rval, low, high)) {
			call erract (EA_WARN)
			statline = YES
		    }
	        } else {
	            call ap_values (current, Memi[aps], line, apid,
			apbeam, center, low, high)
		    rval = rval - center
		    do i = 1, naps {
			call ap_values (i, Memi[aps], line, apid,
			    apbeam, center, low, high)
			center = center + rval
			iferr (call ap_update (gp, Memi[aps+i-1], line, apid,
			    apbeam, center, low, high)) {
			    call erract (EA_WARN)
			    statline = YES
			}
		    }
	        }
	    case LOWER: # :lower - Set lower aperture limit
	        if (current == 0)
		    goto done_
	        call gargr (rval)
	        if (nscan() == 1) {
	            call ap_values (current, Memi[aps], line, apid,
			apbeam, center, low, high)
		    call printf ("low %g")
		        call pargr (low)
		    statline = YES
	        } else if (all == NO) {
		    call ap_values (current, Memi[aps], line, apid,
			apbeam, center, low, high)
		    iferr (call ap_update (gp, Memi[aps+current-1], line, apid,
			apbeam, center, rval, high)) {
			call erract (EA_WARN)
			statline = YES
		    }
	        } else {
		    do i = 1, naps {
		        call ap_values (i, Memi[aps], line, apid,
			    apbeam, center, low, high)
		        iferr (call ap_update (gp, Memi[aps+i-1], line, apid,
			    apbeam, center, rval, high))
			    call erract (EA_WARN) {
			    statline = YES
			}
		    }
	        }
	    case UPPER: # :upper - Set upper aperture limit
	        if (current == 0)
		    goto done_
	        call gargr (rval)
	        if (nscan() == 1) {
	            call ap_values (current, Memi[aps], line, apid,
			apbeam, center, low, high)
		    call printf ("high %g")
		        call pargr (high)
		    statline = YES
	        } else if (all == NO) {
		    call ap_values (current, Memi[aps], line, apid,
			apbeam, center, low, high)
		    iferr (call ap_update (gp, Memi[aps+current-1], line, apid,
			apbeam, center, low, rval)) {
			call erract (EA_WARN)
			statline = YES
		    }
	        } else {
		    do i = 1, naps {
		        call ap_values (i, Memi[aps], line, apid,
			    apbeam, center, low, high)
		        iferr (call ap_update (gp, Memi[aps+i-1], line, apid,
			    apbeam, center, low, rval)) {
			    call erract (EA_WARN)
			    statline = YES
			}
		    }
	        }
	    case APTITLE:
	        if (current == 0)
		    goto done_
	        call gargwrd (Memc[wrd], SZ_LINE)
	        if (nscan() == 1) {
		    call printf ("title %s")
		        if (AP_TITLE(Memi[aps+current-1]) != NULL)
			    call pargstr (Memc[AP_TITLE(Memi[aps+current-1])])
		        else
			    call pargstr ("[NONE]")
		    statline = YES
	        } else {
		    call reset_scan ()
		    call gargwrd (Memc[str], SZ_LINE)
		    call gargstr (Memc[str], SZ_LINE)
		    if (AP_TITLE(Memi[aps+current-1]) == NULL)
		        call malloc (AP_TITLE(Memi[aps+current-1]), SZ_APTITLE,
			    TY_CHAR)
		    call strcpy (Memc[str+1],
			Memc[AP_TITLE(Memi[aps+current-1])], SZ_APTITLE)
	        }
	    }

	} else {
	    Memc[wrd+j-1] = EOS
	    switch (Memc[wrd+j]) {
	    case 'b':
	        call gargb (bval)
	        if (nscan() < 2) {
		    call printf ("%s %b")
			call pargstr (Memc[wrd])
		        call pargb (apgetb (Memc[wrd]))
		    statline = YES
	        } else
	            call apputb (Memc[wrd], bval)
	    case 'i':
	        call gargi (ival)
	        if (nscan() < 2) {
		    call printf ("%s %d")
			call pargstr (Memc[wrd])
		        call pargi (apgeti (Memc[wrd]))
		    statline = YES
	        } else
	            call apputi (Memc[wrd], ival)
	    case 'r':
	        call gargr (rval)
	        if (nscan() < 2) {
		    call printf ("%s %g")
			call pargstr (Memc[wrd])
		        call pargr (apgetr (Memc[wrd]))
		    statline = YES
	        } else
	            call apputr (Memc[wrd], rval)
	    case 's':
	        call gargwrd (Memc[str], SZ_LINE)
	        if (nscan() < 2) {
		    call apgstr (Memc[wrd], Memc[str], SZ_LINE)
		    call printf ("%s %s")
			call pargstr (Memc[wrd])
		        call pargstr (Memc[str])
		    statline = YES
	        } else
	            call appstr (Memc[wrd], Memc[str])
	    }
	}

done_	call sfree (sp)

end
