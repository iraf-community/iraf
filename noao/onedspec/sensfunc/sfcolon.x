include	<error.h>
include	<gset.h>
include	"sensfunc.h"

# SENSFUNC colon commands
define	CMDS	"|stats|vstats|function|order|graphs|images|skys|marks\
		 |fluxlimits|colors|"
define	STATS		1	# Show results
define	VSTATS		2	# Show verbose results
define	FUNCTION	3	# Sensitivity function type
define	ORDER		4	# Function order
define	GRAPHS		5	# Select graphs
define	IMAGES		6	# Select images
define	SKYS		7	# Select skys
define	MARKS		8	# Set graph mark types
define	FLIMITS		9	# Flux graph limits
define	COLORS		10	# Flux graph limits

# SF_COLON -- Process SENSFUNC colon commands.
# This procedure has so many arguments because of the STATS option.

procedure sf_colon (cmd, gp, stds, nstds, cv, wextn, extn, nextn, ecv, function,
    order, npts, rms, newfit, newgraph)

char	cmd[ARB]		# Colon command
pointer	gp			# Graphics structure
pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
pointer	cv			# Sensitivity function curve
real	wextn[nextn]		# Extinction table wavelengths
real	extn[nextn]		# Extinction table values
int	nextn			# Number of extinction table values
pointer	ecv			# Residual extinction curve
char	function[ARB]		# Function type
int	order			# Function order
int	npts			# Number of points in fit
real	rms			# RMS in fit
int	newfit			# New function?
int	newgraph		# New graphs?

int	i, j, ncmd, ival, fd, nscan(), strdic(), open(), stridx()
real	rval1, rval2
bool	streq()
pointer	sp, str
errchk	open

begin
	# Match the command against a dictionary.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call sscan (cmd)
	call gargwrd (Memc[str], SZ_LINE)
	ncmd = strdic (Memc[str], Memc[str], SZ_LINE, CMDS)

	# Switch on the command.
	switch (ncmd) {
	case STATS:
	    call gargwrd (Memc[str], SZ_LINE)
	    iferr {
		# If no argument write to STDOUT otherwise append to file.
	        if (nscan() == 1) {
		    call gdeactivate (GP_GIO(gp), AW_CLEAR)
		    call sf_stats (STDOUT, stds, nstds, function, order, npts,
			rms)
		    call greactivate (GP_GIO(gp), AW_PAUSE)
	        } else {
		    fd = open (Memc[str], APPEND, TEXT_FILE)
		    call sf_stats (fd, stds, nstds, function, order, npts, rms)
		    call close (fd)
	        }
	    } then
		call erract (EA_WARN)
	case VSTATS:
	    call gargwrd (Memc[str], SZ_LINE)
	    iferr {
	        if (nscan() == 1) {
		    # If no argument page on STDOUT otherwise append to file.
		    # A temp file is used in order to page output.

		    call mktemp ("tmp$sf", Memc[str], SZ_LINE)
		    fd = open (Memc[str], NEW_FILE, TEXT_FILE)
		    call sf_stats (fd, stds, nstds, function, order, npts, rms)
		    call sf_vstats (fd, stds, nstds, cv, wextn, extn, nextn,
			ecv)
		    call close (fd)
		    call gpagefile (GP_GIO(gp), Memc[str], "sensfunc")
		    call delete (Memc[str])
	        } else {
		    fd = open (Memc[str], APPEND, TEXT_FILE)
		    call sf_stats (fd, stds, nstds, function, order, npts, rms)
		    call sf_vstats (fd, stds, nstds, cv, wextn, extn, nextn,
			ecv)
		    call close (fd)
		}
	    } then
		call erract (EA_WARN)
	case FUNCTION:
	    call gargwrd (Memc[str], SZ_LINE)
	    if (nscan() == 2) {
	        call strcpy (Memc[str], function, SZ_FNAME)
		newfit = NO
	    } else {
		call printf ("function %s")
		    call pargstr (function)
	    }
	case ORDER:
	    call gargi (ival)
	    if (nscan() == 2) {
		order = ival
		newfit = NO
	    } else {
		call printf ("order %d")
		    call pargi (order)
	    }
	case GRAPHS:
	    call gargstr (Memc[str], SZ_LINE)
	    j = str
	    for (i=str; Memc[i] != EOS; i=i+1) {
		switch (Memc[i]) {
		case 'a','c','e','i','l','r','s':
		    Memc[j] = Memc[i]
		    j = j + 1
		}
	    }
	    Memc[j] = EOS
	    if (Memc[str] != EOS) {
		call strcpy (Memc[str], GP_GRAPHS(gp,1), SF_NGRAPHS)
		newgraph = YES
	    } else {
		call printf ("graphs %s")
		    call pargstr (GP_GRAPHS(gp,1))
	    }
	case IMAGES:
	    # Note that changing the image automatically clears the sky.
	    do i = 1, SF_NGRAPHS {
	        call gargwrd (Memc[str], SZ_LINE)
		if (nscan() == i + 1) {
		    call strcpy (Memc[str], Memc[GP_IMAGES(gp,i)], SZ_FNAME)
		    Memc[GP_SKYS(gp,i)] = EOS
		    do j = 1, nstds
	    		if (streq (Memc[str], STD_IMAGE(stds[j])))
			    call strcpy (STD_SKY(stds[j]), Memc[GP_SKYS(gp,i)],
				SZ_FNAME)
		} else
		    break
	    }
	    if (nscan() == 1) {
	        call printf ("images %s %s %s %s")
		    call pargstr (Memc[GP_IMAGES(gp,1)])
		    call pargstr (Memc[GP_IMAGES(gp,2)])
		    call pargstr (Memc[GP_IMAGES(gp,3)])
		    call pargstr (Memc[GP_IMAGES(gp,4)])
	    }
	case SKYS:
	    do i = 1, SF_NGRAPHS {
	        call gargwrd (Memc[str], SZ_LINE)
		if (nscan() == i + 1)
		    call strcpy (Memc[str], Memc[GP_SKYS(gp,i)], SZ_FNAME)
		else
		    break
	    }
	    if (nscan() == 1) {
	        call printf ("skys %s %s %s %s")
		    call pargstr (Memc[GP_SKYS(gp,1)])
		    call pargstr (Memc[GP_SKYS(gp,2)])
		    call pargstr (Memc[GP_SKYS(gp,3)])
		    call pargstr (Memc[GP_SKYS(gp,4)])
	    }
	case MARKS:
	    call gargstr (Memc[str], SZ_LINE)
	    call sf_marks (gp, Memc[str])
	case FLIMITS:
	    call gargr (rval1)
	    call gargr (rval2)
	    if (nscan() == 3) {
		GP_FMIN(gp) = rval1
		GP_FMAX(gp) = rval2
		if (stridx (GP_GRAPHS(gp,1), "il") != 0)
		    newgraph = YES
	    } else {
		call printf ("fluxlimits %g %g")
		    call pargr (GP_FMIN(gp))
		    call pargr (GP_FMAX(gp))
	    }
	case COLORS:
	    call gargstr (Memc[str], SZ_LINE)
	    call sf_colors (gp, Memc[str])
	default:
	    call printf ("Unrecognized or ambiguous command\007")
	}

	call sfree (sp)
end
