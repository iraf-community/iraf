include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

define PDM_KEYWORDS "|show|vshow|minp|maxp|minf|maxf|ntheta|sample|signif\
		     |ampep|phase|unreject|alldata|origdata|"

define	SHOW		1	# Show parameter settings
define	VSHOW		2	# Show verbose information
define	PMIN		3	# Set min search period
define	PMAX		4	# Set max search period
define	FMIN		5	# Set min search frequency
define	FMAX		6	# Set max search frequency
define	NTHETA		7	# Set number of points for theta
define	SAMPLE		8	# Set/show the sample ranges
define	SIGNIF		9	# Find theta significance
define	AMPEP		10	# Amplitude and Epoch
define	PHASE		11	# Graph phase curve
define	UNREJECT	12	# Unreject all the rejected data points
define	ALLDATA		13	# Reset range to entire dataset
define	ORIGDATA	14	# Reset data to origional dataset

define	SLOWTHRESH	500	# Threshold above which theta calc gets slow

# PDM_COLON -- Decode colon commands.

procedure pdm_colon (pdmp, cmdstr, ptype, infile, period, flip)

pointer	pdmp				# PDM structure pointer
char	cmdstr[ARB]			# Command string
int	ptype				# plot type
char	infile[SZ_FNAME]		# input file name
double	period				# current working period
bool	flip				# flip the y-axis scale

int	nscan(), strdic()
int	itemp, i
double	temp, p1, signif, pdm_signif()
bool	verbose
pointer	cmd, sp
errchk	pdm_signif, pdm_ampep, pdm_phase

string	keywords PDM_KEYWORDS

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	# Unpack keyword from string, look up command in dictionary.
	# Switch on command.  Call the appropriate subroutines.

	switch (strdic (Memc[cmd], Memc[cmd], SZ_FNAME, keywords)) {
	case SHOW:
	    # Show parameter settings.
	    call gargstr (Memc[cmd], SZ_LINE)
	    verbose = false
	    if (Memc[cmd] == EOS) {
		call gdeactivate (PDM_GP(pdmp), AW_CLEAR)
		iferr (call pdm_show (pdmp, "STDOUT", verbose))
		    call erract (EA_WARN)
		call greactivate (PDM_GP(pdmp), AW_PAUSE)
	    } else {
		iferr (call pdm_show (pdmp, Memc[cmd], verbose))
		    call erract (EA_WARN)
	    }
	case VSHOW:
	    # Show verbose information.
	    call gargstr (Memc[cmd], SZ_LINE)
	    verbose = true
	    if (Memc[cmd] == EOS) {
		call gdeactivate (PDM_GP(pdmp), AW_CLEAR)
		iferr (call pdm_show (pdmp, "STDOUT", verbose))
		    call erract (EA_WARN)
		call greactivate (PDM_GP(pdmp), AW_PAUSE)
	    } else {
		iferr (call pdm_show (pdmp, Memc[cmd], verbose))
		    call erract (EA_WARN)
	    }
	case PMIN:
	    # List or set minimum period.
	    call gargd (temp)
	    if (nscan() == 2) {
		# Set the period minimum in structure.
		PDM_PMIN(pdmp) = temp
		if (temp >= EPSILOND)
		    PDM_FMAX(pdmp) = 1.0d+0/temp
		# Save this minp out to the parameter.
		call clputd ("minp", temp)
	    } else {
		# Print out the period minimum from the structure.
		call printf ("Current minimum period = %g\n")
		    call pargd (PDM_PMIN(pdmp))
		call flush (STDOUT)
	    }
	case PMAX:
	    # List or set maximum period.
	    call gargd (temp)
	    if (nscan() == 2) {
		# Set the period maximum in structure.
		PDM_PMAX(pdmp) = temp
		if (temp >= EPSILOND)
		    PDM_FMIN(pdmp) = 1.0d+0/temp
		# Save this minp out to the parameter.
		call clputd ("maxp", temp)
	    } else {
		# Print out the period maximum from the structure.
		call printf ("Current maximum period = %g\n")
		    call pargd (PDM_PMAX(pdmp))
		call flush (STDOUT)
	    }
	case FMIN:
	    # List or set minimum frequency.
	    call gargd (temp)
	    if (nscan() == 2) {
		# Set the frequency minimum in structure.
		PDM_FMIN(pdmp) = temp
		if (temp >= EPSILOND)
		    PDM_PMAX(pdmp) = 1.0d+0/temp
		# Save this minp out to the parameter.
		if (temp >= EPSILOND)
		    call clputd ("maxp", 1.0d+0/temp)
	    } else {
		# Print out the frequency minimum from the structure.
		call printf ("Current minimum frequency = %g\n")
		    call pargd (PDM_FMIN(pdmp))
		call flush (STDOUT)
	    }
	case FMAX:
	    # List or set maximum frequency.
	    call gargd (temp)
	    if (nscan() == 2) {
		# Set the frequency maximum in structure.
		PDM_FMAX(pdmp) = temp
		if (temp >= EPSILOND)
		    PDM_PMIN(pdmp) = 1.0d+0/temp
		# Save this minp out to the parameter.
		if (temp >= EPSILOND)
		    call clputd ("minp", 1.0d+0/temp)
	    } else {
		# Print out the frequency maximum from the structure.
		call printf ("Current maximum frequency = %g\n")
		    call pargd (PDM_FMAX(pdmp))
		call flush (STDOUT)
	    }
	case NTHETA:
	    # Set/show number of theta points
	    call gargi (itemp)
	    if (nscan() == 2) {
		# Set ntheta in structure.
		PDM_NTHPT(pdmp) = itemp
		if (itemp > SLOWTHRESH)
		    # Give message saying that with this ntheta, the
		    # theta calculation will take quite a while.
		    call printf ("Large ntheta => long calculation time \007\n")
	    } else {
		# Print out the value of ntheta from the structure.
		call printf ("Number of theta points = %g\n")
		    call pargi (PDM_NTHPT(pdmp))
		call flush (STDOUT)
	    }
	case SAMPLE:
	    # List or set the sample points.
	    call gargstr (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
	        call printf ("sample = %s\n")
		    call pargstr (PDM_SAMPLE(pdmp))
	    } else {
		if (ptype == DATAPLOT) {
		    call rg_gxmarkd (PDM_GP(pdmp), PDM_SAMPLE(pdmp),
			PDM_X(pdmp,1), PDM_NPT(pdmp), 0)
		}
		call strcpy (Memc[cmd], PDM_SAMPLE(pdmp), SZ_LINE)
		if (ptype == DATAPLOT) {
		    call rg_gxmarkd (PDM_GP(pdmp), PDM_SAMPLE(pdmp),
			PDM_X(pdmp,1), PDM_NPT(pdmp), 1)
		}
	    }
	case SIGNIF:
	    # Calculate the significance of theta at period.
	    p1 = 0.0
	    call gargd (temp)
	    if (nscan() == 2)		# User entered a period.
		p1 = temp
	    else {
		# Use remembered period.
		if (PDM_MINR(pdmp) >= EPSILOND)
		    p1 = PDM_MINR(pdmp)
		else {
		    call printf ("No remembered minimum period. \007\n")
		    call flush (STDOUT)
		}
	    }
	    # Calculate significance at cursor x position (per).
	    if (p1 >= EPSILOND) {
	        signif = pdm_signif (pdmp, p1)
	        # Print at bottom of screen.
	        call printf ("Significance at period %g = %g\n")
		    call pargd (p1)
		    call pargd (signif)
	        call flush (STDOUT)
	    }
	case AMPEP:
	    # Calculate the amplitude and epoch for the data.
	    p1 = 0.0
	    call gargd (temp)
	    if (nscan() == 2)		# User entered a period.
		p1 = temp
	    else {
		# Use remembered period.
		if (PDM_MINR(pdmp) >= EPSILOND)
		    p1 = PDM_MINR(pdmp)
		else {
		    call printf ("No remembered minimum period. \007\n")
		    call flush (STDOUT)
		}
	    }
	    # Calculate ampl & epoch at p1.
	    if (p1 >= EPSILOND) {
	        call pdm_ampep (pdmp, p1)
	        # Print at bottom of screen.
	        call printf("amplitude of data at period %g = %g, epoch = %g\n")
		    call pargd (p1)
		    call pargd (PDM_AMPL(pdmp))
		    call pargd (PDM_EPOCH(pdmp))
	        call flush (STDOUT)
	    }
	case PHASE:
	    # Phase curve plot.
	    call gargd (temp)
	    if (nscan() == 2) {
		# Calculate the phase curve, then make the plot.
		call pdm_ampep (pdmp, temp)
		call pdm_phase(pdmp, temp, PDM_EPOCH(pdmp))
		call pdm_pplot (pdmp, temp, infile, flip)
		period = temp
	    } else {
		# Use remembered period.
		if (PDM_MINR(pdmp) >= EPSILOND) {
		    call pdm_ampep (pdmp, PDM_MINR(pdmp))
		    call pdm_phase(pdmp, PDM_MINR(pdmp), PDM_EPOCH(pdmp))
		    call pdm_pplot (pdmp, PDM_MINR(pdmp), infile, flip)
		} else {
		    call printf ("No remembered minimum period. \007\n")
		    call flush (STDOUT)
		}
	    }
	    ptype = PHASEPLOT
	case UNREJECT:
	    # Copy original data vector into working data vector and
	    # set inuse array to all ones.

	    do i = 1, PDM_NPT(pdmp) {
	        PDM_INUSE(pdmp,i) = 1
	        PDM_DY(pdmp,i) = PDM_ODY(pdmp,i)
	    }
	    PDM_RESID(pdmp) = NO

	    # Check type of plot and replot.
	    if (ptype == DATAPLOT)
		call pdm_dplot (pdmp, infile, flip)
	    if (ptype == PHASEPLOT)
		call pdm_pplot (pdmp, period, infile, flip)
	case ALLDATA:
	    # Initialize the sample string and erase from the graph.
	    if (ptype == DATAPLOT) {
		call rg_gxmarkd (PDM_GP(pdmp), PDM_SAMPLE(pdmp),
		    PDM_X(pdmp,1), PDM_NPT(pdmp), 0)
	    }
	    call strcpy ("*", PDM_SAMPLE(pdmp), SZ_LINE)
	case ORIGDATA:
	    # Copy the original data vector into the working data vector.
	    do i = 1, PDM_NPT(pdmp)
		PDM_DY(pdmp,i) = PDM_ODY(pdmp,i)
	    PDM_RESID(pdmp) = NO

	    # Replot data.
	    call pdm_dplot (pdmp, infile, flip)
	    ptype = DATAPLOT
	default:
	    # Error, unknown colon command; ring bell.
	    call printf ("\007")
	    call printf ("v/show minp/f maxp/f ntheta sample ")
	    call printf ("signif ampep phase unreject all/origdata\n")
	}

	call sfree (sp)
end
