include <mach.h>
include <ctype.h>
include <error.h>
include "pdm.h"

# PDM -- Phase Dispersion Minimization.  Find periodicities in light
# curve data. Root program.

procedure t_pdm()

char	infile[SZ_FNAME]		# input data file
char	metafile[SZ_FNAME]		# batch metafile
char	batchfile[SZ_FNAME]		# batch log file
char	device[SZ_FNAME]		# plot device
bool	interactive			# interactive or batch flag
bool	autoranges			# autoranges flag

int	ptype				# plot type
pointer	pdmp, pdm_open()		# structure stuff
bool	flip, clgetb()
real	clgetr()
int	clgeti(), pdm_gdata(), pdm_autorang()
errchk	pdm_open, pdm_autorang, pdm_batch, pdm_cursor

begin
	# Get some of the CL parameters and open the pdm data structure.
	call clgstr ("infiles", infile, SZ_FNAME)
	call clgstr ("metafile", metafile, SZ_FNAME)
	call clgstr ("batchfile", batchfile, SZ_FNAME)
	call clgstr ("device", device, SZ_FNAME)
	interactive = clgetb ("interactive")
	flip = clgetb ("flip")
	pdmp = pdm_open (device, batchfile, metafile, interactive)

	# Clio to get more parameters.
	PDM_PMIN(pdmp) = clgetr ("minp")
	call clputr ("minp", PDM_PMIN(pdmp))
	if (PDM_PMIN(pdmp) > EPSILONR)
	    PDM_FMAX(pdmp) = 1./PDM_PMIN(pdmp)
	else
	    PDM_FMAX(pdmp) = 0.0
	PDM_PMAX(pdmp) = clgetr ("maxp")
	call clputr ("maxp", PDM_PMAX(pdmp))
	if (PDM_PMAX(pdmp) > EPSILONR)
	    PDM_FMIN(pdmp) = 1./PDM_PMAX(pdmp)
	else
	    PDM_FMIN(pdmp) = 0.0
	PDM_NTHPT(pdmp) = clgeti ("ntheta")
	autoranges = clgetb ("autoranges")
	PDM_NSIGMA(pdmp) = clgeti ("nsigma")
	PDM_DEBUG(pdmp) = clgetb ("debug")
	PDM_PLUSPOINT(pdmp) = clgeti ("pluspoint")

	# Read in the data.
	PDM_NPT(pdmp) = pdm_gdata (pdmp, infile)

	# If the autoranges flag is set, call the autorange subroutine.
	if (autoranges)
	    PDM_NRANGE(pdmp) = pdm_autorang (pdmp)

	if (!interactive)
	    call pdm_batch (pdmp, batchfile, infile, flip)
	else {
	    # Plot the data on the screen and call the cursor loop. 
	    call pdm_dplot (pdmp, infile, flip)
	    ptype = DATAPLOT
	    call pdm_cursor(pdmp, ptype, infile, flip)
	}

	# Close the pdm data structure.
	call pdm_close (pdmp, interactive)
end
