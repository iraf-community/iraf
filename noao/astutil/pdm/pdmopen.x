include <mach.h>
include "pdm.h"

# PDM_OPEN -- Open a new PDM data structure.

int procedure pdm_open (device, batchfile, metafile, interactive)

char	device[SZ_FNAME]		# graphics device
char	batchfile[SZ_FNAME]		# file to store batch information
char	metafile[SZ_FNAME]		# file to store plots
bool	interactive			# interactive flag

pointer	pdmp
pointer	gt_init(), gopen()
int	open()
errchk	ic_open, gopen, open, calloc, malloc

begin
	# Allocate a pdm data structure.
	call calloc (pdmp, PDM_LENSTRUCT, TY_STRUCT)

	# Set up icfit structure pointers for data and phase curve fits.
	call ic_open (PDM_ICD(pdmp)) 
	call ic_open (PDM_ICP(pdmp)) 

 	# Set up gtools and the gio structure pointers.
	PDM_GT(pdmp) = gt_init ()
	if (interactive) {
	    PDM_GP(pdmp) = gopen (device, NEW_FILE, STDGRAPH)
	    PDM_LFD(pdmp) = open ("STDOUT", APPEND, TEXT_FILE)
	} else {
	    PDM_LFD(pdmp) = open (batchfile, APPEND, TEXT_FILE)
	    PDM_PFD(pdmp) = open (metafile, APPEND, BINARY_FILE)
	    PDM_GP(pdmp) = gopen ("stdvdm", NEW_FILE, PDM_PFD(pdmp))
	}

	# Allocate space for the sample string and put a '*' in it.
	call malloc (PDM_SAMPLEP(pdmp), SZ_LINE, TY_CHAR)
	call strcpy ("*", PDM_SAMPLE(pdmp), SZ_LINE)

	# Booleans.
	PDM_RESID(pdmp) = NO
	PDM_RANGE(pdmp) = YES
	PDM_EB(pdmp) = NO

	return (pdmp)
end
