include <mach.h>
include <ctype.h>
include <error.h>
include "pdm.h"

# PDM_BATCH -- Batch mode calculation.

procedure pdm_batch (pdmp, file, infile, flip)

pointer	pdmp				# PDM structure pointer
char	file[SZ_FNAME]			# file to store information
char	infile[SZ_FNAME]		# input data file name
bool	flip				# flip y-axis scale

int	fd
double	pdm_signif(), signif
bool	verbose
errchk	pdm_alltheta, pdm_signif, pdm_ampep, pdm_phase

begin
	# Plot the data.
	call pdm_dplot (pdmp, infile, flip)

	# Call pdm_alltheta to get the theta array; plot to metafile.
	call pdm_alltheta (pdmp, THETAPPLOT)
	call pdm_tplot (pdmp, THETAPPLOT, infile)

	# Call pdm_signif to calculate the significance of the theta minimum.
	signif = pdm_signif (pdmp, PDM_MINR(pdmp))

	# Write this significance out to the file.
	fd = PDM_LFD(pdmp)
	call fprintf (fd, "significance at this minimum = %g\n")
	    call pargd (signif)
	call close (fd)

	# Call pdm_amplitudeepoch to get the amplitude and epoch.
	call pdm_ampep (pdmp, PDM_MINR(pdmp))

	# Call pdm_phase to get the phase curve.
	# Plot to metafile.
	
	call pdm_phase(pdmp, PDM_MINR(pdmp), PDM_EPOCH(pdmp))
	call pdm_pplot (pdmp, PDM_MINR(pdmp), infile, flip)

	# Call pdm_show to print the parameter information to a file.
	verbose = TRUE
	call pdm_show (pdmp, file, verbose)
end
