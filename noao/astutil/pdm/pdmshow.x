include <mach.h>
include <ctype.h>
include <error.h>
include "pdm.h"

# PDM_SHOW -- Print information to file.

procedure pdm_show (pdmp, file, verbose)

pointer	pdmp			# pointer to PDM data structure
char	file[ARB]		# file to put the show information in
bool	verbose			# verbose output flag

int	fd, open(), i
errchk	open()

begin
	# Open the output file.
	fd = open (file, APPEND, TEXT_FILE)

	# Print information from the data structure.
	call fprintf (fd,
		    "minimum period searched = %12.12g, maximum = %g12.12\n")
	    call pargd (PDM_PMIN(pdmp))
	    call pargd (PDM_PMAX(pdmp))
	call fprintf (fd,
		    "period = %12.12g, amplitude = %12.12g, epoch = %12.12g\n")
	    call pargd (PDM_MINR(pdmp))
	    call pargd (PDM_AMPL(pdmp))
	    call pargd (PDM_EPOCH(pdmp))

	if (verbose) {
	    # Print the working data set out as x,y,in-use triplets.
	    call fprintf (fd, "The working data vector is as follows: \n")
	    do i = 1, PDM_NPT(pdmp) {
		call fprintf ( fd, "index = %d, x = %12.12g, y = %12.12g\n")
		    call pargi (i)
		    call pargd (PDM_X(pdmp,i))
		    call pargd (PDM_DY(pdmp,i))
	    }

	    if (PDM_XPHP(pdmp) != NULL) {
	        # Print the phasecurve out as x,y pairs
	        call fprintf (fd, "\nThe phase curve vector is as follows: \n")
	        do i = 1, PDM_NPT(pdmp) {
		    call fprintf ( fd, "index = %d, x = %12.12g, y = %12.12g\n")
		        call pargi (i)
		        call pargd (PDM_XPH(pdmp,i))
		        call pargd (PDM_YPH(pdmp,i))
	        }
	    }
	}

	# Close the output file.
	call close (fd)
end
