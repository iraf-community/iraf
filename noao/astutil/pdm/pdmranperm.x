include <mach.h>
include <ctype.h>
include <error.h>
include	<gset.h>
include <pkg/rg.h>
include "pdm.h"

# PDM_RANPERM -- Make a random permutation of the data vector.
# This is the algorithm:
# Starting at the beginning of the input array,
# Do this numdatapoints times {
#   Find the next random position.
#   Call urand for a random number between one and numdatapoints(hash).
#   Move forward in the input array this number of places (mod numpts).
#   While (marker array for this position has a zero) {
#       Move to the next position (linear rehash).
#   }
#   Put the input array value found at this position into the
#   output array, set the marker array value corresponding
#   to this position to zero.
# }


procedure pdm_ranperm (inarray, inuse, outarray, outinuse, numpts, seed)

int	numpts			# number of points in the data
double	inarray[numpts]		# data to be permuted
double	outarray[numpts]	# output permuted data to this array
int	inuse[numpts]		# the PDM in-use array
int	outinuse[numpts]	# output scrambled in-use array
long	seed			# a seed for the random number generator

int	count, pos
real	urand()
pointer	p, sp			# array to keep track of which have been used

begin
	# Allocate the output array and the marker array.
	# Fill the marker array with ones (amovki)

	call smark (sp)
	call salloc (p, numpts, TY_INT)
	call amovki (1, Memi[p], numpts)	# Set this array to all ones.

	pos = 0
	do count = 1, numpts {
	    pos = mod(pos+int(urand(seed)*numpts)+1, numpts) # Hash.
	    while (Memi[p+pos] == 0)			     # Linear rehash.
		pos = mod(pos+1, numpts)
	    outarray[count] = inarray[pos+1]
	    outinuse[count] = inuse[pos+1]
	    Memi[p+pos] = 0
	}

	call sfree (sp)
end
