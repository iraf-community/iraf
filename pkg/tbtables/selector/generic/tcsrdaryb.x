include	"../tcs.h"

# TCS_RDARY -- Read an array using the column selector

procedure tcs_rdaryb (tp, descrip, irow, maxbuf, nbuf, buffer)

pointer	tp			# i: table descriptor
pointer	descrip			# i: column selector
int	irow			# i: table row number
int	maxbuf			# i: declared length of buffer
int	nbuf			# o: length of output array
bool	buffer[ARB]		# o: array of values
#--
int	idim, ndim, pdim, plen, psize, off
int	axsize, axlen[MAXDIM], axpos[MAXDIM]

int	tbagtb()

begin
	if (TCS_DIMEN(descrip) == 0) {
	    # Column is a scalar, use a scalar read routine

	    if (maxbuf > 0) {
		nbuf = 1
		call tbegtb (tp, TCS_COLUMN(descrip), irow, buffer)
	    } else {
		nbuf = 0
	    }

	} else {
	    # Compute size and dimensionality of the largest contigous
	    # piece that can be read from the array

	    call tbciga (tp, TCS_COLUMN(descrip), ndim, axlen, MAXDIM)

	    pdim = 0
	    psize = 1
	    do idim = 1, TCS_DIMEN(descrip) {
		if (TCS_INC(descrip,idim) > 1)
		    break

		pdim = pdim + 1
		plen = (TCS_LAST(descrip,idim) - TCS_FIRST(descrip,idim) + 1)
		psize = psize * plen

		if (plen < axlen[idim])
		    break
	    }

	    # Compute offset to first element to be read into array

	    off = 0
	    do idim = ndim-1, 1, -1
		off = (off + TCS_FIRST(descrip,idim+1) - 1) * axlen[idim]

	    off = off + TCS_FIRST(descrip,1)

	    # Save position of first element to be read in array

	    do idim = 1 , ndim
		axpos[idim] = TCS_FIRST(descrip,idim)

	    nbuf = 1

	    repeat {

		# Adjust piece size for possible overflow

		if (nbuf + psize > maxbuf)
		    psize = maxbuf - (nbuf - 1)

		# Read chunk from array

		psize = tbagtb (tp, TCS_COLUMN(descrip), irow, 
			      	 buffer[nbuf], off, psize)

		# Exit if array is full

		nbuf = nbuf + psize
		if (nbuf > maxbuf)
		    break

		# Compute offset to next piece to read into array

		axsize = 1
		for (idim = 1; idim <= ndim; idim = idim + 1) {
		    if (idim > pdim) {
			axpos[idim] = axpos[idim] + TCS_INC(descrip,idim)

			if (axpos[idim] + TCS_INC(descrip,idim) <=
			    TCS_LAST(descrip,idim)) {

			    off = off + axsize * TCS_INC(descrip,idim)
			    break

			} else {
			    axpos[idim] = TCS_FIRST(descrip,idim)

			    off = off - axsize * (TCS_LAST(descrip,idim) -
						  TCS_FIRST(descrip,idim))
			}
		    }

		    axsize = axsize * axlen[idim]
		}

		# Exit if array has been traversed

		if (idim > ndim)
		    break
	    }

	    nbuf = nbuf - 1
	}
end

