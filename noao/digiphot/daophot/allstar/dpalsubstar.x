include "../lib/daophotdef.h"

# DP_ALSUBSTAR -- Subtract a single star.

procedure dp_alsubstar (wtim, wnx, wny, wxoff, wyoff, stim, snx, sny, sxoff,
        syoff, lx, ly, nx, ny, xcen, ycen, mag, varpsf, psfradsq, psffit)

real	wtim[wnx, wny]			# weight array
int	wnx, wny			# dimensions of the wtim array
int	wxoff, wyoff			# lower left corner offsets
real	stim[snx,sny]			# subtracted image array
int	snx, sny			# dimensions of the stim array
int	sxoff, syoff			# lower left corner offsets
int	lx, ly				# lower left corner coordinates
int	nx, ny				# number of points in x and y
real	xcen, ycen			# x and y centers
real	mag				# magnitude
int	varpsf				# variable psf
real	psfradsq			# psf radius squared
pointer	psffit				# pointer to the psf structure

int	si, sj, wxdiff, wydiff
real	deltax, deltay, dx, dy, dysq, dvdx, dvdy
real	dp_evalpsf()

begin
	wydiff = syoff - wyoff
	wxdiff = sxoff - wxoff
	deltax = xcen - DP_XPSF(psffit)
	deltay = ycen - DP_YPSF(psffit)

	do sj = ly - syoff + 1, ly - syoff + ny {
	    dy = (sj + syoff - 1) - ycen
	    dysq = dy * dy
	    do si = lx - sxoff + 1, lx - sxoff + nx {
		if (IS_INDEFR(wtim[si+wxdiff,sj+wydiff]))
		    next
		if (wtim[si+wxdiff,sj+wydiff] <= 0.0)
		    next
		dx = (si + sxoff - 1) - xcen
		if ((dx * dx + dysq) > psfradsq)
		    next
		stim[si,sj] = stim[si,sj] - mag * dp_evalpsf (dx, dy,
		    psffit, deltax, deltay, varpsf, dvdx, dvdy)
	    }
	}
end
