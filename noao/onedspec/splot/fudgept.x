# FUDGEPT -- Fudge a point

procedure fudgept (gfd, pix, npts, x1, x2, dx, wx, wy)

int	gfd
real	pix[ARB]
real	x1, x2, dx, wx, wy
int	npts

int	i1, nplot, istart
real	w1, w2

begin
	# Get pixel number
	call pixind (x1, x2, dx, wx, i1)

	# Replace with Y-value
	if (i1 > 0 && i1 <= npts)
	     pix[i1] = wy
	else
	    return

	# Plot region around new point
	if (i1 > 1 && i1 < npts) {
	    w1 = x1 + (i1 - 2) * dx
	    w2 = x1 + (i1    ) * dx
	    nplot = 3
	    istart = i1 - 1
	} else if (i1 == 1) {
	    w1 = x1
	    w2 = x1 + dx
	    nplot = 2
	    istart = i1
	} else if (i1 == npts) {
	    w1 = x1 + (i1   - 1) * dx
	    w2 = x1 + (npts - 1) * dx
	    nplot = 2
	    istart = npts - 1
	}

	call gvline (gfd, pix[istart], nplot, w1, w2)
end
