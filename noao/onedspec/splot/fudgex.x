# FUDGEX -- Fudge an extended region marked by the cursor

procedure fudgex (gfd, pix, npts, x1, x2, dx, wx1, wy1)

int	gfd
real	pix[ARB]
real	x1, x2, dx, wx1, wy1
int	npts

char	command[SZ_FNAME]
int	i, i1, i2, junk, key
real	slope
real	wx2, wy2, wc

int	nplot
real	w1, w2

int	clgcur()

begin
	# Get second point
	call printf ("x again:")
	call flush (STDOUT)

	# Get next position
	junk = clgcur ("cursor", wx2, wy2, wc, key, command, SZ_FNAME)

	# Check out the left-right coords
	call fixx (wx1, wx2, wy1, wy2, x1, x2)

	slope = (wy2-wy1) / (wx2-wx1)

	# Get pixels coordinates
	call pixind (x1, x2, dx, wx1, i1)
	call pixind (x1, x2, dx, wx2, i2)

	# Replace pixels
	do i = i1, i2
	    pix[i] = wy1 + (x1 + (i-1) * dx - wx1) * slope

	# Plot replaced pixels
	nplot = i2 - i1 + 1
	w1 = x1 + (i1-1) * dx
	w2 = x1 + (i2-1) * dx
	call gvline (gfd, pix[i1], nplot, w1, w2)
end
