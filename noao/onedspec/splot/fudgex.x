# FUDGEX -- Fudge an extended region marked by the cursor

procedure fudgex (sh, gfd, x, y, n, wx1, wy1, xydraw)

pointer	sh
int	gfd
real	x[n]
real	y[n]
int	n
real	wx1, wy1
int	xydraw

char	command[SZ_FNAME]
int	i, i1, i2, wc, key
real	slope
real	wx2, wy2

int	clgcur()
bool	fp_equalr()

begin
	# Get second point
	call printf ("x again:")
	call flush (STDOUT)
	i = clgcur ("cursor", wx2, wy2, wc, key, command, SZ_FNAME)

	# Fix order
	call fixx (sh, wx1, wx2, wy1, wy2, i1, i2)

	if (xydraw == NO) {
	    wy1 = y[i1]
	    wy2 = y[i2]
	}
	if (fp_equalr (wx1, wx2))
	    slope = 0.
	else
	    slope = (wy2-wy1) / (wx2-wx1)

	# Replace pixels
	do i = i1, i2
	    y[i] = wy1 + (x[i] - wx1) * slope

	# Plot replaced pixels
	i = i2 - i1 + 1
	call gpline (gfd, x[i1], y[i1], i)
end
