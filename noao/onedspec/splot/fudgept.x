# FUDGEPT -- Fudge a point

procedure fudgept (sh, gfd, x, y, n, wx, wy)

pointer	sh
int	gfd
real	x[n]
real	y[n]
int	n
real	wx, wy

int	i1, nplot, istart
double	shdr_wl()

begin
	# Get pixel number
	i1 = max (1, min (n, nint (shdr_wl (sh, double(wx)))))

	# Replace with Y-value
	if (i1 > 0 && i1 <= n)
	     y[i1] = wy
	else
	    return

	# Plot region around new point
	if (i1 > 1 && i1 < n) {
	    nplot = 3
	    istart = i1 - 1
	} else if (i1 == 1) {
	    nplot = 2
	    istart = i1
	} else if (i1 == n) {
	    nplot = 2
	    istart = n - 1
	}

	call gpline (gfd, x[istart], y[istart], nplot)
end
