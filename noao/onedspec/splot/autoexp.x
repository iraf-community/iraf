include	<pkg/gtools.h>

# AUTO_EXP -- Auto expand around the marked region

procedure auto_exp (gp, gt, key, wx1, pix, npix, w1, w2)

pointer	gp		# GIO pointer
pointer	gt		# GTOOLS pointer
int	key		# Key
real	wx1		# Cursor position
real	pix[npix]	# Pixel data for Y scaling
int	npix		# Number of pixels
real	w1, w2		# Wavelength of first and last pixels

char	cmd[1]
int	i1, i2, n, wcs
real	wx2, wy, x1, x2, y1, y2, dx

int	clgcur()

begin
	# Get the current window.
	call ggwind (gp, x1, x2, y1, y2)
	dx = x2 - x1

	# Compute the new window in x.
	switch (key) {
	case 'a':	# Expand
	    call printf ("again:\n")
	    i1 = clgcur ("cursor", wx2, wy, wcs, key, cmd, SZ_LINE)
	    x1 = wx1
	    x2 = wx2
	case ',':	# Shift left
	    x1 = x1 - 0.85 * dx
	    x2 = x2 - 0.85 * dx
	case '.':	# Shift right
	    x1 = x1 + 0.85 * dx
	    x2 = x2 + 0.85 * dx
	case 'z':	# Zoom x axis
	    x1 = x1 + 0.25 * dx
	    x2 = x2 - 0.25 * dx
	}

	# Determine the y limits for pixels between x1 and x2.
	dx = (w2 - w1) / (npix - 1)
	i1 = (x1 - w1) / dx + 1.5
	i2 = (x2 - w1) / dx + 1.5
	i1 = max (1, min (npix, i1))
	i2 = max (1, min (npix, i2))
	if (i1 < i2) {
	    n = i2 - i1 + 1
	    call alimr (pix[i1], n, y1, y2)
	} else {
	    n = i1 - i2 + 1
	    call alimr (pix[i2], n, y1, y2)
	}

	call gclear (gp)
	call gswind (gp, x1, x2, y1, y2)
	call gt_labax (gp, gt)
	call replot (gp, gt, pix, npix, w1, w2, NO)
end
