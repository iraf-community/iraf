define	VLIGHT		2.997925e18

# CONFNU -- Convert to FNU from FLAMBDA

procedure confnu (pix, w0, wpc, npts)

real	pix[ARB], w0, wpc
int	npts

int	i
real	w

begin
	if (IS_INDEF (w0) || IS_INDEF(wpc))
	    return

	do i = 1, npts {
	    w = w0 + (i-1) * wpc
	    pix[i] = pix[i] * w**2 / VLIGHT
	}
end
