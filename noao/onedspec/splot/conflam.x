define	VLIGHT		2.997925e18

# CONFLAM -- Convert to FLAMBDA from FNU

procedure conflam (pix, w0, wpc, npts)

real	pix[ARB], w0, wpc
int	npts

int	i
real	w

begin
	if (IS_INDEF(w0) || IS_INDEF(wpc))
	    return

	do i = 1, npts {
	    w = w0 + (i-1) * wpc
	    if (w != 0.0)
	        pix[i] = pix[i] * VLIGHT / w**2
	}
end
