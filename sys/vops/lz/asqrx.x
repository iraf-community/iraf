# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ASQR -- Compute the square root of a vector (generic).  If the square root
# is undefined (x < 0) a user supplied function is called to compute the value.

procedure asqrx (a, b, npix, errfcn)

complex	a[ARB], b[ARB]
int	npix, i
extern	errfcn()
complex	errfcn()
errchk	errfcn

begin
	do i = 1, npix {
		{
			b[i] = sqrt (a[i])
		}
	}
end
