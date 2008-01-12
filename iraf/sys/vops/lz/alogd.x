# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ALOG -- Compute the logarithm to the base 10 of a vector (generic).  If the
# logarithm is undefined (x <= 0) a user supplied function is called to get
# the function value.

procedure alogd (a, b, npix, errfcn)

double	a[ARB], b[ARB]
int	npix, i
extern	errfcn()
double	errfcn()
errchk	errfcn

begin
	do i = 1, npix {
		if (a[i] <= 0.0D0)
		    b[i] = errfcn (a[i])
		else {
		    # Note Fortran standard forbids log10(cplx).
			b[i] = log10 (a[i])
		}
	}
end
