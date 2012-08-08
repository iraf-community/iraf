# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# II_SPLINE -- This procedure fits uniformly spaced data with a cubic
# spline. The spline is given as basis-spline coefficients that replace
# the data values.
# 
# Storage at call time:
#
# bcoeff[1] = second derivative at x = 1
# bcoeff[2] = first data point y[1]
# bcoeff[3] = y[2]
#
# bcoeff[n+1] = y[n]
# bcoeff[n+2] = second derivative at x = n
#
# Storage after call:
#
# bcoeff[1] ... bcoeff[n+2] = the n + 2 basis-spline coefficients for the
# basis splines as defined in P.M. Prenter's book "Splines and Variational
# Methods", Wiley, 1975.

procedure ii_spline (bcoeff, diag, npts)

real	bcoeff[ARB]	# data in and also bspline coefficients out
real	diag[ARB]	# needed for offdiagnol matrix elements
int	npts		# number of data points

int	i

begin
        diag[1] = -2.
        bcoeff[1] = bcoeff[1] / 6.

        diag[2] = 0.
        bcoeff[2] = (bcoeff[2] - bcoeff[1]) / 6.

        # Gaussian elimination - diagnol below main is made zero
        # and main diagnol is made all 1's
        do i = 3, npts + 1 {
            diag[i] = 1. / (4. - diag[i-1])
            bcoeff[i] = diag[i] * (bcoeff[i] - bcoeff[i-1])
        }

        # Find last b spline coefficient first - overlay r.h.s.'s
        bcoeff[npts+2] = ((diag[npts] + 2.) * bcoeff[npts+1] - bcoeff[npts] +
		 bcoeff[npts+2] / 6.) / (1. + diag[npts+1] * (diag[npts] + 2.))

        # back substitute filling in coefficients for b splines
        # note bcoeff[npts+1] is evaluated correctly as can be checked 
        # bcoeff[2] is already set since offdiagnol is 0.
        do i = npts + 1, 3, -1
            bcoeff[i] = bcoeff[i]  - diag[i] * bcoeff[i+1]

        # evaluate bcoeff[1]
        bcoeff[1] = bcoeff[1] + 2. * bcoeff[2] - bcoeff[3]
end
