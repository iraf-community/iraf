# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

.help
		procedure iif_spline

     This fits uniformly spaced data with a cubic spline.  The spline is
given as basis-spline coefficients that replace the data values.

Storage at call time:

b[1] = second derivative at x = 1
b[2] = first data point y[1]
b[3] = y[2]
.
.
.
b[n+1] = y[n]
b[n+2] = second derivative at x = n

Storage after call:

b[1] ... b[n+2] = the n + 2 basis-spline coefficients for the basis splines
    as defined in P. M. Prenter's book Splines and Variational Methods,
    Wiley, 1975.

.endhelp

procedure iif_spline(b,d,n)

real b[ARB]	# data in and also bspline coefficients out
real d[ARB]     # needed for offdiagnol matrix elements
int n           # number of data points

int i

begin
    
        d[1] = -2.
        b[1] = b[1] / 6.

        d[2] = 0.
        b[2] = (b[2] - b[1]) / 6.

        # Gaussian elimination - diagnol below main is made zero
        # and main diagnol is made all 1's
        do i = 3,n+1 {
            d[i] = 1. / (4. - d[i-1])
            b[i] = d[i] * (b[i] - b[i-1])
        }

        # Find last b spline coefficient first - overlay r.h.s.'s
        b[n+2] = ( (d[n] + 2.) * b[n+1] - b[n] + b[n+2]/6.) / (1. +
            d[n+1] * (d[n] + 2.))

        # back substitute filling in coefficients for b splines
        do i = n+1, 3, -1
            b[i] = b[i]  - d[i] * b[i+1]

        # note b[n+1] is evaluated correctly as can be checked 

        # b[2] is already set since offdiagnol is 0.

        # evaluate b[1]
        b[1] = b[1] + 2. * b[2] - b[3]

	return
end
