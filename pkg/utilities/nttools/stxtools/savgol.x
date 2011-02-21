# Define some memory management.
define	A		Memd[a+((($2)-1)*(m+1))+($1)-1]
define	B		Memd[b+($1)-1]

#---------------------------------------------------------------------------
.help savgol Jun93 source
.ih
NAME
savgol -- Create a kernel for Savitzky-Golay smoothing.
.ih
USAGE
call savgol (c, np, nl, nr, ld, m)
.ih
ARGUMENTS
.ls c (O: double[np])
The smoothing kernel in "wrap-around" order.  See discussion for
details.
.le
.ls np (I: int)
The number of points allocated in the array represented by the "c"
argument.
.le
.ls nl (I: int)
Size of the kernel "to the left" of the central point.  See discussion
for more details.
.le
.ls nr (I: int)
Size of the kernel "to the right" of the central point. See discussion
for more details.
.le
.ls ld (I: int)
Order of the derivative desired.  Should be 0 for smoothing, higher
for smoothed versions of the specified derivative.
.le
.ls m (I: int)
Order of the smoothing polynomial.  Should be 0 or 1 for standard
"boxcar" or "moving window" averaging.
.le
.ih
DISCUSSION
For an introduction to Savitzky-Golay filtering, see:

.nf
	Press, Teukolsky, Vetterling, & Falnnery, "Numeric Recipies:
        The Art of Scientifitc Computing, Second Edition", Cambridge,
	1992.
.fi

This routine returns the set of Savitzky-Golay smoothing coefficients
given the size, order of smoothing polynomial, and derivative to
return.  The coefficients are returned in "wrap-around" order.  Thus,
if the smoothing coefficients are C[-nl]...C[0]...C[nr], they are
returned in the array, c[i], as follows:

.nf
	c[1], c[2],  c[3], ..., c[nl+1],c[nl+2],...,c[np-1],c[np]

	C[0], C[-1], C[-2],..., C[-nl], C[nr],  ...,C[2],   C[1]
.fi

A code fragment to transform the array c[i] to the orginal order,
k[i], is: 

.nf
        do i = 1, nl+1 
            k[i] = c[nl+2-i]
        do i = 1, nr
            k[nl+1+i] = c[np+1-i]
.fi

Array k[i], is now suitable for routines such as the IRAF VOPS routine
acnvrd.
.endhelp
#---------------------------------------------------------------------------
procedure savgol (c, np, nl, nr, ld, m)

double	c[np]			# O:  The kernel.
int	np			# I:  Size of the smoothing kernel.
int	nl			# I:  Points to the left of center.
int	nr			# I:  Points to the right of center.
int	ld			# I:  Order of derivative to return.
int	m			# I:  Order of the smoothing polynomial.

int	imj, ipj, j, k, kk, mm, ix 
double	d, fac, sum
pointer	indx, a, b, sp
int	shifti()

begin
	call smark (sp)
	# Check input parameters.
	if (np < nl+nr+1 || nl < 0 || nr < 0 || ld > m || nl+nr < m)
	    call error (1, "savgol: invalid inputs")

	# Allocate memory.
	call salloc (indx, m+1, TY_INT)
	call salloc (a, (m+1)**2, TY_DOUBLE)
	call salloc (b, m+1, TY_DOUBLE)

	# Do it.
	ipj = shifti (m, 1)
	do ipj = 0, shifti (m, 1) {
	    if (ipj != 0)
		sum = 0.d0
	    else
		sum = 1.d0
	    do k = 1, nr
		sum = sum + k**ipj
	    do k = 1, nl
		sum = sum + (-k)**ipj
	    mm = min (ipj, 2*m-ipj)
	    do imj = -mm, mm, 2
		A(1+(ipj-imj)/2,1+(ipj+imj)/2) = sum
	}
	call ludcmd (Memd[a], m+1, m+1, Memi[indx], d, ix)
	if (ix != OK)
	    call error (1, "savgol: singular matrix")
	do j = 1, m+1
	    B(j) = 0.d0
	B(ld+1) = 1.d0;
	call lubksd (Memd[a], m+1, m+1, Memi[indx], Memd[b])
	do kk = 1, np
	    c[kk] = 0.d0
	do k = -nl, nr {
	    sum = B(1)
	    fac = 1.d0
	    do mm = 1, m {
		fac = fac * k
		sum = sum + B(mm+1) * fac
	    }
	    kk = mod (np - k, np) + 1
	    c[kk] = sum
	}

	# That's all folks.
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of savgol
#---------------------------------------------------------------------------
