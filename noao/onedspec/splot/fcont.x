# FCONT  -- Find the CONTinuum level of a spectrum
#
# call fcont (spec, npts, norder, negsig, possig, niter, cont)
#
# spec   -- array containing spectrum (real*4) <-- THIS ARRAY IS DESTROYED
# npts   -- length of spectrum (integer*4)
# norder -- order of analyzing polynomial (integer*4 = 4)
# negsig -- negative going sigma below which points are rejected (real*4 = 1)
# possig -- positive going sigma above which points are rejected (real*4 = 8)
# niter  -- number of rejection iterations (integer*4 = 8)
# cont   -- array of length NPTS to receive the computed continuum (real*4)
#
# Method -- A polynomial of order NORDER (def = 4) is fit to
#           the spectrum. Points further than NEGSIG (def=2) sigma below and
#           POSSIG (def = 4) above the spectrum are deleted, and
#           a new fit is made. The process is iterated NITER (def = 4)
#           times to determine the continuum level. The continuum
#           is derived from a polynomial fit to the remaining points.
#
# G. Jacoby KPNO  26 Apr 84
#
#------------------------------------------------------------------------------

procedure fcont (spec, npts, norder, negsig, possig, niter, cont)

real	spec[ARB], negsig, possig, cont[ARB]
int	npts, norder, niter

real	coefs[10], blank
real	sum, sum2, spmean, temp, yhi, ylo, sigmay, chisqr, sigma
int	nmin, mode, ierr
int	i, j, k, n
pointer	sp, work

real	polyno()

data	blank /-9.99e-37/
data	nmin /10/
data	mode /0/

begin
	# Normalize spectrum to 1.0
	sum = 0.0
	do i = 1, npts
	    sum = sum + spec[i]

	spmean = sum / npts
	do i = 1, npts
	    spec[i] = spec[i] / spmean

	# Allocate some work space
	call smark (sp)
	call salloc (work, 500, TY_DOUBLE)

	# Primary iteration loop
	do i = 1, niter {
	    k = 1

	    # Consider only non-'blank' elements
	    # Blanks are set if spectrum is outside sigma bounds.
	    do j = 1, npts {
		if (spec[j] != blank) {
		    cont[k] = spec[j]
		    k = k + 1
		}
	    }

	    n = k - 1

	    # If a lot of points have been rejected, terminate iteration
	    if (n < nmin)
		go to 10

	    # Perform polynomial fit
	    # This routine is essentially that of Bevington BUT:
	    # the independent variable is the index of the dependent

	    call polft1 (cont, sigmay, n, norder+1, mode, coefs, chisqr,
                    Memd[work], ierr)

	    # derive sigma
	    sum = 0.0
	    sum2= 0.0

	    do j = 1, n {
	        temp = cont[j] - polyno (coefs, norder, j)
	        sum = sum + temp
	        sum2 = sum2 + temp**2
	    }

	    sigma = sqrt ((sum2**2 - 2*sum2*sum/n + (sum/n)**2)/(n-1))

	    ylo = -sigma * negsig
	    yhi =  sigma * possig

	    # Discard bad points
	    do j = 1, npts {
	        if (spec[j] != blank) {
	    	    temp = spec[j] - polyno (coefs, norder, j)
	    	    if(temp > yhi || temp < ylo) 
	    	        spec[j] = blank
	        }
	    }
	}

	# Save continuum fit 
10	do i = 1, npts {
	    temp = polyno (coefs, norder, i)
	    cont[i] = temp * spmean
	}

	call sfree (sp)
end
