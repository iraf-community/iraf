# FLATTEN -- Flatten a spectrum and normalize to 1.0

procedure flatten (y, n)

real	y[ARB]
int	n

real	negsig, possig
int	norder, niter
pointer	sp, spec, cont

real 	clgetr()
int	clgeti()

begin
	# Allocate space for continuum fit and current spectrum
	# which gets destroyed by the procedure
	call smark (sp)
	call salloc (cont, n, TY_REAL)
	call salloc (spec, n, TY_REAL)
	call amovr (y, Memr[spec], n)

	# Get values for the iteration parameters

	negsig = clgetr ("negsig")	# Negative going clip level
	possig = clgetr ("possig")	# Positive going clip level
	niter  = clgeti ("niter")	# Number of iterations
	norder = clgeti ("norder")	# Order of fit

	call fcont (Memr[spec], n, norder, negsig, possig, niter, Memr[cont])

	# Divide original spectrum by continuum fit
	call adivr (y, Memr[cont], y, n)

	call sfree (sp)
end


# FCONT  -- Find the CONTinuum level of a spectrum
#
# call fcont (y, n, norder, negsig, possig, niter, cont)
#
# y      -- array containing spectrum (real*4) <-- THIS ARRAY IS DESTROYED
# n      -- length of spectrum (integer*4)
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

procedure fcont (y, n, norder, negsig, possig, niter, cont)

real	y[ARB], negsig, possig, cont[ARB]
int	n, norder, niter

real	coefs[10], blank
real	sum, sum2, spmean, temp, yhi, ylo, sigmay, chisqr, sigma
int	nmin, mode, ierr
int	i, j, k, npts
pointer	sp, work

real	polyno()

data	blank /-9.99e-37/
data	nmin /10/
data	mode /0/

begin
	# Normalize spectrum to 1.0
	sum = 0.0
	do i = 1, n
	    sum = sum + y[i]

	spmean = sum / n
	do i = 1, n
	    y[i] = y[i] / spmean

	# Allocate some work space
	call smark (sp)
	call salloc (work, 500, TY_DOUBLE)

	# Primary iteration loop
	do i = 1, niter {
	    k = 1

	    # Consider only non-'blank' elements
	    # Blanks are set if spectrum is outside sigma bounds.
	    do j = 1, n {
		if (y[j] != blank) {
		    cont[k] = y[j]
		    k = k + 1
		}
	    }

	    npts = k - 1

	    # If a lot of points have been rejected, terminate iteration
	    if (npts < nmin)
		go to 10

	    # Perform polynomial fit
	    # This routine is essentially that of Bevington BUT:
	    # the independent variable is the index of the dependent

	    call polft1 (cont, sigmay, npts, norder+1, mode, coefs, chisqr,
                    Memd[work], ierr)

	    # derive sigma
	    sum = 0.0
	    sum2= 0.0

	    do j = 1, npts {
	        temp = cont[j] - polyno (coefs, norder, j)
	        sum = sum + temp
	        sum2 = sum2 + temp**2
	    }

	    sigma = sqrt ((sum2**2 - 2*sum2*sum/npts + (sum/npts)**2)/(npts-1))

	    ylo = -sigma * negsig
	    yhi =  sigma * possig

	    # Discard bad points
	    do j = 1, n {
	        if (y[j] != blank) {
	    	    temp = y[j] - polyno (coefs, norder, j)
	    	    if(temp > yhi || temp < ylo) 
	    	        y[j] = blank
	        }
	    }
	}

	# Save continuum fit 
10	do i = 1, n {
	    temp = polyno (coefs, norder, i)
	    cont[i] = temp * spmean
	}

	call sfree (sp)
end
