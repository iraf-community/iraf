# MRQMIN -- Levenberg-Marquard nonlinear chi square minimization.
# From NUMERICAL RECIPES by Press, Flannery, Teukolsky, and Vetterling, p526.
#
# Levenberg-Marquardt method, attempting to reduce the value of chi
# square of a fit between a set of NDATA points X,Y with individual
# standard deviations SIG, and a nonlinear function dependent on MA
# coefficients A.  The array LISTA numbers the parameters A such that the
# first MFIT elements correspond to values actually being adjusted; the
# remaining MA-MFIT parameters are held fixed at their input value.  The
# program returns the current best-fit values for the MA fit parameters
# A, and chi square, CHISQ.  The arrays COVAR and ALPHA with physical
# dimension NCA (>= MFIT) are used as working space during most
# iterations.  Supply a subroutine FUNCS(X,A,YFIT,DYDA,MA) that evaluates
# the fitting function YFIT, and its derivatives DYDA with respect to the
# fitting parameters A at X.  On the first call provide an initial guess
# for the parameters A, and set ALAMDA<0 for initialization (which then
# sets ALAMDA=0.001).  If a step succeeds  CHISQ becomes smaller and
# ALAMDA decreases by a factor of 10.  If a step fails ALAMDA grows by a
# factor of 10.  You must call this routine repeatedly until convergence
# is achieved.  Then make one final call with ALAMDA = 0, so that COVAR
# returns the covariance matrix, and ALPHA the curvature matrix.
#
# This routine is cast in the IRAF SPP language but the variable names have
# been maintained for reference to the original source.  Also the working
# arrays ATRY, BETA, and DA are allocated dynamically to eliminate
# limitations on the number of parameters fit.

procedure mrqmin (x, y, sig, ndata, a, ma, lista, mfit, covar, alpha, nca,
	chisq, funcs, alamda)

real	x[ndata]		# X data array
real	y[ndata]		# Y data array
real	sig[ndata]		# Sigma array
int	ndata			# Number of data points
real	a[ma]			# Parameter array
int	ma			# Number of parameters
int	lista[ma]		# List array indexing parameters to fit
int	mfit			# Number of parameters to fit
real	covar[nca,nca]		# Covariance matrix
real	alpha[nca,nca]		# Curvature matrix
int	nca			# Matrix dimension (>= mfit)
real	chisq			# Chi square of fit
extern	funcs			# Function to compute derivatives
real	alamda			# Initialization and convergence parameter

int	j, k, kk, ihit
real	ochisq
pointer	atry, beta, da

errchk	gaussj

begin
	# Initialize and check that LISTA contains a proper permutation.
	if (alamda < 0.) {
	    call mfree (atry, TY_REAL)
	    call mfree (beta, TY_REAL)
	    call mfree (da, TY_REAL)
	    call malloc (atry, ma, TY_REAL)
	    call malloc (beta, mfit, TY_REAL)
	    call malloc (da, mfit, TY_REAL)

	    kk = mfit + 1
	    do j = 1, ma {
		ihit = 0
		do k = 1, mfit
		    if (lista(k) == j)
			ihit = ihit + 1
		if (ihit == 0) {
		    lista (kk) = j
		    kk = kk + 1
		} else if (ihit > 1)
		    call error (0, "Improper permutation in LISTA")
	    }
	    if (kk != (ma + 1))
		call error (0, "Improper permutation in LISTA")
	    alamda = 0.001
	    call mrqcof (x, y, sig, ndata, a, ma, lista, mfit, alpha,
		Memr[beta], nca, chisq, funcs)
	    ochisq = chisq
	    do j = 1, ma
		Memr[atry+j-1] = a[j]
	}

	# Alter linearized fitting matrix by augmenting diagonal elements.
	do j = 1, mfit {
	    do k = 1, mfit
		covar[j,k] = alpha[j,k]
	    covar[j,j] = alpha[j,j] * (1. + alamda)
	    Memr[da+j-1] = Memr[beta+j-1]
	}

	# Matrix solution.
	call gaussj (covar, mfit, nca, Memr[da], 1, 1)

	# Once converged evaluate covariance matrix with ALAMDA = 0.
	if (alamda == 0.) {
	    call covsrt (covar, nca, ma, lista, mfit)
	    call mfree (atry, TY_REAL)
	    call mfree (beta, TY_REAL)
	    call mfree (da, TY_REAL)
	    return
	}

	# Did the trial succeed?
	do j = 1, mfit
	    Memr[atry+lista[j]-1] = a[lista[j]] + Memr[da+j-1]
	call mrqcof (x, y, sig, ndata, Memr[atry], ma, lista, mfit, covar,
	    Memr[da], nca, chisq, funcs)

	# Success - accept the new solution, Failure - increase ALAMDA
	if (chisq < ochisq) {
	    alamda = 0.1 * alamda
	    ochisq = chisq
	    do j = 1, mfit {
		do k = 1, mfit
		    alpha[j,k] = covar[j,k]
		Memr[beta+j-1] = Memr[da+j-1]
		a[lista[j]] = Memr[atry+lista[j]-1]
	    }
	} else {
	    alamda = 10. * alamda
	    chisq = ochisq
	}
end


# MRQCOF -- Evaluate linearized matrix coefficients.
# From NUMERICAL RECIPES by Press, Flannery, Teukolsky, and Vetterling, p527.
#
# Used by MRQMIN to evaluate the linearized fitting matrix ALPHA and vector
# BETA.
#
# This procedure has been recast in the IRAF/SPP language but the variable
# names have been maintained.  Dynamic memory is used.

procedure mrqcof (x, y, sig, ndata, a, ma, lista, mfit, alpha, beta, nalp,
    chisq, funcs)

real	x[ndata]		# X data array
real	y[ndata]		# Y data array
real	sig[ndata]		# Sigma array
int	ndata			# Number of data points
real	a[ma]			# Parameter array
int	ma			# Number of parameters
int	lista[ma]		# List array indexing parameters to fit
int	mfit			# Number of parameters to fit
real	alpha[nalp,nalp]	# Work matrix
real	beta[ma]		# Work array
int	nalp			# Matrix dimension (>= mfit)
real	chisq			# Chi square of fit
extern	funcs			# Function to compute derivatives

int	i, j, k
real	sig2i, ymod, dy, wt
pointer	sp, dyda

begin
	call smark (sp)
	call salloc (dyda, ma, TY_REAL)

	do j = 1, mfit {
	   do k = 1, j
	       alpha[j,k] = 0.
	    beta[j] = 0.
	}

	chisq = 0.
	do i = 1, ndata {
	    call funcs (x[i], a, ymod, Memr[dyda], ma)
	    sig2i = 1. / (sig[i] * sig[i])
	    dy = y[i] - ymod
	    do j = 1, mfit {
		wt = Memr[dyda+lista[j]-1] * sig2i
		do k = 1, j
		    alpha[j,k] = alpha[j,k] + wt * Memr[dyda+lista[k]-1]
		beta[j] = beta[j] + dy * wt
	    }
	    chisq = chisq + dy * dy * sig2i
	}

	do j = 2, mfit
	    do k = 1, j-1
		alpha[k,j] = alpha[j,k]

	call sfree (sp)
end
	    

# GAUSSJ -- Linear equation solution by Gauss-Jordan elimination.
# From NUMERICAL RECIPES by Press, Flannery, Teukolsky, and Vetterling, p28.
#
# Linear equation solution by Gauss-Jordan elimination.  A is an input matrix
# of N by N elements, stored in an array of physical dimensions NP by
# NP.  B is an input matrix of N by M containing the M right-hand side
# vectors, stored in an array of physical dimensions NP by MP.  On
# output, A is replaced by its matrix inverse, and B is replaced by the
# corresponding set of solutionn vectors.
#
# This procedure has been recast in the IRAF/SPP language using dynamic
# memory allocation and error return.  The variable names have been maintained.

procedure gaussj (a, n, np, b, m, mp)

real	a[np,np]		# Input matrix and returned inverse
int	n			# Dimension of input matrix
int	np			# Storage dimension of input matrix
real	b[np,mp]		# Input RHS matrix and returned solution
int	m			# Dimension of input matrix
int	mp			# Storage dimension of input matrix

int	i, j, k, l, ll, irow, icol, indxrl, indxcl
real	big, pivinv, dum
pointer	sp, ipiv, indxr, indxc

begin
	call smark (sp)
	call salloc (ipiv,  n, TY_INT)
	call salloc (indxr, n, TY_INT)
	call salloc (indxc, n, TY_INT)

	do j = 1, n
	    Memi[ipiv+j-1] = 0

	do i = 1, n {
	    big = 0.
	    do j = 1, n {
		if (Memi[ipiv+j-1] != 1) {
		    do k = 1, n {
			if (Memi[ipiv+k-1] == 0) {
			    if (abs (a[j,k]) >= big) {
				big = abs (a[j,k])
				irow = j
				icol = k
			    }
			} else if (Memi[ipiv+k-1] > 1) {
			    call sfree (sp)
			    call error (0, "Singular matrix")
			}
		    }
		}
	    }

	    Memi[ipiv+icol-1] = Memi[ipiv+icol-1] + 1

	    if (irow != icol) {
		do l = 1, n {
		    dum = a[irow,l]
		    a[irow,l] = a[icol,l]
		    a[icol,l] = dum
		}
		do l = 1, m {
		    dum = b[irow,l]
		    b[irow,l] = b[icol,l]
		    b[icol,l] = dum
		}
	    }
	    Memi[indxr+i-1] = irow
	    Memi[indxc+i-1] = icol
	    if (a[icol,icol] == 0.) {
		call sfree (sp)
		call error (0, "Singular matrix")
	    }
	    pivinv = 1. / a[icol,icol]
	    a[icol,icol] = 1
	    do l = 1, n
		a[icol,l] = a[icol,l] * pivinv
	    do l = 1, m
		b[icol,l] = b[icol,l] * pivinv
	    do ll = 1, n {
		if (ll != icol) {
		    dum = a[ll,icol]
		    do l = 1, n
			a[ll,l] = a[ll,l] - a[icol,l] * dum
		    do l = 1, m
			b[ll,l] = b[ll,l] - b[icol,l] * dum
		}
	    }
	}

	do l = n, 1, -1 {
	    indxrl = Memi[indxr+l-1]
	    indxcl = Memi[indxr+l-1]
	    if (indxrl != indxcl) {
		do k = 1, n {
		    dum = a[k,indxrl]
		    a[k,indxrl] = a[k,indxcl]
		    a[k,indxcl] = dum
		}
	    }
	}

	call sfree (sp)
end


# COVSRT -- Sort covariance matrix.
# From NUMERICAL RECIPES by Press, Flannery, Teukolsky, and Vetterling, p515.
#
# Given the covariance matrix COVAR of a fit for MFIT of MA total parameters,
# and their ordering LISTA, repack the covariance matrix to the true order of
# the parameters.  Elements associated with fixed parameters will be zero.
# NCVM is the physical dimension of COVAR.
#
# This procedure has been recast into the IRAF/SPP language but the
# original variable names are used.

procedure covsrt (covar, ncvm, ma, lista, mfit)

real	covar[ncvm,ncvm]	# Input and output array
int	ncvm			# Physical dimension of array
int	ma			# Number of parameters
int	lista[mfit]		# Index of fitted parameters
int	mfit			# Number of fitted parameters

int	i, j
real	swap

begin
	# Zero all elements below diagonal.
	do j = 1, ma-1
	    do i = j+1, ma
		covar[i,j] = 0.

	# Repack off-diag elements of fit into correct locations below diag.
	do i = 1, mfit-1
	    do j = i+1, mfit
		if (lista[j] > lista[i])
		    covar [lista[j],lista[i]] = covar[i,j]
		else
		    covar [lista[i],lista[j]] = covar[i,j]

	# Temporarily store original diag elements in top row and zero diag.
	swap = covar[1,1]
	do j = 1, ma {
	    covar[1,j] = covar[j,j]
	    covar[j,j] = 0.
	}
	covar[lista[1],lista[1]] = swap

	# Now sort elements into proper order on diagonal.
	do j = 2, mfit
	    covar[lista[j],lista[j]] = covar[1,j]

	# Finally, fill in above diagonal by symmetry.
	do j = 2, ma
	    do i = 1, j-1
		covar[i,j] = covar[j,i]
end
