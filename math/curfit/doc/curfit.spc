.help curfit May84 "Math Package"
.ce
Specifications for the Curfit Package
.ce
Lindsey Davis
.ce
July 1984

.sh
1. Introduction

The CURFIT package provides a set of routines for fitting data to
functions linear in their coefficients using least
squares techniques. The basic numerical technique employed
is the solution of the normal equations by the Cholesky method.
This document presents the formal requirements for the package
and describes the algorithms used.

.sh
2. Requirements

.ls 4
.ls (1)
The package shall take as input a set of x and y values and their
corresponding weights. The package routines asssume that data values
equal to INDEF have been rejected from the data set or replaced with
appropriate interpolated values prior to entering the package
routines. The input data may be arbitrarily spaced in x. No assumptions
are made about the ordering of the x values, but see (3) below. 
.le
.ls (2)
The package shall perform the following operations:
.ls o
Determine the coefficients of the fitting function by solving the normal
equations. The fitting function is selected at run time from the following
list: (1) LEGENDRE, Legendre polynomials, (2) CHEBYSHEV,
Chebyshev polynomials, (3) SPLINE3, Cubic spline
with uniformly spaced break points, SPLINE1, Linear spline with evenly
spaced break points.  The calling sequence must be
invariant to the form of the fitting function.
.le
.ls o
Set an error code if the numerical routines are unable to fit the
specified function.
.le
.ls o
Output the values of the coefficients.
The coefficients are stored internal to the CURFIT package.
However in some applications it is the coefficients which are of primary
interest. A package routine shall exist to extract the
the coefficients from the curve descriptor structure.
.le
.ls o
Evaluate the fitting function at arbitrary value(s) of x.  The evaluating
routines shall use the coefficients calculated and
the user supplied x value(s).
.le
.ls o
Calculate the standard deviation of the coefficients and the standard deviation
of the fit.
.le
.le
.ls (3)
The program shall perform a weighted fit using a user supplied weight
array and weight flag. The weighting options are WTS_USER, WTS_UNIFORM and
WTS_SPACING. In WTS_USER  mode the package routines apply user supplied
weights to the individual data points, otherwise the package routines
calculate the weights. In WTS_SPACING mode the program assumes that the data
are sorted in x, and sets the individual weights to the difference between
adjacent x values. In WTS_UNIFORM mode the weights are set to 1.
.le
.ls (4)
The input data set and output coefficent, error, and fitted y arrays are single
precision real quantities. All package arithmetic shall be done in single
precision. The package shall however be designed with
conversion to double precision arithmetic in mind.
.le
.le

.sh
3. Specifications

.sh
3.1. List of Routines

The package prefix will be cv for curve fit.
The following procedures shall be part of the package.
Detailed documentation for each procedure can be found by invoking
the help facility.

.nf
        cvinit	(cv, curvetype, order, xmin, xmax)
	cvzero	(cv)
       cvaccum	(cv, x, y, w, wtflag)
      cvreject	(cv, x, y, w)
       cvsolve	(cv, ier)
	 cvfit	(cv, x, y, w, npts, wtflag, ier)
       cvrefit	(cv, x, y, w, ier)
    y = cveval	(cv, x)
      cvvector	(cv, x, yfit, npts)
       cvcoeff	(cv, coeff, ncoeff)
      cverrors	(cv, y, w, yfit, rms, errors)
        cvsave	(cv, fit)
     cvrestore	(cv, fit)
	 cvset	(cv, curve_type, xmin, xmax, coeff, ncoeff)
        cvfree	(cv)
.fi

.sh
3.2. Algorithms

.sh
3.2.1. Polynomial Basis Functions

The approximating function is assumed to be of the form

.nf
    f(x) = a(1)*F(1,x) + a(2)*F(2,x) + ... + a(ncoeff)*F(ncoeff,x)
.fi

where the F(n,x) are polynomial basis functions containing terms
of order x**(n-1), and the a(n) are the coefficients.
In order to avoid a very ill-conditioned linear system for moderate or large n
the Legendre and Chebyshev polynomials  were chosen for the basis functions.
The Chebyshev and Legendre polynomials are
orthogonal over -1. <= x <= 1. The data x values are normalized to
this regime using minimum and maximum x values supplied by the user.
For each data point the ncoeff basis functions are calculated using the
following recursion relations.

.nf
    Legendre series
    F(1,x) = 1.
    F(2,x) = x
    F(n,x) = [(2*n-3)*x*F(n-1,x)-(n-2)*F(n-2,x)]/(n-1)

    Chebyshev series
    F(1,x) = 1.
    F(2,x) = x
    F(n,x) = 2*x*F(n-1,x)-F(n-2,x)
.fi

.sh
3.2.2. Cubic Cardinal B-Spline

The approximating function is assumed to be of the form

.nf
    f(x) = a(1)*F(1,x) + a(2)*F(2,x) + ... a(ncoeff)*F(ncoeff,x)
.fi

where the basis functions, F(n, x), are the cubic cardinal B-splines
(Prenter 1975).
The user supplies minimum and maximum x values and the number of polynomial
pieces, npieces, to be fit to the data set. The number of cubic spline
coefficents, ncoeff, will be

.nf
    ncoeff = npieces + 3
.fi

The cardinal B-spline is stored in a lookup table. For each x the appropriate
break point is selected and the four non-zero B-splines are calculated by
nearest neighbour interpolation in the lookup table.

.sh
3.2.3. The Normal Equations

The coefficients, a, are determined by the solution of the normal equations

.nf
    c * a = b
.fi

where

.nf
    c[i,j] = (F(i,x), F(j,x))
    b[j] = (F(j,x), f(x))
.fi

F(i,x) is the ith basis function at x, f(x) is the function to be
approximated and the inner product of two functions G and H, (G,H),
is given by

.nf
    (G, H) = sum (G(x[i]) * H(x[i]) * weight[i]) i=1,...npts
.fi

The resulting matrix is symmetric and positive semi-definite.
Therefore it is necessary to store the ncoeff bands at or below the
diagonal. Storage is particularly efficient for the cubic spline
as only the diagonal and three adjacent lower bands are non-zero
(deBoor 1978).

.sh
3.2.4. Method of Solution

Since the matrix is symmetric, positive semi-definite and banded
it may be solved by the Cholesky method. The data matrix c may be
written as

.nf
    c = l * d * l-transpose
.fi

where l is a unit lower triangular matrix and d is the diagonal of c
(deBoor 1978). Near zero pivots are handled in the following way.
At the nth elimination step the current value of the nth diagonal
element is compared with the original nth diagonal element. If the diagonal
element has been reduced by one computer word length, the entire nth
row is declared linearly dependent on the previous n-1 rows and
a(n) = 0.

The triangular system

.nf
    l * w = b
.fi

is solved for w (forward substitution), the vector d ** (-1) * w
is computed and the triangular system

.nf
    l-transpose * a = d ** (-1) * w
.fi

solved for the coefficients, a (backward substitution).

.sh
3.2.5. Errors

The reduced ch-squared of the fit is defined as the weighted sum of
the squares of the residuals divided by the number of degrees of
freedom.

.nf
    rms = sqrt (sum (weight * (y - yfit) ** 2) / nfree)
    nfree = npts - ncoeff
.fi

The error of the j-th coefficient, error[j], is equal to the square root
of the j-th diagonal element of inverse data matrix times a scale factor.

.nf
    error[j] = sqrt (c[j,j]-inverse) * scale
.fi

The scale factor is the square root of the variance of the data when
all the weights are equal, otherwise scale is one.

.sh
4. Usage

.sh
4.1. User Notes

The following series of steps illustrates the use of the package.

.ls 4
.ls (1)
Insert an include <curfit.h> statement in the calling program to
make the CURFIT package definitions available to the user program.
.le
.ls (2)
Call CVINIT to initialize the curve fitting parameters.
.le
.ls (3)
Call CVACCUM to select a weighting function
and accumulate data points into the appropriate arrays and vectors.
.le
.ls (4)
Call CVSOLVE to solve the normal equations and calculate the coefficients
of the fitting function. Test for an error condition.
.le
.ls (5)
Call CVEVAL or CVVECTOR to evaluated the fitted function at the
x value(s) of interest.
.le
.ls (6)
Call CVCOEFF to fetch the number and value of the coefficients of the fitting
function.
.le
.ls (7)
Call CVERRORS to calculate the standard deviations in the
coefficients and the standard deviation of the fit.
.le
.ls (8)
Call CVFREE to release the space allocated for the fit.
.le
.le

Steps (2) and (3) may be combined in a single step by calling CVFIT
and inputting an array of x, y and weight values. Individual points may
be rejected from the fit by calling CVREJECT and CVSOLVE to determine
a new set of coefficients. If the x and weight values remain the same
and only the y values change from fit to fit, CVREFIT can be called.


.sh
4.2. Examples

.nf
Example 1: Fit curve to data, no weighting

	include <curfit.h>
	...
	call cvinit (cv, CHEBYSHEV, 4, 1., 512.)

	call cvfit (cv, x, y, w, 512, WTS_UNIFORM, ier)
	if (ier != OK)
	    call error (...)

	do i = 1, 512 {
	    x = i
	    call printf ("%g %g\n")
	        call pargr (x)
	        call pargr (cveval (cv, x))
	}

	call cvfree (cv)

Example 2: Fit curve using accumulate mode, weight based on spacing

	include <curfit.h>
	...
	call cvinit (cv, SPLINE3, npolypieces, 1., 512.)

	old_x = 0.0
	do i =1, 512 {
	    x = real (i)
	    if (y[i] != INDEF) {
		call cvaccum (cv, x, y, x - old_x, WTS_USER)
		old_x = x
	    }
	}

	call cvsolve (cv, ier)
	if (ier != OK)
	    call error (...)
	...
	call cvfree (cv)

Example 3: Fit and subtract smooth curve from image lines

	include <curfit.h>
	...
	call cvinit (cv, CHEBYSHEV, order, 1., 512.)

	do line = 1, nlines {
	    inpix = imgl2r (im, line)
	    outpix = impl2r (im, line)
	    if (line == 1)
		call cvfit (cv, x, Memr[inpix], w, 512, WTS_USER, ier)
	    else
		call cvrefit (cv, x, Memr[inpix], w, WTS_USER, ier)
	    if (ier != OK)
		...
	    call cvvector (cv, x, y, 512)
	    call asubr (Memr[inpix], y, Memr[outpix], 512)
	}

	call cvfree (cv)

Example 4: Fit curve and save parameters for later use by CVEVAL
	   Fit must be at least order + 7 elements long.

	include <curfit.h>

	real  fit[LEN_FIT)
	...
	call cvinit (cv, LEGENDRE, order, xmin, xmax)
	call cvfit (cv, x, y, w, npts, WTS_UNIFORM, ier)
	if (ier != OK) 
	    ...
	call cvsave (cv, fit)
	call cvfree (cv)
	...
	call cvrestore (cv, fit)
	do i = 1, npts
	    yfit[i] = cveval (cv, x[i])
	call cvfree (cv)
	...

.fi

.sh
5. Detailed Design

.sh
5.1. Curve Descriptor Structure

The CURFIT parameters, and the
size and location of the arrays and vectors used in the fitting procedure
are stored in the curve descriptor structure. The structure is referenced
by the pointer cv returned by the CVINIT routine.  The curve
descriptor structure is defined in the package
header file curfit.h. The structure is listed below.

.nf
define	LEN_CVSTRUCT	17

# CURFIT parameters

define	CV_TYPE		Memi[$1]	# Type of curve to be fitted
define	CV_ORDER	Memi[$1+1]	# Order of the fit
define	CV_NPIECES	Memi[$1+2]	# Number of polynomial pieces (spline)
define	CV_NCOEFF	Memi[$1+3]	# Number of coefficients
define	CV_XMAX		Memr[$1+4]	# Maximum x value
define	CV_XMIN		Memr[$1+5]	# Minimum x value
define	CV_RANGE	Memr[$1+6]	# Xmax minus xmin
define	CV_MAXMIN	Memr[$1+7]	# Xmax plus xmin
define	CV_SPACING	Memr[$1+8]	# Break point spacing (spline)
define	CV_NPTS		Memi[$1+9]	# Number of data points

# Pointers to storage arrays and vectors

define	CV_XBASIS	Memi[$1+10]	# Basis functions single x
define	CV_MATRIX	Memi[$1+11]	# Pointer to matrix
define	CV_CHOFAC	Memi[$1+12]	# Pointer to Cholesky factorization
define	CV_VECTOR	Memi[$1+13]	# Pointer to vector
define	CV_COEFF	Memi[$1+14]	# Pointer to coefficient vector

# Used only by CVREFIT

define	CV_BASIS	Memi[$1+15]	# Pointer to basis functions all x
define	CV_LEFT		Memi[$1+16]	# Pointer to index array (spline)
.fi

.sh
5.2. Storage Requirements

The storage requirements are listed below.

.ls 4
.ls real MATRIX[order,ncoeff]
The real array, matrix, stores the original accumulated data. Storage of this
array is required by the CURFIT routines CVACCUM and CVREJECT which accumulate
and reject individual points from the data set respectively. If the fitting
function is SPLINE3 then order = 4, otherwise order = ncoeff.
.le
.ls real CHOFAC[order, ncoeff]
The real array chofac stores the Cholesky factorization of matrix.
Storage of CHOFAC is required by the CURFIT routines CVERRORS and
CVREFIT.
.le
.ls real VECTOR[ncoeff]
Ncoeff real storage units must be allocated for the vector containing
the right side of the matrix equation. VECTOR is stored for use by the
CVREJECT and CVACCUM routines. Vector is zeroed before every CVREFIT call.
.le
.ls real COEFF[ncoeff]
The coefficients of the fitted function must be stored for use by
the CVEVAL, CVVECTOR, and CVCOEFF routines.
.le
.ls real BASIS[order,npts]
Space is allocated for the basis functions only if the routine CVREFIT is
called.  The first call to CVREFIT generates an array of basis functions and
subsequent calls reference the array.
.le
.ls int	LEFT[npts]
Space for the array left is allocated only if
CVREFIT is called. The array indicates to which element of
the matrix a given spline function should be accumulated.
.le
.le

.sh
6. References

.ls (1)
Carl de Boor, "A Practical Guide to Splines", 1978, Springer-Verlag New
York Inc.
.le
.ls (2)
P.M. Prenter, "Splines and Variational Methods", 1975, John Wiley and Sons
Inc.
.le
.endhelp
