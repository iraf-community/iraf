.help surfit Aug84 "Math Package"
.CE
Specifications for the Surface Fitting Package
.CE
Lindsey Davis
.CE
August 1984
.CE
First Draft

.sh
1. Introduction

The SURFIT package provides a set of routines for fitting surfaces to functions
linear in their coefficients using least squares techniques. The basic numerical
technique employed is the solution of the normal equations by the Cholesky
method.

.sh
2. Requirements

.ls (1)
The package shall take as input a surface or section of
a surface.
The data are assumed to be on a rectangular grid and to have the following
ranges, 0 <= x <= ncols + 1 and 0 <= y <= nlines + 1.
The package routines assume that data values equal to INDEF have
been removed from the data set or replaced with appropriate interpolated
values prior to entering the package routines. It is not necessary that
all the data be present to perform the fit.
.le
.ls (2)
The package shall perform the following operations:
.ls o
Determine the coefficients of the fitting function by solving the normal
equations. The surface fitting function is selected at run time from the
following list: (1) SF_LEGENDRE, Legendre polynomials in x and y,
(2) SF_CHEBYSHEV, Chebyshev polynomials in x and y, (3) SF_SPLINE3, bicubic
spline with break points evenly spaced in x and y. The calling sequence
must be invariant to the form of the fitting function.
.le
.ls o
Set an error code if the numerical routines are unable to fit the specified
function.
.le
.ls o
Optionally output the values of the coefficients. The coefficients will be
stored internal to the SURFIT package. However in some applications it is
the coefficients which are of primary interest. A package routine shall
exist to extract the coefficients from the curve descriptor structure.
.le
.ls o
Evaluate the surface at arbitrary value(s) of x and y. The evaluating
routines shall use the calculated coefficients and the user supplied
x values(s).
.le
.ls o
Be capable of storing the fitted surface parameters and coefficients in a
user supplied array and restoring the saved fit for later use by the
evaluating routines.
.le
.ls o
Input surface parameters and surface coefficients derived external to
the surfit package into the surfit format for use by the evaluate
routines.
.le
.le
.ls (3)
The program shall perform a weighted fit to the surface using a user
supplied weight array and weight flag. The weighting options will be
SF_WTSUSER and SF_WTSUNIFORM. In SF_WTSUNIFORM mode the package routines
set all the weights to 1. In SF_WTSUSER mode the package routines apply
user supplied weights to the individual data points.
.le
.ls (4)
The input data set, output coefficients, error and fitted z arrays are
single precision real quantities. All package arithmetic shall be performed
in single precision.
.le

.sh
3. Specifications

.sh
3.1. List of Routines


The surface is defined in the following way. The x and y values are
assumed to be integers and lie in the range 0 <= x <= ncols + 1 and
0 <= y <= nlines + 1. The package prefix is is for image surface fit.
Package routines with a prefix followed by l operate on individual
image lines only.

.nf
	z = f (col, line)
.fi

.nf
        isinit	(sf, surf_type, xorder, yorder, xterms, ncols, nlines)
	iszero	(sf)
       islzero	(sf, lineno)
      islaccum	(sf, cols, lineno, z, w, ncols, wtflag)
      islsolve	(sf, lineno, ier)
        islfit	(sf, cols, lineno, z, w, ncols, wtflag, ier)
      islrefit	(sf, cols, lineno, z, w, ier)
       issolve	(sf, lines, nlines, ier)
     isresolve	(sf, lines, ier)
    z = iseval	(sf, x, y)
      isvector	(sf, x, y, zfit, npts)
        issave	(sf, surface)
     isrestore	(sf, surface)
        isfree	(sf)
.fi

.sh
3.2. Algorithms

.sh
3.2.1. Polynomial Basis Functions

The approximating function is assumed to be of the following form,

.nf
    f(x,y) = sum (a[i,j] * F(i,x) * F(j,y))    i = 1...nxcoeff  j = 1...nycoeff
.fi

where the F(i,x) are the polynomial basis functions containing terms of order
x**(i-1) and the a(i,j) are the coefficients. In order to avoid a very
ill-conditioned linear system for moderate or large i, the Legendre and
Chebyshev polynomials were chosen for the basis functions. The Chebyshev
and Legendre polynomials are orthogonal over -1. <= x,y <= 1. The data x and
y values are normalized to this regime using the number of lines and columns
supplied by the user. For each data point the basis
functions are calculated using the following recursion relations. The
cross terms are optional.

Legendre series:
.nf

    F(1,x) = 1.
    F(2,x) = x
    F(i,x) = [(2*i-1)*x*F(i-1,x)-(i-2)*F(i-2,x)]/(i-1)
    F(1,y) = 1.
    F(2,y) = y
    F(j,y) = [(2*j-1)*y*F(j-1,y)-(j-2)*F(j-2,y)]/(j-1)

.fi
Chebyshev series:
.nf

    F(1,x) = 1.
    F(2,x) = x
    F(i,x) = 2.*x*F(i-1,x)-F(i-2,x)
    F(1,y) = 1.
    F(2,y) = y
    F(j,y) = 2.*y*F(j-1,y)-F(j-2,y)
.fi


.sh
3.2.2. Bicubic Cardinal B-spline

The approximating function is of the form

.nf
    f(x,y) =  sum (x(i,j) * F(i,x) * F(j,y))   i=1...nxcoeff j=1...nycoeff
.fi

where the basis functions, F(i,x), are the cubic cardinal B-splines
(Prenter 1975). The user supplies the number of columns and lines and the
number of polynomial pieces in x and y, nxpieces and nypieces, to be fit
to the data set. The number of bicubic spline coefficients, ncoeff, will be

.nf
    nxcoeff = (nxpieces + 3)
    nycoeff = (nypieces + 3)
    ncoeff = nxcoeff * nycoeff
.fi

The cardinal B-spline is stored in a lookup table. For each x and y the
appropriate break point is selected and the four non-zero B-splines are
calculated by nearest neighbour interpolation in the lookup table.

.sh
3.2.3. Method of Solution

.sh
3.2.3.1. Full Surface Fit

The normal equations are accumulated in the following form.

.nf
    c * x = b
    c[i,j] = (B(i,x,y), B(j,x,y))
    b[j] = (B(j,x,y), S(x,y))
.fi

B(i,x,y) is the ith basis function at x and y, S(x,y) is the surface to
be approximated and the inner product of two functions G and H is
given by 

.nf
    (G,H) = sum (w[i] * G(x[i],y[i]) * H(x[i],y[i]))  i = 1...npts
.fi

Since the matrix c is symmetric and positive semi-definite it may
be solved by the Cholesky method.
The coefficient matrix c  can be written as

.nf
    c = l * d * l-transpose
.fi

where l is a unit lower triangular matrix and d is the diagonal of c
(de Boor 1978). Near zero pivots are handled in the following way.
At the nth elimination step the current value of the nth diagonal
element is compared with the original nth diagonal element. If the
diagonal element has been reduced by one computer word length, the
entire nth row is declared linearly dependent on the previous n-1
rows and x(n) = 0.

The triangular system

.nf
    l * w = b
.fi

is solved for w (forward substitution), the vector d  ** (-1) * w is
computed and the triangular system

.nf
    l-transpose * x = d ** (-1) * w
.fi

solved for the coefficients, x (backward substitution).


.sh
3.2.3.2. Line by Line Fit

Each line of the surface can be represented by the following equation.

.nf
    S(x,yo) = a[i](yo) * B[i](x)            i = 1...nxcoeff for each yo
.fi

The normal equations for each image line are formed

.nf
    c * a = b
    c[i,j] = (B(i,x), B(j,x))
    b[j] = (B(j,x), S(x,yo))
.fi

and solved for a as described in the previous section.
After fitting the entire image matrix a has nxcoeff columns and
nlines rows.

For each column i in a the normal equations
are formed and solved for the c[i,j]

.nf
    c * x  = b
    c[j,k] = (B(j,y), B(k,x))
    b[i,j] = (a[i](y), B(j,y))
.fi

.sh
3.2.4. Number of Operations

It is worth while to calculate the number of operations reqired to compute
the surface fit by the full surface method versus the line by line method.
The number of operations required to accumulate the normal equations
and solve the set is the following

.nf
    nop = npts * order ** 2 / 2 + order ** 3 /6
.fi

where the first term is the number of operations required to accumulate the
matrix and the second is the number required to solve the system.

.nf
Example 1: full surface, 3 by 3 polynomial no cross terms, 512 by 512 image

    norder = 5
    npts = 512 * 512
    nops(accum) = 3,276,800
    nops(solve) = 21

Example 2: full surface, 30 by 30 spline, 512 by 512 image

    norder = 900
    npts = 512 * 512
    nops(accum) = 1.062 * 10 ** 11
    nops(solve) = 1.216 * 10 ** 8

Example 3: line by line method, 3 by 3 polynomial, 512 by 512 image

    step 1 solve for a coefficients
    norder = 3
    npts = 512
    nlines = 512
    nops(accum) = 1,179,648
    nops(solve) = 2304

    step 2 solve for c coefficients
    norder = 3
    npts = 512
    nlines = 3
    nops(accum) = 6912
    nops(solve) = 14

Example 4: line by line method, 30 by 30 spline, 512 by 512 image

    step 1 solve coefficients
    norder = 30
    npts = 512
    nlines = 512
    nops(accum) = 117,964,800
    nops(solve) = 2,304,000

    step 2 solve for c coefficients
    norder = 30
    npts = 512
    nlines = 30
    nops(accum) = 6,912,000
    nops(solve) = 135,000
.fi

The figures for the line by line method are worst case numbers. If the
x values remain the same from line to line then the coefficient matrix
only has to be accumulated and inverted twice.
For the bicubic spline function the number of operations is significantly
reduced by taking advantage of the banded nature of the matrices.

.sh
4. Usage

.sh
4.1. User Notes

The following series of steps illustrates the use of the package.

.ls 4
.ls (1)
Include an include <surfit.h> statement in the calling program to make the
SURFIT package definitions available to the user program.
.le
.ls (2)
Call SFINIT to initialize the surface fitting parameters.
.le
.ls (3)
Call SFLACCUM to select a weighting function and accumulate data points
for each line into the appropriate arrays and vectors. Call SFLSOLVE
to compute the coefficients in x for each line. The coefficents are
stored inside SURFIT. Repeat this procedure for each line. SFLACCUM
and SFLSOLVE can be combined by a call to SFLFIT. If the x values
and weights remain the same from line to line SFLREFIT can be called.
.le
.ls (4)
Call SFSOLVE to solve for the surface coefficients.
.le
.ls (5)
Call SFEVAL or SFVECTOR to evaluate the fitted surface at the x and y
value(s) of interest.
.le
.ls (6)
Call SFFREE to release the space allocated for the fit.
.le
.le

.sh
4.2. Examples

.nf
Example 1: Fit a 2nd order Lengendre polynomial in x and y to an image
and output the fitted image

include <imhdr.h>
include <math/surfit.h>

 	
	old = immap (old_image, READ_ONLY, 0)
	new = immap (new_image, NEW_COPY, 0)
	
	ncols = IM_LEN(old, 1)
	nlines = IM_LEN(old, 2)
	
	# initialize surface fit
	call isinit (sf, LEGENDRE, 3, 3, YES, ncols, nlines)
        
	# allocate space for lines, columns and weight arrays
	call malloc (cols, ncols, TY_INT)
	call malloc (lines, nlines, TY_INT)
	call malloc (weight, ncols, TY_REAL)

	# initialize lines and columns arrays
	do i = 1, ncols
	    Memi[cols - 1 + i] = i
	do i = 1, nlines
	    Memi[lines - 1 + i] = i

	# fit the surface in x line by line
	call amovkl (long(1), v, IM_MAXDIM)
	do i = 1, nlines {
	    if (imgnlr (old, inbuf, v) == EOF)
		call error (0, "Error reading image.")
	    if (i == 1) {
		call islfit (sf, Memi[cols], i, Memr[inbuf], Memr[weight],
				ncols, SF_WTSUNIFORM, ier)
		if (ier != OK)
		    ...
	    } else
		call sflrefit (sf, Memi[cols], i, Memr[inbuf], Memr[weight],
				ier)
	}

	# solve for surface coefficients
	call issolve (sf, Memi[lines], nlines, ier)

	# free space used in fitting arrays
	call mfree (cols, TY_INT)
	call mfree (lines, TY_INT)
	call mfree (weight, TY_REAL)

	# allocate space for x and y arrays
	call malloc (x, ncols, TY_REAL)
	call malloc (y, ncols, TY_REAL)

	# intialize z array
	do i = 1, ncols
	    Memr[x - 1 + i] = real (i)

	# create fitted image
	call amovkl (long(10, v, IM_MAXDIM)
	do i = 1, nlines {
	    if (impnlr (new, outbuf, v) == EOF)
		call error (0, "Error writing image.")
	    call amovkr (real (i), Memr[y], ncols)
	    call isvector (sf, Memr[x], Memr[y], Memr[outbuf], ncols)
	}

	# close files and cleanup
	call mfree (x, TY_REAL)
	call mfree (y, TY_REAL

	call isfree (sf)

	call imunmap (old)
	call imunmap (new)

.fi
.sh
5. Detailed Design

.sh
5.1. Surface Descriptor Structure

To be written when specifications are finalised.

.sh
5.2. Storage Requirements

The minimum storage requirements for the full surface fit method
assuming that points are to be rejected from the fit without revaluating
the matrix are the following.

.nf
    (nxcoeff*nycoeff) ** 2    -- coefficient array plus triangularized matrix
    nxcoeff*nycoeff           -- right side
    nxcoeff*nycoeff           -- solution vector
.fi

For a 30 by 30 spline roughly 811,800 storage units are required.
For a 3 by 3 polynomial the requirements are 675 storage units. The
requirements are roughly half when rejection is disabled.

The minimum storage requirements for the line by line method are

.nf
    nx*nx*2   -- The x matrix and its factorization
    nx*nlines -- The x coefficient matrix
    ny*ny*2 -- The y matrix and its factorization
    nx*ny -- The solution
.fi


.sh
6. References

.ls (1)
Carl de Boor, "A Practical Guide to Splines", 1978, Springer-Verlag New York
Inc.
.le
.ls (2)
P.M. Prenter, "Splines and Variational Methods", 1975, John Wiley and Sons
Inc.
.le
.endhelp
