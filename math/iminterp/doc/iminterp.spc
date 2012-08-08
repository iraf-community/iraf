.help iminterp Jul84 "Math Package"

.ce
Specifications for the Image Interpolator Package
.ce
Lindsey Davis
.ce
Vesa Junkkarinen
.ce
August 1984

.sh
1. Introduction

    One of the most common operations in image processing is
interpolation in a data array. Due to the large amount of data involved,
efficiency is highly important. The advantage of having locally written
interpolators, includes the ability to optimize for uniformly spaced data
and the possibility of adding features that are useful to the final
application.

.sh
2. Requirements

.ls (1)
The package shall take as input a one-dimensional array containing image
data. The pixels are assumed to be equally spaced along a line.
The coordinates of a pixel are assumed to be
the same as the subscript of the pixel in the data array.
The coordinate of the first pixel in the array and the spacing between pixels
is assumed to be 1.0. All pixels are assumed to be good.
Checking for INDEF valued and out of bounds pixels is the responsibility of the
user. A routine to remove INDEF valued pixels from a data array shall be
included in the package.
.le
.ls (2)
The package is divided into array sequential interpolators and array
random interpolators. The sequential interpolators have been optimized
for returning many values as is the case when an array is shifted, or
oversampled at many points in order to produce a
smooth plot.
The random interpolators allow the evaluation of a few interpolated
points without the computing time and storage overhead required for
setting up the sequential version.
.le
.ls (3)
The quality of the interpolant will be set at run time. The options are:

.nf
    II_NEAREST		- nearest neighbour
    II_LINEAR		- linear interpolation
    II_POLY3		- 3rd order divided differences
    II_POLY5		- 5th order divided differences
    II_SPLINE3		- cubic spline
.fi

The calling sequences shall be invariant to the interpolant selected.
Routines should be designed so that new interpolants can be added
with minimal changes to the code and no change to the calling sequences.
.le
.ls (4)
The interpolant parameters and the arrays necessary to store the coefficients
are stored in a structure referenced by a pointer. The pointer is returned
to the user program by the initial call to ASIINIT or ASIRESTORE and freed
by a call to ASIFREE (see section 3.1).
.le
.ls (5)
The package routines shall be able to:
.ls o
Calculate the coefficients of the interpolant and store these coefficients in
the appropriate part of the interpolant descriptor structure.
.le
.ls o
Evaluate the interplant at a given x(s) coordinate(s).
.le
.ls o
Calculate the derivatives of the interpolant at a given value of x.
.le
.ls o
Integrate the interpolant over a specified x interval.
.le
.ls o
Store the interpolant in a user supplied array. Restore the saved interpolant
to the interpolant descriptor structure for later use by ASIEVAL, ASIVECTOR,
ASIDER and ASIGRL.
.le

.sh
3. Specifications

.sh
3.1. The Array Sequential Interpolator Routines

    The package prefix is asi and the package routines are:

.nf
        asiinit	(asi, interp_type)
         asifit	(asi, datain, npix)
    y = asieval	(asi, x)
      asivector	(asi, x, yfit, npix)
         asider	(asi, x, der, nder)
     v = asigrl	(asi, a, b)
        asisave	(asi, interpolant)
     asirestore	(asi, interpolant)
        asifree	(asi)
.fi

.sh
3.2. The Array Random Interpolator Routines

    The package prefix is ari and the package routines are:

.nf
    y = arieval	(x, datain, npix, interp_type)
	 arider	(x, datain, npix, der, nder, interp_type)
.fi

.sh
3.3. Miscellaneous

    A routine has been included in the package to remove INDEF valued
pixels from an array.

.nf
    arbpix (datain, dataout, npix, interp_type, boundary_type)
.fi

.sh
3.4. Algorithms

.sh
3.4.1. Coefficients

    The coefficient array used by the asi routines is calculated by ASIFIT.
ASIFIT accepts an array of data, checks that the number
of data points is appropriate for the interpolant selected, allocates
space for the interpolant, and calculates the coefficients.
Boundary coefficient values are calculated
using boundary projection.  With the exception of the cubic spline interpolant,
the coefficients are stored as the data points.
The B-spline coefficients are 
calculated using natural end conditions (Prenter 1975).
After a call to ASIFIT the coefficient array contains the following.

.nf
    case II_NEAREST:

        # no boundary conditions necessary
	coeff[1] = datain[1]
	.
	.
	.
	coeff[npts] = datain[npix]

    case II_LINEAR:

        # coeff[npxix+1] required if x = npix
	coeff[1] = datain[1]
	.
	.
	.
	coeff[npix] = datain[npix]
	coeff[npix+1] = 2. * datain[npix] - datain[npix-1]

    case II_POLY3:

        # coeff[0] required if x = 1
	# coeff[npix+1], coeff[npix+2] required if x = npix
	coeff[0] = 2. * datain[1] - datain[2]
	coeff[1] = datain[1]
	.
	.
	.
	coeff[npix] = datain[npix]
	coeff[npix+1] = 2. * datain[npix] - datain[npix-1]
	coeff[npix+2] = 2. * datain[npix] - datain[npix-2]

    case II_POLY5:

        # coeff[1], coeff[0] reqired if x = 1
	# coeff[npix+1], coeff[npix+2], coeff[npix=3]
	# required if x = npix

	coeff[-1] = 2. * datain[1] - datain[3]
	coeff[0] = 2. * datain[1] - datain[2]
	coeff[1] = datain[1]
	.
	.
	.
	coeff[npix] = datain[npix]
	coeff[npix+1] = 2. * datain[npix] - datain[npix-1]
	coeff[npix+2] = 2. * datain[npix] - datain[npix-2]
	coeff[npix+3] = 2. * datain[npix] - datain[npix-3]

    case SPLINE3:

        # coeff[0] = 2nd der at x = 1, coeff[0] = 0.
	# coeff[npix+1] = 2nd der at x = npts, coeff[npix+1] = 0.
	# coeff[npix+2] = 0., required if x = npix
	coeff[0] = b[1]
	coeff[1] = b[2]
	.
	.
	.
	coeff[npix] = b[npix+1]
	coeff[npix+1] = b[npix+2]
	coeff[npix+2] = 0.
.fi

.sh
3.4.2. Evaluation

    The ASIEVAL and ASIVECTOR routines have been optimized to be as efficient
as possible. The values of the II_NEAREST and II_LINEAR interpolants
are calculated directly. The II_SPLINE3 interpolant is evaluated using
polynomial coefficients calculated directly from the B-spline coefficients
(de Boor 1978). Values of the higher order polynomial interpolants
are calculated using central differences. The equations for each case are
listed below.

.nf
case II_NEAREST:
    
    y = coeff[int (x + 0.5)]

case II_LINEAR:

    nx = x
    y = (x - nx) * coeff[nx+1] + (nx + 1 - x) * coeff[nx]

case II_POLY3:

    nx = x
    s = x - nx
    t = 1. - s

    # second central differences
    cd20 = 1./6. * (coeff[nx+1] - 2. * coeff[nx] + coeff[nx-1])
    cd21 = 1./6. * (coeff[nx+2] - 2. * coeff[nx+1] + coeff[nx])

    y = s * (coeff[nx+1] + (s * s - 1.) * cd21) + t * (coeff[nx] +
	(t * t - 1.) * cd20)

case II_POLY5:

    nx = x
    s = x - nx
    t = 1. - s

    # second central differences
    cd20 = 1./6. * (coeff[nx+1] - 2. * coeff[nx] + coeff[nx-1])
    cd21 = 1./6. * (coeff[nx+2] - 2. * coeff[nx+1] + coeff[nx])

    # fourth central diffreences
    cd40 = 1./120. * (coeff[nx-2] - 4. * coeff[nx-1] + 6. * coeff[nx] - 4. *
	   coeff[nx+1] + a[nx+2])
    cd41 = 1./120. * (coeff[nx-1] - 4. * coeff[nx] + 6. * coeff[nx+1] - 4. *
	   coeff[nx+2] + coeff[nx+3]

    y = s * (coeff[nx+1] + (s * s - 1.) * (cd21 + (s * s - 4.) * cd41)) +
	t * (coeff[nx] + (t * t - 1.) * (cd20 + (t * t - 4.) * cd40))

case II_SPLINE3:

    nx = x
    s = x - nx

    pc[1] = coeff[nx-1] + 4. * coeff[nx] + coeff[nx+1]
    pc[2] = 3. * (coeff[nx+1] - coeff[nx-1])
    pc[3] = 3. * (coeff[nx-1] - 2. * coeff[nx] + coeff[nx+1])
    pc[4] = -coeff[nx-1] + 3. * coeff[nx] - 3. * coeff[nx+1] + coeff[nx+2]

    y = pc[1] + s * (pc[2] + s * (pc[3] + s * pc[4]))
.fi


    The ARIEVAL routine uses the expressions above to evaluate the
interpolant. However unlike ASIEVAL, ARIEVAL does not use a previously
calculated coefficient array. Instead ARIEVAL selects the appropriate
portion of the data array, calculates the coefficients and boundary
coefficients if necessary, and evaluates the interpolant at the time it
is called. The cubic spline interpolant uses at most SPLTS (currently 16)
data points to calculate the B-spline coefficients.

.sh
3.4.3. Derivatives

    Derivatives of the interpolant are calculated by evaluating the
derivatives of the interpolating polynomial. For all interpolants der[1]
equals the value of the interpolant at x.
For the sake of efficiency the derivatives
of the II_NEAREST and II_LINEAR interpolants are calculated directly.

.nf
    case II_NEAREST:

	der[1] = coeff[int (x+0.5)]

    case II_LINEAR:

	der[1] = (x - nx) * coeff [nx+1] + (nx + 1 - x) * coeff[nx]
	der[2] = coeff[nx+1] - coeff[nx]
.fi

    In order to calculate the derivatives of the cubic spline and
polynomial interpolants
the coefficients of the interpolating polynomial must be calculated.
The polynomial
coefficients for the cubic spline interpolant are computed directly from the
B-spline coefficients (see 3.4.2.). The higher order polynomial
interpolant coefficients are calculated as follows.

First the appropriate portion of the coefficient array is loaded.

.nf
	do i = 1, nterms
	    d[i] = coeff[nx - nterms/2 + i]
.fi

Next the divided differences are calculated (Conte and de Boor 1972).

.nf
	do k = 1, nterms - 1
	    do i = 1, nterms - k
		d[i] = (d[i+1] - d[i]) / k
.fi

The d[i] are the coefficients of an interpolating polynomial of the
following form. The x[i] are the nterms data points surrounding the
point of interest.

.nf
	p(x) = d[1] * (x-x[1]) * ... * (x-x[nterms-1) +
	       d[2] * (x-x[2]) * ... * (x-x[nterms-1]) + ... + d[nterms]
.fi

Next a new set of polynomial coefficients are calculated
(Conte and de Boor 1972).

.nf
	do k = nterms, 2, -1
	    do i = 2, k
		d[i] = d[i] + d[i-1] * (k - i - nterms/2)
.fi

The new d[i] are the coefficients of the follwoing polynomial.

.nf
	nx = x
	p(x) = d[1] * (x-nx) ** (nterms-1) + d[2] * (x-nx) ** (nterms-2) + ...
	       d[nterms]
.fi

The d[i] array is flipped. The value and derivatives
of the interpolant are then calculated using the d[i] array and
nested multiplication.

.nf
	s = x - nx

	do k = 1, nder {

	    accum = d[nterms-k+1]

	    do j = nterms - k, 1, -1
		accum = d[j] + s * accum

	    der[k] = accum

	    # differnetiate
	    do j = 1, nterms - k
		d[j] = j * d[j + 1]
	}
.fi

    ARIDER calculates the derivatives of the interpolant using the same
technique ASIDER. However ARIDER does not use a previously calculated
coefficient array like ASIDER. Instead ARIDER selects the appropriate portion
of the data array, calculates the coefficients and boundary coefficients,
and computes the derivatives at the time it is called.

.sh
3.4.5. Integration

    ASIGRL calculates the integral of the interpolant between fixed limits
by integrating the interpolating polynomial. The coefficients of the
interpolating polynomial are calculated as discussed in section 3.4.4.

.sh 
4. Usage

.sh
4.1. User Notes

The following series of steps illustrates the use of the package.

.ls 4
.ls (1)
Insert an include <iminterp.h> statement in the calling program to make
the IINTERP definitions available to the user program.
.le
.ls (2)
Remove INDEF valued pixels from the data using ARBPIX.
.le
.ls (3)
Call ASIINIT to initialize the interpolant parameters.
.le
.ls (4)
Call ASIFIT to calculate the coefficients of the interpolant.
.le
.ls (5)
Evaluate the interpolant at a given value of x(s) using ASIEVAL or
ASIVECTOR.
.le
.ls (6)
Calculate the derivatives and integral or the interpolant using
ASIDER and ASIGRL.
.le
.ls (7)
Free the interpolator structure by calling ASIFREE.
.le
.le

    The interpolant can be saved and restored using ASISAVE and ASIRESTORE.
If the values and derivatives of only a few points in an array are desired
ARIEVAL and ARIDER can be called.

.sh
4.2. Examples

.nf
Example 1: Shift a data array by a constant amount

    include <iminterp.h>
    ...
    call asiinit (asi, II_POLY5)
    call asifit (asi, inrow, npix)

    do i = 1, npix
	outrow[i] = asieval (asi, i + shift)

    call asifree (asi)
    ...

Example 2: Calculate the integral under the data array

    include <iminterp.h>
    ...
    call asiinit (asi, II_POLY5)
    call asifit (asi, datain, npix)

    integral =  asigrl (asi, 1. real (npix))

    call asifree (asi)
    ...

Example 2: Store interpolant for later use by ASIEVAL
	   LEN_INTERP must be at least npix + 8 units long where npix is
	   is defined in the call to ASIFIT.

    include <iminterp.h>

    real   interpolant[LEN_INTERP]
    ...
    call asiinit (asi, II_POLY3)
    call asifit (asi, datain, npix)
    call asisave (asi, interpolant)
    call asifree (asi)
    ...
    call asirestore (asi, interpolant)
    do i = 1, npts
	yfit[i] = asieval (asi, x[i])
    call asifree (asi)
    ...
.fi
.sh
5. Detailed Design

.sh
5.1. Interpolator Descriptor

    The interpolant parameters and coefficients are stored in a
structure listed below.

.nf
    define	LEN_ASISTRUCT	4		# Length in structure units of
						# interpolant descriptor

    define	ASI_TYPE	Memi[$1]	# Interpolant type
    define	ASI_NCOEFF	Memi[$1+1]	# No. of coefficients
    define	ASI_OFFSET	Memi[$1+2]	# First "data" point in
						# coefficient array
    define	ASI_COEFF	Memi[$1+3]	# Pointer to coefficient array
.fi

.sh
5.2. Storage Requirements

    The interpolant descriptor requires LEN_ASISTRUCT storage units. The
coefficient array requires ASI_NCOEFF(asi) real storage elements, where
ASI_NCOEFF(asi) is defined as follows.

.nf
    ASI_NCOEFF(asi) = npix	# II_NEAREST
    ASI_NCOEFF(asi) = npix+1	# II_LINEAR
    ASI_NCOEFF(asi) = npix+3	# II_POLY3
    ASI_NCOEFF(asi) = npix+5	# II_POLY5
    ASI_NCOEFF(asi) = npix+3	# II_SPLINE3
.fi

.sh
6. References

.ls (1)
Carl de Boor, "A Practical Guide to Splines", 1978, Springer-Verlag New
York Inc.
.le
.ls (2)
S.D. Conte and C. de Boor, "Elementary Numerical Analysis", 1972, McGraw-Hill,
Inc.
.le
.ls (3)
P.M. Prenter, "Splines and Variational Methods", 1975, John Wiley and Sons Inc.
.le
.endhelp
