.help iinterp Dec84 "Math Package"
.ce
Specifications for the 2D-Image Interpolator Package
.ce
Lindsey Davis
.ce
December 1984

.sh
1. Introduction

One of the most common operations required in image processing is
two-dimensional interpolation in a data array. Any image operator which
physically moves pixels around requires image interpolation to calculate
the new gray levels of the pixels. The advantage
of having a locally written image interpolation package includes the ability to
optimize for uniformly spaced data and the possibility of adding features
that are useful to the final application.

.sh
2. Requirements

.ls (1)
The package shall take as input a 2-D array containing an image or image
section. The pixels are assumed to be equally spaced on a rectangular grid.
The coordinates of the pixels
are assumed to be the same as the subscripts of the pixel in the data array.
Therefore the coordinates of the first pixel in the array are assumed
to be (1,1). For operations on image sections the calling program must
keep track of any transformations between image and section coordinates.
All pixels are assumed to be good.
Checking for INDEF and out of bounds pixels is
the responsibility of the user. A routine to remove INDEF valued pixels
ARBPIX is available in the 1-D package.
.le
.ls (2)
The package is divided into array sequential interpolants and array random
interpolants. The sequential interpolants have been optimized for returning
many values as when an array is shifted or when an array is oversampled
at many points in order to produce a smooth surface plot.
The random interpolants
allow the evaluation of a few interpolated points without the computing
time and storage overhead required for setting up the sequential version.
.le
.ls (3)
The quality of the interpolant will be set at run time. The options are:

.nf
    II_BINEAREST		# nearest neighbour
    II_BILINEAR			# bilinear
    II_BIPOLY3			# bicubic interior polynomial
    II_BIPOLY5			# biquintic interior polynomial
    II_BISPLINE3		# bicubic spline
.fi

The calling sequences shall be invariant to the interpolant selected.
Routines should be designed so that new interpolants can be added with
minimal changes to the code and no changes to the calling sequences.
.le
.ls (4)
The interpolant parameters and the arrays necessary to store the
coefficients are stored in a structure referenced by a pointer. The pointer
is returned to the user program by the initial call to MSIINIT or
MSIRESTORE and freed by a call to MSIFREE.
.le
.ls (5)
The package routines should be able to:
.ls o
Calculate the coefficients of the interpolant and store these coefficients
in the appropriate part of the interpolant descriptor structure.
.le
.ls o
Evaluate the interpolant at a given x-y(s) coordinate.
.le
.ls o
Evaluate the first nder derivatives at a given value of x-y.
.le
.ls o
Integrate the interpolant over a specified interval in x-y.
.le

.sh
3. Specifications

.sh
3.1. The Matrix Sequential Interpolator Routines

The package prefix is msi and the package routines are:

.nf
        msiinit	(msi, interp_type)
         msifit	(msi, datain, nxpix, nypix, len_datain)
    y = msieval	(msi, x, y)
      msivector	(msi, x, y, zfit, npix)
        msigrid	(msi, x, y, zfit, nx, ny, len_zfit)
         msider	(msi, x, y, der, nxder, nyder, len_der)
     v = msigrl	(msi, x, y, npts)
   v = msisqgrl (msi, x1, x2, y1, y2) 
        msisave	(msi, interpolant)
     msirestore	(msi, interpolant)
        msifree	(msi)
.fi

.sh
3.2. The Matrix Random Interpolator Routines

The package prefix is mri and the package routines are:

.nf
    y = mrieval	(x, y, datain, nx, ny, len_datain, interp_type)
         mrider	(x, y, datain, nx, ny, len_datain, der, nxder, nyder,
	 	 len_der, interp_type)
.fi

.sh
3.3. Miscellaneous

A routine ARBPIX will remove INDEF valued pixels from an
array and is available in the 1-D package.

.nf
    arbpix (datain, dataout, npix, interp_type, boundary_type)
.fi

.sh
3.4. Algorithms

.sh
3.4.1. Coefficients

The coefficients for the msi routines are calculated by MSIFIT. MSIFIT
accepts the input data, checks that the number of data pixels is
appropriate for the interpolant selected, and allocates space for the
interpolant. Boundary coefficient values are calculated using boundary
projection. With the exception of the II_BISPLINE3 option, the interpolant
is stored as the data points. The B-spline coefficients are calculated
using the "natural" end conditions in two steps.
First the B-spline coefficients in x at each value of y are calculated.
The B-x coefficients are then solved for the B-spline coefficients in x-y.
After a call to MSIFIT the coefficient
array contains the following.

.nf
    CASE II_BINEAREST:

	# no boundary extension required, coeff[1:nx,1:ny]

	coeff[i,j] = data[i,j]				i = 1,...,nx
							j = 1,...,ny

    case II_BILINEAR:

	# must extend nx by 1 and ny by 1, coeff[1:nx+1,1:ny+1]

	coeff[i,j] = data[i,j]				i = 1,...,nx
							j = 1,...,ny

	coeff[nx+1,j] = 2 * data[nx] - data[nx-1]	j = 1,...,ny
	coeff[i,ny+1] = 2 * data[ny] - data[ny-1]	i = 1,...,nx

	coeff[nx+1,ny+1] = 2 * coeff[nx+1,ny] - data[nx,ny]

    case II_BIPOLY3:

	# must extend nx by -1 and 2 and ny by -1 and 2, coeff[0:nx+2,0:ny+2]

	coeff[i,j] = data[i,j]				i = 1,...,nx
							j = 1,...,ny

	coeff[0,j] = 2 * data[1,j] - data[2,j]		j = 1,...,ny
	coeff[nx+1,j] = 2 * data[nx,j] - data[nx-1,j]	j = 1,...,ny
	coeff[nx+2,j] = 2 * data[nx,j] - data[nx-2,j]	j = 1,...,ny

	coeff[i,0] = 2 * data[i,1] - data[i,2]		i = 1,...,ny
	coeff[i,ny+1] = 2 * data[i,ny] - data[i,ny-1]	i = 1,...,nx
	coeff[i,ny+2] = 2 * data[i,ny] - data[i,ny-2]	i = 1,...,nx

	# plus remaining points

    case II_BIPOLY5:

	# extend -2 and 3 in nx and -2 and 3 in ny, coeff[-1:nx+3,-1:ny+3]

	coeff[i,j] = data[i,j]				i = 1,...,nx
							j = 1,...,ny

	coeff[-1,j] = 2 * data[1,j] - data[3,j]		j = 1,...,ny
	coeff[0,j] = 2 * data[1,j] - data[2,j]		j = 1,...,ny
	coeff[nx+1,j] = 2 * data[nx,j] - data[nx-1,j]	j = 1,...,ny
	coeff[nx+2,j] = 2 * data[nx,j] - data[nx-2,j]	j = 1,...,ny
	coeff[nx+3,j] = 2 * data[nx,j] - data[nx-3,j]	j = 1,...,ny

	coeff[i,-1] = 2 * data[i,1] - data[i,3]		i = 1,...,nx
	coeff[i,0] = 2 * data[i,1] - data[i,2]		i = 1,...,nx
	coeff[i,ny+1] = 2 * data[i,ny] - data[i,ny-1]	i = 1,...,nx
	coeff[i,ny+2] = 2 * data[i,ny] - data[i,ny-2]	i = 1,...,nx
	coeff[i,ny+3] = 2 * data[i,ny] - data[i,ny-3]	i = 1,...,nx

	# plus remaining conditions

    case II_BISPLINE3:

	# the natural boundary conditions are used, coeff[0:nx+2,0:ny+2]

	coeff[i,j] = data[i,j]				i = 1,...,nx
							j = 1,...,ny

	coeff[i,0] = 0.					i = 0,...,nx+2
	coeff[i,ny+1] = 0.				i = 0,...,nx+2
	coeff[i,ny+2] = 0.				i = 0,...,nx+2

	coeff[0,j] = 0.					j = 1,...,ny
	coeff[nx+1,j] = 0.				j = 1,...,ny
	coeff[nx+2,j] = 0.				j = 1,...,ny

	# plus remaining coefficients

.fi

.sh
3.4.2. Evaluation

The MSIEVAL and MSIVECTOR routines will be optimized to be as efficient
as possible for evaluating arbitrarily spaced data. A special function
MSIGRID is included for evaluating closely spaced points on a rectangular grid.
For the options II_BINEAREST and II_BILINEAR the value of the
interpolant is calculated directly. The II_BISPLINE3 interpolant is
evaluated using polynomial coefficients calculated directly from the
B-spline coefficients. Values of the higher order polynomial interpolants
are calculated using Everett's central difference formula. The equations
are listed below.

.nf
case II_BINEAREST:

    z = coeff[int (x + 0.5), int (y + 0.5))

case II_BILINEAR:

    sx = x - nx		sy = y - ny
    tx = 1. - sx	ty = 1. - sy

    z = tx * ty * coeff[nx,ny] + sx * ty * coeff[nx+1,ny] +
	sy * tx * coeff[nx,ny+1] + sx * sy * coeff[nx+1,ny+1]


case II_BIPOLY3:

    nx = x
    sx = x - nx
    tx = 1. - sx

    # interpolate in x

    i = 1

    do j = ny-1, ny+2 {
	 
	cd20[i] = 1./6. * (coeff[nx+1,j] - 2. * coeff[nx,j] + coeff[nx-1,j])
	cd21[i] = 1./6. * (coeff[nx+2,j] - 2. * coeff[nx+1,j] + coeff[nx,j])

	z[i] = sx * (coeff[nx+1,j] + (sx * sx - 1.) * cd21[i]) +
	       tx * (coeff[nx,j] + (tx * tx - 1.) * cd20[i])	

	i = i + 1
    }

    ny = y
    sy = y - ny
    ty = 1. - sy

    # interpolate in y

    cd20y = 1./6. * (z[3] - 2. * z[2] + z[1])
    cd21y = 1./6. * (z[4] - 2. * z[3] + z[2])

    value = sy * (z[3] + (sy * sy - 1.) * cd21y) +
    	    ty * (z[2] + (ty * ty - 1.) *cd20y) 


case II_BIPOLY5:

    nx = x
    sx = x - nx
    sx2 = sx * sx
    tx = 1. - sx
    tx2 = tx * tx

    # interpolate in x

    i = 1

    do j = ny-2, ny+3 {

	cd20[i] = 1./6. * (coeff[nx+1,j] - 2. * coeff[nx,j] + coeff[nx-1,j])
	cd21[i] = 1./6. * (coeff[nx+2,j] - 2. * coeff[nx+1,j] + coeff[nx,j])
	cd40[i] = 1./120. * (coeff[nx-2,j] - 4. * coeff[nx-1,j] +
		  6. * coeff[nx,j] - 4. * coeff[nx+1,j] + coeff[nx+2,j])
	cd41[i] = 1./120. * (coeff[nx-1,j] - 4. * coeff[nx,j] +
		  6. * coeff[nx+1,j] - 4. * coeff[nx+2,j] + coeff[nx+3,j])

	z[i] = sx * (coeff[nx+1,j] + (sx2 - 1.) * (cd21[j] + (sx2 - 4.) *
	       cd41[j])) + tx * (coeff[nx,j] + (tx2 - 1.) *
	       (cd20[j] + (tx2 - 4.) * cd40[j]))

	i = i + 1
    }

    ny = y
    sy = y - ny
    sy2 = sy * sy
    ty = 1. - sy
    ty2 = ty * ty

    # interpolate in y

    cd20y = 1./6. * (z[3] - 2. * z[2] + z[1])
    cd21y = 1./6. * (z[4] - 2. * z[3] + z[2])
    cd40y = 1./120. * (z[1] - 4. * z[2] + 6. * z[3] - 4. * z[4] + z[5])
    cd41y = 1./120. * (z[2] - 4. * z[3] + 6. * z[4] - 4. * z[5] + z[6])

    value = sy * (z[4] + (sy2 - 1.) * (cd21y + (sy2 - 4.) * cd41y)) +
	    ty * (z[3] + (ty2 - 1.) * (cd20y + (ty2 - 4.) * cd40y))

case II_BISPLINE3:

    # use the B-spline representation

    value = coeff[i,j] * B[i](x) * B[j](y)

    # the B-splines are the following

    B1(x) = (x - x1) ** 3
    B2(x) = 1 + 3 * (x - x2) + 3 * (x - x2) ** 2 - 3 * (x - x2) ** 3
    B3(x) = 1 + 3 * (x3 - x) + 3 * (x3 - x) ** 2 - 3 * (x3 - x) ** 3
    B4(x) = (x4 - x) ** 3

    # the y B-splines are identical

.fi

.sh
3.4.3. Derivatives

The derivatives are calculated by evaluating the derivatives of the
interpolating polynomial. The 0-th derivative is the value of the
interpolant. The 1-st derivatives are d/dx and d/dy. The second are
d2/dx2, d2/dy2 and d2/dxdy. The derivatives in the case II_BINEAREST
and II_BILINEAR are calculated directly.

.nf
case II_BINEAREST:

    der[1,1] = value				# see previous section

case II_BILINEAR:

    der[1] = value				# see previous section

    # d/dx
    der[2,1] = -ty * coeff[nx,ny] + ty * coeff[nx+1,ny] -
    	       sy * coeff[nx,ny+1] + sy * coeff[nx+1,ny+1]

    # d/dy
    der[1,2] = -tx * coeff[nx,ny] + sx * coeff[nx+1,ny] +
	       tx * coeff[nx,ny+1] + sx * coeff[nx+1,ny+1]

    # d2/dxdy
    der[2,2] = coeff[nx,ny] - coeff[nx+1,ny] - coeff[nx,ny+1] + coeff[nx+1,ny+1]

case II_BIPOLY3, II_BIPOLY5, II_BISPLINE3:
.fi

For the higher order interpolants the coefficients of the interpolating
polynomial in x and y are calculated. In the case of II_BIPOLY3 and II_BIPOLY5
this is the Everett polynomial of the 3-rd and 5-th order respectively.
In the case of II_BISPLINE3 the pp-representation is calculated for
the B-spline coefficients. The value of the interpolant and its
derivatives is calculated using nested multiplication.

.sh
3.4.5. Integration

Integration is most easily accomplished by integrating the interpolant in
x for each value of y. The resulting function of y can then be integrated
in y to derive the 2-D integral. The limits of the integral are assumed
to be the corners of a polygon. At minimum of three points which define
a triangular region in x-y are required. The integral will be an approximation.
A special function for integrating over a rectangular region is also
provided.

.sh
4. Detailed Design

.sh
4.1. Interpolant Descriptor

The interpolant parameters and coefficients will be stored in a structure
as listed below.

.nf
	define	LEN_MSISTRUCT		10

	struct {
	    
	    int msi_type		# interpolant type
	    int	msi_nxcoeff		# size of coefficient array in x
	    int	msi_nycoeff		# size of coefficient array in y
	    int	msi_fstpnt		# first datapoint in coefficient array
	    int	asi_coeff		# pointer to 1-D coefficient array

	} msistruct

.fi

.sh
4.2. Storage Requirements

The interpolant descriptor requires LEN_MSISTRUCT storage units.
The coefficient array storage is dynamically allocated and requires
msi_nxcoeff * msi_nycoeff real storage elements. The requirements for
each interpolant type are listed below.

.nf
	II_BINEAREST		nxpix * nypix
	II_BILINEAR		(nxpix + 1) * (nypix + 1)
	II_BIPOLY3		(nxpix + 3) * (nypix + 3)
	II_BIPOLY5		(nxpix + 5) * (nypix + 5)
	II_BISPLINE3		(nxpix + 3) * (nypix + 3)
.fi

.endhelp
