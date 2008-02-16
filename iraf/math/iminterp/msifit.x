# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSIFIT -- MSIFIT calculates the coefficients of the interpolant.
# With the exception of the bicubic spline interpolant the coefficients
# are stored as the data points.  The 2D B-spline coefficients are
# calculated using the routines II_SPLINE2D. MSIFIT checks that the
# dimensions of the data array are appropriate for the interpolant selected
# and allocates space for the coefficient array.
# Boundary extension is performed using boundary projection.

procedure msifit (msi, datain, nxpix, nypix, len_datain)

pointer	msi			# pointer to interpolant descriptor structure
real	datain[len_datain,ARB]	# data array
int	nxpix			# number of points in the x dimension
int	nypix			# number of points in the y dimension
int	len_datain			# row length of datain

int	i, j
pointer	fptr, nptr, rptr
pointer	tmp
pointer	rptrf[FNROWS]
pointer	rptrl[LNROWS]

errchk	calloc, mfree

begin
	# check the row length of datain
	if (len_datain < nxpix)
	    call error (0, "MSIFIT: Row length of datain too small.")

	# check that the number of data points in x and y is
	# appropriate for the interpolant type selected and
	# allocate space for the coefficient array allowing
	# sufficient storage for boundary extension

	switch (MSI_TYPE(msi)) {

	case II_BINEAREST:

	    if (nxpix < 1 || nypix < 1) {
		call error (0, "MSIFIT: Too few data points.")
		return
	    } else {
		MSI_NXCOEFF(msi) = nxpix
		MSI_NYCOEFF(msi) = nypix
		MSI_FSTPNT(msi) = 0
		if (MSI_COEFF(msi) != NULL)
		    call mfree (MSI_COEFF(msi), TY_REAL)
		call malloc (MSI_COEFF(msi), nxpix * nypix, TY_REAL)
	    }

	case II_BILINEAR, II_BIDRIZZLE:

	    if (nxpix < 2 || nypix < 2) {
		call error (0, "MSIFIT: Too few data points.")
		return
	    } else {
		MSI_NXCOEFF(msi) = nxpix + 1
		MSI_NYCOEFF(msi) = nypix + 1
		MSI_FSTPNT(msi) = 0
		if (MSI_COEFF(msi) != NULL)
		    call mfree (MSI_COEFF(msi), TY_REAL)
		call malloc (MSI_COEFF(msi),
			     MSI_NXCOEFF(msi) * MSI_NYCOEFF(msi), TY_REAL)
	    }

	case II_BIPOLY3:

	    if (nxpix < 4 || nypix < 4) {
		call error (0, "MSIFIT: Too few data points.")
		return
	    } else {
		MSI_NXCOEFF(msi) = nxpix + 3
		MSI_NYCOEFF(msi) = nypix + 3
		MSI_FSTPNT(msi) = MSI_NXCOEFF(msi) + 1
		if (MSI_COEFF(msi) != NULL)
		    call mfree (MSI_COEFF(msi), TY_REAL)
		call malloc (MSI_COEFF(msi),
			     MSI_NXCOEFF(msi) * MSI_NYCOEFF(msi), TY_REAL)
	    }

	case II_BIPOLY5:

	    if (nxpix < 6 || nypix < 6) {
		call error (0, "MSIFIT: Too few data points.")
		return
	    } else {
		MSI_NXCOEFF(msi) = nxpix + 5
		MSI_NYCOEFF(msi) = nypix + 5
		MSI_FSTPNT(msi) = 2 * MSI_NXCOEFF(msi) + 2
		if (MSI_COEFF(msi) != NULL)
		    call mfree (MSI_COEFF(msi), TY_REAL)
		call malloc (MSI_COEFF(msi),
			     MSI_NXCOEFF(msi) * MSI_NYCOEFF(msi), TY_REAL)
	    }

	case II_BISPLINE3:

	    if (nxpix < 4 || nypix < 4) {
		call error (0, "MSIFIT: Too few data points.")
		return
	    } else {
		MSI_NXCOEFF(msi) = nxpix + 3
		MSI_NYCOEFF(msi) = nypix + 3
		MSI_FSTPNT(msi) = MSI_NXCOEFF(msi) + 1
		if (MSI_COEFF(msi) != NULL)
		    call mfree (MSI_COEFF(msi), TY_REAL)
		call calloc (MSI_COEFF(msi),
			     MSI_NXCOEFF(msi) * MSI_NYCOEFF(msi), TY_REAL)
	    }

	case II_BISINC, II_BILSINC:

	    if (nxpix < 1 || nypix < 1) {
		call error (0, "MSIFIT: Too few data points.")
		return
	    } else {
		MSI_NXCOEFF(msi) = nxpix
		MSI_NYCOEFF(msi) = nypix
		MSI_FSTPNT(msi) = 0
		if (MSI_COEFF(msi) != NULL)
		    call mfree (MSI_COEFF(msi), TY_REAL)
		call calloc (MSI_COEFF(msi), nxpix * nypix, TY_REAL)
	    }

	}

	# index the coefficient pointer so that COEFF(fptr+1) points to the
	# first data point in the coefficient array
	fptr = MSI_COEFF(msi) - 1 + MSI_FSTPNT(msi)

	# load data into coefficient array
	rptr = fptr
	do j = 1, nypix {
	    call amovr (datain[1,j], COEFF(rptr+1), nxpix)
	    rptr = rptr + MSI_NXCOEFF(msi)
	}

	# calculate the coefficients of the interpolant
	# boundary extension is performed using boundary projection

	switch (MSI_TYPE(msi)) {

	case II_BINEAREST, II_BISINC, II_BILSINC:

	    # no end conditions necessary, coefficients stored as data

	case II_BILINEAR, II_BIDRIZZLE:

	    # extend the rows
	    rptr = fptr + nxpix
	    do j = 1, nypix {
		COEFF(rptr+1) = 2. * COEFF(rptr) - COEFF(rptr-1)
		rptr = rptr + MSI_NXCOEFF(msi)
	    }

	    # define the pointers to the last, 2nd last and third last rows
	    rptrl[1] = MSI_COEFF(msi) + (MSI_NYCOEFF(msi) - 1) *
	    	       MSI_NXCOEFF(msi)
	    do i = 2, 3
		rptrl[i] = rptrl[i-1] - MSI_NXCOEFF(msi)

	    # define the last row by extending the columns
	    call awsur (COEFF(rptrl[2]), COEFF(rptrl[3]), COEFF(rptrl[1]),
	    		MSI_NXCOEFF(msi), 2., -1.)
	    
	case II_BIPOLY3:

	    # extend the rows
	    rptr = fptr
	    nptr = fptr + nxpix
	    do j = 1, nypix {
		COEFF(rptr) = 2. * COEFF(rptr+1) - COEFF(rptr+2)
		COEFF(nptr+1) = 2. * COEFF(nptr) - COEFF(nptr-1)
		COEFF(nptr+2) = 2. * COEFF(nptr) - COEFF(nptr-2)
		rptr = rptr + MSI_NXCOEFF(msi)
		nptr = nptr + MSI_NXCOEFF(msi)
	    }

	    # define pointers to first, second and third rows
	    rptrf[1] = MSI_COEFF(msi) 
	    do i = 2, 3
		rptrf[i] = rptrf[i-1] + MSI_NXCOEFF(msi)

	    # extend the columns, first row
	    call awsur (COEFF(rptrf[2]), COEFF(rptrf[3]), COEFF(rptrf[1]),
	    		MSI_NXCOEFF(msi), 2., -1.)
	    
	    # define the pointers to the last to fifth last rows
	    rptrl[1] = MSI_COEFF(msi) + (MSI_NYCOEFF(msi) - 1) *
	    	       MSI_NXCOEFF(msi) 
	    do i = 2, 5
		rptrl[i] = rptrl[i-1] - MSI_NXCOEFF(msi)

	    # extend the columns, define 2nd last row
	    call awsur (COEFF(rptrl[3]), COEFF(rptrl[4]), COEFF(rptrl[2]),
	    		MSI_NXCOEFF(msi), 2., -1.)
	    
	    # extend the columns, define last row
	    call awsur (COEFF(rptrl[3]), COEFF(rptrl[5]), COEFF(rptrl[1]),
	    		MSI_NXCOEFF(msi), 2., -1.)
	    
	case II_BIPOLY5:

	    # extend the rows
	    rptr = fptr
	    nptr = fptr + nxpix
	    do j = 1, nypix {
		COEFF(rptr-1) = 2. * COEFF(rptr+1) - COEFF(rptr+3)
		COEFF(rptr) = 2. * COEFF(rptr+1) - COEFF(rptr+2)
		COEFF(nptr+1) = 2. * COEFF(nptr) - COEFF(nptr-1)
		COEFF(nptr+2) = 2. * COEFF(nptr) - COEFF(nptr-2)
		COEFF(nptr+3) = 2. * COEFF(nptr) - COEFF(nptr-3)
		rptr = rptr + MSI_NXCOEFF(msi)
		nptr = nptr + MSI_NXCOEFF(msi)
	    }

	    # define pointers to first five rows
	    rptrf[1] = MSI_COEFF(msi) 
	    do i = 2, 5
		rptrf[i] = rptrf[i-1] + MSI_NXCOEFF(msi)

	    # extend the columns, define first row
	    call awsur (COEFF(rptrf[3]), COEFF(rptrf[5]), COEFF(rptrf[1]),
	    		MSI_NXCOEFF(msi), 2., -1.)
	    
	    # extend the columns, define second row
	    call awsur (COEFF(rptrf[3]), COEFF(rptrf[4]), COEFF(rptrf[2]),
	    		MSI_NXCOEFF(msi), 2., -1.)
	    
	    # define pointers last seven rows
	    rptrl[1] = MSI_COEFF(msi) + (MSI_NYCOEFF(msi) - 1) *
	    	       MSI_NXCOEFF(msi)
	    do i = 2, 7
		rptrl[i] = rptrl[i-1] - MSI_NXCOEFF(msi) 

	    # extend the columns, last row
	    call awsur (COEFF(rptrl[4]), COEFF(rptrl[7]), COEFF(rptrl[1]),
	    		MSI_NXCOEFF(msi), 2., -1.)

	    # extend the columns, 2nd last row
	    call awsur (COEFF(rptrl[4]), COEFF(rptrl[6]), COEFF(rptrl[2]),
	    		MSI_NXCOEFF(msi), 2., -1.)

	    # extend the columns, 3rd last row
	    call awsur (COEFF(rptrl[4]), COEFF(rptrl[5]), COEFF(rptrl[3]),
	    		MSI_NXCOEFF(msi), 2., -1.)

	case II_BISPLINE3:
		
	    # allocate space for a temporary work arrays
	    call calloc (tmp, MSI_NXCOEFF(msi) * MSI_NYCOEFF(msi), TY_REAL)

	    # the B-spline coefficients are calculated using the
	    # natural end conditions, end coefficents are set to
	    # zero

	    # calculate the univariate B_spline coefficients in x
	    call ii_spline2d (COEFF(MSI_COEFF(msi)), TEMP(tmp),
	    	nxpix, MSI_NYCOEFF(msi), MSI_NXCOEFF(msi), MSI_NYCOEFF(msi))


	    # calculate the univariate B-spline coefficients in y to
	    # results of x interpolation
	    call ii_spline2d (TEMP(tmp), COEFF(MSI_COEFF(msi)),
	        nypix, MSI_NXCOEFF(msi), MSI_NYCOEFF(msi), MSI_NXCOEFF(msi))

	    # deallocate storage for temporary arrays
	    call mfree (tmp, TY_REAL)
	}
end
