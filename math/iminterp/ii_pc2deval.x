# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# II_PCPOLY3 -- Procedure to evaluate the polynomial coefficients
# of third order in x and y using Everetts formuala.

procedure ii_pcpoly3 (coeff, index, len_coeff, pcoeff, len_pcoeff)

real	coeff[ARB]		# 1D array of interpolant coeffcients
int	index			# pointer into coeff array
int	len_coeff		# row length of coeffcients
real	pcoeff[len_pcoeff,ARB]	# polynomial coefficients
int	len_pcoeff		# row length of pcoeff

int	tptr
int	i, j
real	cd20, cd21, temp[4]

begin
	# determine polynomial coefficients in x
	tptr = index
	do i = 1, 4 {

	    # calculate the central differences
	    cd20 = 1./6. * (coeff[tptr+1] - 2. * coeff[tptr] + coeff[tptr-1])
	    cd21 = 1./6. * (coeff[tptr+2] - 2. * coeff[tptr+1] + coeff[tptr])

	    # calculate the polynomial coefficients in x at each y
	    pcoeff[1,i] = coeff[tptr]
	    pcoeff[2,i] = coeff[tptr+1] - coeff[tptr] - 2. * cd20 - cd21
	    pcoeff[3,i] = 3. * cd20
	    pcoeff[4,i] = cd21 - cd20

	    tptr = tptr + len_coeff
	}

	# calculate polynomial coefficients in y
	do j = 1, 4 {

	    # calculate the central differences
	    cd20 = 1./6. * (pcoeff[j,3] - 2. * pcoeff[j,2] + pcoeff[j,1])
	    cd21 = 1./6. * (pcoeff[j,4] - 2. * pcoeff[j,3] + pcoeff[j,2])

	    # calculate the final coefficients
	    temp[1] = pcoeff[j,2] 
	    temp[2] = pcoeff[j,3] - pcoeff[j,2] - 2. * cd20 - cd21
	    temp[3] = 3. * cd20
	    temp[4] = cd21 - cd20

	    do i = 1, 4
		pcoeff[j,i] = temp[i]
	}
end


# II_PCPOLY5 -- Procedure to evaluate the polynomial coefficients
# of fifth order in x and y using Everetts formuala.

procedure ii_pcpoly5 (coeff, index, len_coeff, pcoeff, len_pcoeff)

real	coeff[ARB]		# 1D array of interpolant coeffcients
int	index			# pointer into coeff array
int	len_coeff		# row length of coeffcients
real	pcoeff[len_pcoeff,ARB]	# polynomial coefficients
int	len_pcoeff		# row length of pcoeff array

int	tptr
int	i, j
real	cd20, cd21, cd40, cd41, temp[6]

begin
	# determine polynomial coefficients in x
	tptr = index
	do i = 1, 6 {

	    # calculate the central differences
	    cd20 = 1./6. * (coeff[tptr+1] - 2. * coeff[tptr] + coeff[tptr-1])
	    cd21 = 1./6. * (coeff[tptr+2] - 2. * coeff[tptr+1] + coeff[tptr])
	    cd40 = 1./120. * (coeff[tptr-2] - 4. * coeff[tptr-1] +
		   6. * coeff[tptr] - 4. * coeff[tptr+1] +
		   coeff[tptr+2])
	    cd41 = 1./120. * (coeff[tptr-1] - 4. * coeff[tptr] +
		   6. * coeff[tptr+1] - 4. * coeff[tptr+2] +
		   coeff[tptr+3])

	    # calculate coefficients in x for each y
	    pcoeff[1,i] = coeff[tptr]
	    pcoeff[2,i] = coeff[tptr+1] - coeff[tptr] - 2. * cd20 - cd21 +
			  6. * cd40 + 4. * cd41 
	    pcoeff[3,i] = 3. * cd20 - 5. * cd40
	    pcoeff[4,i] = cd21 - cd20 - 5. * (cd40 + cd41)
	    pcoeff[5,i] = 5. * cd40
	    pcoeff[6,i] = cd41 - cd40

	    tptr = tptr + len_coeff
	}

	# calculate polynomial coefficients in y
	do j = 1, 6 {

	    # calculate the central differences
	    cd20 = 1./6. * (pcoeff[j,4] - 2. * pcoeff[j,3] + pcoeff[j,2])
	    cd21 = 1./6. * (pcoeff[j,5] - 2. * pcoeff[j,4] + pcoeff[j,3])
	    cd40 = 1./120. * (pcoeff[j,1] - 4. * pcoeff[j,2] +
	    	   6. * pcoeff[j,3] - 4. * pcoeff[j,4] + pcoeff[j,5])
	    cd41 = 1./120. * (pcoeff[j,2] - 4. * pcoeff[j,3] +
		   6. * pcoeff[j,4] - 4. * pcoeff[j,5] + pcoeff[j,6])

	    # calculate the final coefficients
	    temp[1] = pcoeff[j,3]
	    temp[2] = pcoeff[j,4] - pcoeff[j,3] - 2. * cd20 - cd21 +
		      6. * cd40 + 4. * cd41
	    temp[3] = 3. * cd20 - 5. * cd40
	    temp[4] = cd21 - cd20 - 5. * (cd40 + cd41)
	    temp[5] = 5. * cd40
	    temp[6] = cd41 - cd40

	    do i = 1, 6
		pcoeff[j,i] = temp[i]

	}

end


# II_PCSPLINE3 -- Procedure to evaluate the polynomial coefficients
# of bicubic spline.

procedure ii_pcspline3 (coeff, index, len_coeff, pcoeff, len_pcoeff)

real	coeff[ARB]		# 1D array of interpolant coeffcients
int	index			# pointer into coeff array
int	len_coeff		# row length of coeffcients
real	pcoeff[len_pcoeff,ARB]	# polynomial coefficients
int	len_pcoeff		# row length of pcoeff

int	tptr
int	i, j
real	temp[4]

begin
	# determine polynomial coefficients in x
	tptr = index
	do i = 1, 4 {

	    pcoeff[1,i] = coeff[tptr+1] + 4. * coeff[tptr] + coeff[tptr-1]
	    pcoeff[2,i] = 3. * (coeff[tptr+1] - coeff[tptr-1])
	    pcoeff[3,i] = 3. * (coeff[tptr-1] - 2. * coeff[tptr] +
	    		  coeff[tptr+1])
	    pcoeff[4,i] = -coeff[tptr-1] + 3. * coeff[tptr] -
	    		  3. * coeff[tptr+1] + coeff[tptr+2]
			    
	    tptr = tptr + len_coeff
	}

	# calculate polynomial coefficients in y
	do j = 1, 4 {

	    temp[1] = pcoeff[j,3] + 4. * pcoeff[j,2] + pcoeff[j,1]
	    temp[2] = 3. * (pcoeff[j,3] - pcoeff[j,1]) 
	    temp[3] = 3. * (pcoeff[j,1] - 2. * pcoeff[j,2] + pcoeff[j,3])
	    temp[4] = -pcoeff[j,1] + 3. * pcoeff[j,2] - 3. * pcoeff[j,3] +
	    	      pcoeff[j,4]

	    do i = 1, 4
		pcoeff[j,i] = temp[i]
	}
end
