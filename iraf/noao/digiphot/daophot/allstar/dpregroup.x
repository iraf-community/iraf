# DP_ALSORT -- Sort the stars on the y coordinate.

procedure dp_alsort (id, x, y, mag, sky, sumwt, dxold, dyold, xclamp, yclamp,
	nstar)

int	id[ARB]			# array if star ids
real	x[ARB]			# array of star x values
real	y[ARB]			# array of y values
real	mag[ARB]		# array of star magnitudes
real	sky[ARB]		# array of star sky values
real	sumwt[ARB]		# array of weighted sums
real	dxold[ARB]		# the previous star x array
real	dyold[ARB]		# the previous star y array
real	xclamp[ARB]		# the x array clamps
real	yclamp[ARB]		# the y array clamps
int	nstar			# the number of stars

int	ier
pointer	sp, index

begin
	# Allocate some working memory.
	call smark (sp)
	call salloc (index, nstar, TY_INT)

	# Sort the star list on y.
	call quick (y, nstar, Memi[index], ier)

	# Recitfy the remaining arrays.
	call dp_irectify (id, Memi[index], nstar)
	call dp_rectify (x, Memi[index], nstar)
	call dp_rectify (mag, Memi[index], nstar)
	call dp_rectify (sky, Memi[index], nstar)
	call dp_rectify (sumwt, Memi[index], nstar)
	call dp_rectify (dxold, Memi[index], nstar)
	call dp_rectify (dyold, Memi[index], nstar)
	call dp_rectify (xclamp, Memi[index], nstar)
	call dp_rectify (yclamp, Memi[index], nstar)

	# Release memory.
	call sfree (sp)
end


# DP_REGROUP -- Group stars into physical associations based on proximity.

procedure dp_regroup (id, x, y, mag, sky, chi, dxold, dyold, xclamp, yclamp,
	nstar, radius, last)

int	id[ARB]			# array if star ids
real	x[ARB]			# array of star x values
real	y[ARB]			# array of y values
real	mag[ARB]		# array of star magnitudes
real	sky[ARB]		# array of star sky values
real	chi[ARB]		# array of chis
real	dxold[ARB]		# the previous star x array
real	dyold[ARB]		# the previous star y array
real	xclamp[ARB]		# the x array clamps
real	yclamp[ARB]		# the y array clamps
int	nstar			# the number of stars
real	radius			# the fitting radius
int	last[ARB]		# the last array (NO or YES for last star)

int	itop, itest, j, k, i, ier
pointer sp, index
real	critrad, critradsq, xtest, ytest, dx, dy

begin
	# If there is only one stars its value of last is YES.
	if (nstar <= 1) {
	    last[1] = YES
	    return
	}

	# Allocate some working memory.
	call smark (sp)
	call salloc (index, nstar, TY_INT)

	# Compute the critical radius.
	critrad = 2.0 * radius
	critradsq = critrad * critrad

	# Sort the star list on y and rectify the accompanying x array.
	call quick (y, nstar, Memi[index], ier)
	call dp_rectify (x, Memi[index], nstar)

	# At this point the stars are in a stack NSTAR stars long. The
	# variable ITEST will point to the position in the stack
	# occupied by the star which is currently the center of a
	# circle of the CRITRAD pixels, within which we are 
	# looking for other stars. ITEST starts out with a value of one.
	# ITOP points to the top position in the stack of the 
	# stars which have not yet been assigned to groups. ITOP starts
	# out with the value of 2. Each time through, the program goes 
	# down through the stack from ITOP and looks for stars within 
	# the critrad pixels from the star at stack position ITEST.
	# When such a star is found, it changes places in the 
	# stack with the star at ITOP and ITOP is incremented by one.
	# When the search reaches a star J such that Y(J) - Y(ITEST)
	# > CRITRAD we know that no further stars will be found 
	# within the CRITRAD pixels, the pointer ITEST is incremented
	# by one, and the search proceeds again from the new
	# value of ITOP. If the pointer ITEST catches up with the
	# pointer ITOP, then the current group is complete
	# with the star at the position ITEST-1, and
	# ITOP is incremented by one. If ITOP catches up with NSTAR
	# the grouping process is complete.

	itop = 2
	do itest = 1, nstar {

	    # Initialize the last array.
	    last[itest] = NO

	    # ITEST has reached ITOP; so no other unassigned stars are
	    # within CRITRADIUS pixels of any member of the current
	    # group. The group is therefore complete. Signify this by
	    # setting LAST[ITEST-1] = YES, and incrementing the value of
	    # ITOP by one.  If ITOP is greater than NSTAR at this point
	    # list is finished.

	    if (itest == itop) {
		j = itest - 1
		if (j > 0)
		    last[j] = YES
		itop = itop + 1
		if (itop > nstar) {
		    last[itest] = YES
		 	break
		 }
	    }

	    # Now go through the list of unassigned stars, occupying
	    # positions ITOP through NSTAR in the stack, to look for
	    # stars within CRITRADIUS pixels of the star at 
	    # position ITEST inthe stack. If one is found, move it
 	    # up to stack position ITOP and increment ITOP by one.

	    xtest = x[itest]
	    ytest = y[itest]
	    j = itop

	    do i = j, nstar {

		# Check the distance between the stars.
		dy = y[i] - ytest
		if (dy > critrad)
		    break
		dx = x[i] - xtest
		if (abs (dx) > critrad)
		    next
		if ((dx * dx + dy * dy) > critradsq)
		    next

		# This star is within CRITRAD pixels of the
		# star at stack position ITEST. Therefore it is
		# added to the current group by moving it up to position
		# ITOP in the stack, where the pointer ITEST may reach it.

		call dp_rgswap (itop, i, x, y, Memi[index])

		# Now increment ITOP by 1 to point at the top most 
		# unassigned star in the stack.
		itop = itop + 1
		if (itop > nstar) {
		    do k = itest, nstar - 1
			last[k] = NO
		    last[nstar] = YES
		    break
		}
	    }

	    if (itop > nstar)
		break
	}

	# Reorder the remaining arrays to match the x and y arrays.
	call dp_irectify (id, Memi[index], nstar)
   	call dp_rectify (mag, Memi[index], nstar)
	call dp_rectify (sky, Memi[index], nstar)
	call dp_rectify (chi, Memi[index], nstar)
	call dp_rectify (dxold, Memi[index], nstar)
	call dp_rectify (dyold, Memi[index], nstar)
	call dp_rectify (xclamp, Memi[index], nstar)
	call dp_rectify (yclamp, Memi[index], nstar)

	call sfree (sp)
end


# DP_RGSWAP -- Swap the i-th and j-th stars in the stack without otherwise
# altering the order of the stars..

procedure dp_rgswap (i, j, x, y, index)

int	i, j		# indices of the two stars to be swapped
real	x[ARB]		# the array of x values
real	y[ARB]		# the array of y values
int	index[ARB]	# the index array

int	ihold, k,l
real	xhold, yhold

begin
	xhold = x[j]
	yhold = y[j]
	ihold = index[j]

	do k = j, i + 1, -1 {
	    l = k - 1
 	    x[k] = x[l]
	    y[k] = y[l]
	    index[k] = index[l]
	}

	x[i] = xhold
	y[i] = yhold
	index[i] = ihold
end
