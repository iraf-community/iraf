#* HISTORY *
#* B.Simon	24-Aug-94	Original
#* B.Simon	18-Sep-00	Revised search termination criterion

# GETMATCH -- Find rows in second table witch match rows in first

procedure getmatch (in1, in2, ncol, col1, col2, weight, nrow1, index1, 
		    nrow2, index2, maxnorm, sphere, closest, dist)

pointer	in1		# i: first table descriptor
pointer	in2		# i: second table descriptor
int	ncol		# i: number of match columns
pointer	col1[ARB]	# i: match columns in first table
pointer	col2[ARB]	# i: match columns in second table
double	weight[ARB]	# i: weights used in computing norm
int	nrow1		# i: number of rows in first table
int	index1[ARB]	# i: sorted row indices for first table
int	nrow2		# i: number of rows in second table
int	index2[ARB]	# i: sorted row indices for second table
double	maxnorm		# i: maximum norm used in match
bool	sphere		# i: apply spherical correction to first column?
int	closest[ARB]	# o: closest match in second table to first
double	dist[ARB]	# o: distance between matched rows
#--
double	sqnorm,  proj, abnorm, norm, minabnorm, minnorm
int	idx, jdx, irow, jrow, krow, jlast

begin
	jlast = 1
	sqnorm = maxnorm * maxnorm

	# Find the row in the second table which minimizes the norm for
	# each row of the first table


	do idx = 1, nrow1 {
	    irow = index1[idx]
	    jrow = index2[jlast]

	    # The initial guess is the row which matched last time

	    call getnorm (in1, in2, ncol, col1, col2, irow, jrow, 
			  weight, sphere, proj, abnorm, norm)

	    minabnorm = abnorm
	    minnorm = norm
	    krow = jrow

	    # Search backwards for a row which minimizes the norm
	    # Terminate the search when the first dimension of the norm (proj)
	    # is greater than the minimum norm, as all subsequent rows have
	    # norms that must be greater than the minimum

	    do jdx = jlast-1, 1, -1 {
		jrow = index2[jdx]

		call getnorm (in1, in2, ncol, col1, col2, irow, jrow,
			      weight, sphere, proj, abnorm, norm)

		if (proj > minabnorm)
		    break

		if (norm < minnorm) {
		    minabnorm = abnorm
		    minnorm = norm
		    krow = jrow
		}
	    }

	    # Search forwards for a row that minimizes the norm
	    # Use the same termination condition as for the forwards search

	    do jdx = jlast+1, nrow2 {
		jrow = index2[jdx]

		call getnorm (in1, in2, ncol, col1, col2, irow, jrow,
			      weight, sphere, proj, abnorm, norm)

		if (proj > minabnorm)
		    break

		if (norm < minnorm) {
		    minabnorm = abnorm
		    minnorm = norm
		    krow = jrow
		}
	    }

	    if (minnorm > sqnorm) {
		dist[irow] = maxnorm
		closest[irow] = 0
	    } else {
		dist[irow] = sqrt (minnorm)
		closest[irow] = krow
	    }

	    jlast = krow
	}

end

